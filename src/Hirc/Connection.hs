{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hirc.Connection
    (
      -- * Connection
      connect
    , sendCmd
    , sendMsg
    , getMsg

      -- * Settings
    , stdReconnect

      -- * Data types
    , module Hirc.Connection.Managed
    , module Network.IRC
    ) where


import Prelude

--import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
--import Data.Foldable
--import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network
import Network.IRC
import System.IO

import Hirc.Types
import Hirc.Connection.Managed
import Hirc.Logging

-- | Standard reconnect settings with 1 hours delay between retries
stdReconnect :: Int -> Reconnect
stdReconnect t = Reconnect t 0 (60 * 60 * 1) Nothing

-- | Connect to a IRC server, returns two functions. The first one will return
-- incoming messages, the second will handle commands
connect :: String     -- ^ nick
        -> String     -- ^ user
        -> String     -- ^ realname
        -> HircM ()
connect nick' user' rn = do

  hrc <- ask
  let srv = server (runningHirc hrc)

  -- try to connect => catch exception if necessary
  eh <- liftIO $
    (Right `fmap` connectTo (host srv) (PortNumber (port srv))) `catchError` (return . Left)

  case eh of

    -- error during connection => rethrow error
    Left e -> do
      logM 1 $ "Connection to \"" ++ host srv ++ ":" ++ show (port srv) ++ "\" failed: " ++ show e
      throwError H_ConnectionFailed

    -- everything ok => store handle
    Right h -> do

      liftIO $ do

        -- fix handle settings
        hSetBuffering h LineBuffering
        hSetBinaryMode h False
        hSetEncoding h utf8

        -- start workerthreads
        listenTID <- forkIO $ listenForMessages hrc h
        cmdTID    <- forkIO $ handleCommands    hrc h

        -- store handle + thread IDs
        atomically $ do
          writeTVar (networkHandle  hrc) (Just h)
          writeTVar (listenThreadId hrc) (Just listenTID)
          writeTVar (cmdThreadId    hrc) (Just cmdTID)

      -- send user information to server
      sendCmd $ Send (nick (B8.pack nick'))
      sendCmd $ Send (user (B8.pack user') "*" "*" (B8.pack rn))

      -- store user information
      liftIO $ atomically $ do
        writeTVar (nickname $ runningHirc hrc) nick'
        writeTVar (username $ runningHirc hrc) user'
        writeTVar (realname $ runningHirc hrc) rn

      -- wait for OK from IRC server
      waitFor001

 where
  -- Wait for the 001 message before "giving away" our connection
  waitFor001 :: HircM ()
  waitFor001 = do
    msg <- getMsg
    logM 3 $ "waitFor001 --- " ++ show msg
    case msg of
         Message { msg_command = "001" } -> return ()
         _                               -> waitFor001

-- Sending/receiving commands/messages

sendCmd :: ConnectionCommand -> HircM ()
sendCmd c = do
  cmd <- asks cmdChan
  liftIO $ writeChan cmd c

getMsg :: HircM Message
getMsg = do
  msg <- asks msgChan
  liftIO $ readChan msg

sendMsg :: Handle -> Message -> IO ()
sendMsg h m = B8.hPutStrLn h $ encode m

-- Wait for commands, execute them
handleCommands :: HircSettings -> Handle -> IO ()
handleCommands hrc h = forever $ do

  c <- readChan (cmdChan hrc) `catch` (\BlockedIndefinitelyOnMVar -> shutdown)
  case c of

       Send msg           -> sendMsg h $ msg
       PrivMsg to msg     -> sendMsg h $ privmsg (B8.pack to) (T.encodeUtf8 msg)
       Notice  to msg     -> sendMsg h $ notice  to msg
       Join chan          -> sendMsg h $ joinChan chan
       Part chan          -> sendMsg h $ part chan
       Nick new           -> sendMsg h $ nick (B8.pack new)

       Ping               -> sendMsg h $ Message Nothing "PING" []
       Pong               -> sendMsg h $ Message Nothing "PONG" []

       Quit msg           ->
         sendMsg h (quit (T.encodeUtf8 <$> msg)) `finally` shutdown

 where
  shutdown = do

    -- close handle
    hClose h

    -- lookup current thread IDs and reset state to Nothing
    mlistenerTID <- liftIO $ atomically $ do
      writeTVar (networkHandle hrc)  Nothing
      writeTVar (cmdThreadId hrc)    Nothing
      swapTVar  (listenThreadId hrc) Nothing

    -- kill worker threads
    case mlistenerTID of
      Just listenerTID -> killThread listenerTID
      Nothing          -> return ()

    -- kill own thread
    throw ThreadKilled

mkMessage :: String -> [Parameter] -> Message
mkMessage cmd params = Message Nothing (B8.pack cmd) params

notice :: Nickname -> Text -> Message
notice n t = mkMessage "NOTICE" [B8.pack n, T.encodeUtf8 t]

-- Listen on handle and put incoming messages to our Chan
listenForMessages :: HircSettings -> Handle -> IO ()
listenForMessages hrc h = forever $ do

  bs <- BS.hGetLine h
  case decode bs of

    -- successful message decode
    Just m -> do
      liftIO $ writeChan (msgChan hrc) m

    -- fallback TODO
    _ -> return ()

{-
safeGetLine :: HircM (Maybe ByteString)
safeGetLine = requireHandle $ \h ->

  liftIO $ foldr
    (\enc b -> tryEncoding enc h `catch` (\(_ :: SomeException) -> b))
    (return Nothing)
    [utf8, latin1, utf16, utf32]

 where
  tryEncoding encoding handle = do

    let getEncodedLine = do
          hSetEncoding handle encoding
          hGetLine h
        resetEncoding = do
          hSetEncoding handle utf8

    Just <$> getEncodedLine `finally` resetEncoding
-}

  {-
  catch (liftIO $ hGetLine h) $ \(e :: SomeException) -> do
    logM 1 $ "safeGetLine, exception: " ++ show e
    tryEncoding [latin1, utf16, utf32]

tryEncoding :: [TextEncoding] -> HircM (Maybe String)
tryEncoding (enc:r) = requireHandle $ \h -> do

  -- IO block to attempt decoding
  let io = finally (do hSetEncoding h enc
                       hGetLine h)
                   (hSetEncoding h utf8)

  -- run IO and catch exceptions
  eline <- liftIO $ (Right `fmap` io) `catch` (return . Left)
  case eline of
    Right line -> do
      -- show a message on successful decode
      logM 1 $ "tryEncoding, successfully used encoding: \"" ++ show enc ++ "\""
      return $ Just line
    Left err -> do
      logM 1 $ "tryEncoding, exception for \"" ++ show enc ++ "\": " ++ show e
      tryEncoding r
tryEncoding [] = requireHandle $ \h -> do
  logM 1 $ "tryEncoding failed, reset handle encoding to: \"" ++ show utf8 ++ "\""
  liftIO $ do
    hSetEncoding h utf8
    hGetLine h
-}

--
-- Other
--

{-
requireHandle :: (Handle -> HircM a)
              -> HircM a
requireHandle hirc = do

  -- look up TVar value
  nhtvar <- asks networkHandle
  mh <- liftIO $ atomically $ readTVar nhtvar

  case mh of
       Just h  -> hirc h
       Nothing -> throwError H_NotConnected
-}
