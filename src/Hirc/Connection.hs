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

  -- make sure we close previous connections
  liftIO $ shutdown hrc

  -- try to connect to server
  let srv = server (runningHirc hrc)
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
        cmdTID    <- forkIO $ handleCommands    hrc

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

-- | Shutdown all of hirc, including current thread
shutdownSelf :: HircSettings -> IO a
shutdownSelf hrc = shutdown hrc >> throw ThreadKilled

-- | Shutdown all of hirc, but do not kill own thread
shutdown :: HircSettings -> IO ()
shutdown hrc = do

  -- close handle
  mh <- liftIO $ atomically $ swapTVar (networkHandle hrc) Nothing
  case mh of
    Just h  -> hClose h
    Nothing -> return ()

  -- lookup current thread IDs and reset state to Nothing
  tids <- liftIO $ atomically $ do
    c <- swapTVar (cmdThreadId hrc)    Nothing
    l <- swapTVar (listenThreadId hrc) Nothing
    return [ tid | Just tid <- [c,l]]

  -- kill other worker threads (not own!)
  myTID <- myThreadId
  mapM_ (\tid -> unless (tid == myTID) (killThread tid)) tids



-------------------------------------------------------------------------------
-- Worker Threads

-- Wait for commands, execute them
handleCommands :: HircSettings -> IO ()
handleCommands hrc = forever $ do

  c <- readChan (cmdChan hrc) `catch` (\BlockedIndefinitelyOnMVar -> shutdownSelf hrc)
  case c of

       Send msg           -> sendMsg hrc $ msg
       PrivMsg to msg     -> sendMsg hrc $ privmsg (B8.pack to) (T.encodeUtf8 msg)
       Notice  to msg     -> sendMsg hrc $ notice  to msg
       Join chan          -> sendMsg hrc $ joinChan chan
       Part chan          -> sendMsg hrc $ part chan
       Nick new           -> sendMsg hrc $ nick (B8.pack new)

       Ping               -> sendMsg hrc $ Message Nothing "PING" []
       Pong               -> sendMsg hrc $ Message Nothing "PONG" []

       Quit msg           ->
         sendMsg hrc (quit (T.encodeUtf8 <$> msg)) `finally` shutdownSelf hrc

-- Listen on handle and put incoming messages to our Chan
listenForMessages :: HircSettings -> Handle -> IO ()
listenForMessages hrc h = forever $ do

  bs <- BS.hGetLine h
  case decode bs of

    -- successful message decode
    Just m -> do
      liftIO $ writeChan (msgChan hrc) m

    -- fallback TODO
    _ -> logH 1 $ "Could not decode bytestring: " ++ B8.unpack bs

 where
  logH i m = do
    writeChan (logChanH hrc) (i,m)


-------------------------------------------------------------------------------
-- Helper functions

-- Sending/receiving commands/messages

sendCmd :: ConnectionCommand -> HircM ()
sendCmd c = do
  cmd <- asks cmdChan
  liftIO $ writeChan cmd c

getMsg :: HircM Message
getMsg = do
  msg <- asks msgChan
  liftIO $ readChan msg

-- | Send a message in current Hirc instance
sendMsg :: HircSettings -> Message -> IO ()
sendMsg hrc m = do

  -- get (and wait for, if not available) handle
  h <- atomically $ do
    mh <- readTVar (networkHandle hrc)
    case mh of
      Just h -> return h
      Nothing -> retry

  -- send encoded message
  B8.hPutStrLn h $ encode m

mkMessage :: String -> [Parameter] -> Message
mkMessage cmd params = Message Nothing (B8.pack cmd) params

notice :: Nickname -> Text -> Message
notice n t = mkMessage "NOTICE" [B8.pack n, T.encodeUtf8 t]
