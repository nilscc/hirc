{-# LANGUAGE ScopedTypeVariables #-}

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


import Prelude hiding (catch)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MState
import Control.Exception.Peel
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
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
connect :: UserName     -- ^ nick
        -> UserName     -- ^ user
        -> UserName     -- ^ realname
        -> HircM ()
connect nick' user' rn = do

  srv <- asks $ server . runningHirc
  h <- liftIO $
    connectTo (host srv) (PortNumber (port srv)) `catch` \(_ :: IOException) -> do
    error $ "Connection to \"" ++ host srv ++ ":" ++ show (port srv) ++ "\" failed."
  liftIO $ do
    hSetBuffering h LineBuffering
    hSetBinaryMode h False
    hSetEncoding h utf8
  modifyM_ $ \s -> s { connectedHandle = Just h }

  _ <- forkM listenForMessages
  _ <- forkM receiveCommand

  sendCmd $ Send (nick nick')
  sendCmd $ Send (user user' "*" "*" rn)
  modifyM_ $ \s -> s
    { ircNickname = nick'
    , ircUsername = user'
    , ircRealname = rn
    }

  waitFor001

  return ()
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

sendMsg :: Message -> HircM ()
sendMsg m = requireHandle $ \h -> liftIO . hPutStrLn h $ encode m

-- Wait for commands, execute them
receiveCommand :: HircM ()
receiveCommand = forever . requireHandle $ \h -> do

  cmd <- asks cmdChan
  c   <- liftIO $ readChan cmd
  logM 3 $ "receiveCommand --- " ++ show c
  case c of

       Send msg           -> sendMsg $ msg
       PrivMsg to msg     -> sendMsg $ privmsg to msg
       Notice to msg      -> sendMsg $ notice to msg
       Join chan          -> sendMsg $ joinChan chan
       Part chan          -> sendMsg $ part chan

       Ping               -> sendMsg $ Message Nothing "PING" []
       Pong               -> sendMsg $ Message Nothing "PONG" []

       Quit msg           -> do
         -- shutdown everything
         sendMsg $ quit msg
         liftIO $ hClose h
         modifyM_ $ \s -> s { connectedHandle = Nothing }
         -- error will (hopefully) get cought
         throwError H_ConnectionLost

mkMessage :: String -> [Parameter] -> Message
mkMessage cmd params = Message Nothing cmd params

notice :: Nickname -> String -> Message
notice t s = mkMessage "NOTICE" [t,s]

-- Listen on handle and put incoming messages to our Chan
listenForMessages :: HircM ()
listenForMessages = forever $ do
  l <- safeGetLine
  logM 3 $ "listenForMessage --- " ++ show l
  case decode l of
       Just m  -> do
         msg <- asks msgChan
         liftIO $ writeChan msg m
       Nothing -> return () -- todo

safeGetLine :: HircM String
safeGetLine = requireHandle $ \h ->
  catch (liftIO $ hGetLine h) $ \(e :: SomeException) -> do
    logM 1 $ "safeGetLine, exception: " ++ show e
    tryEncoding [latin1, utf16, utf32]

tryEncoding :: [TextEncoding] -> HircM String
tryEncoding (enc:r) = requireHandle $ \h ->
  let io = finally (do hSetEncoding h enc
                       hGetLine h)
                   (hSetEncoding h utf8)
      logEnc = logM 1 $ "tryEncoding, successfully used encoding: \"" ++ show enc ++ "\""
   in catch (liftIO io <* logEnc) $ \(e :: SomeException) -> do
        logM 1 $ "tryEncoding, exception for \"" ++ show enc ++ "\": " ++ show e
        tryEncoding r
tryEncoding [] = requireHandle $ \h -> do
  logM 1 $ "tryEncoding failed, reset handle encoding to: \"" ++ show utf8 ++ "\""
  liftIO $ do
    hSetEncoding h utf8
    hGetLine h

--
-- Other
--

requireHandle :: (Handle -> HircM a)
              -> HircM a
requireHandle hirc = do
  mh <- gets connectedHandle
  case mh of
       Just h  -> hirc h
       Nothing -> throwError H_NotConnected
