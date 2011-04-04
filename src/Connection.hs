{-# LANGUAGE ScopedTypeVariables #-}

module Connection
    (
      -- * Connection
      connect
    , sendCmd
    , sendMsg
    , getMsg

      -- * Settings
    , stdReconnect

      -- * Data types
    , ConnectionCommand (..)
    , module Connection.Managed
    , module Network.IRC
    ) where


import Prelude hiding (catch)

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Concurrent
import Network
import Network.IRC
import System.IO

import Connection.Managed
import Logging
import Hirc

-- | Standard reconnect settings with 1 hours delay between retries
stdReconnect :: Int -> Reconnect
stdReconnect t = Reconnect t 0 (60 * 60 * 1) Nothing

-- | Connect to a IRC server, returns two functions. The first one will return
-- incoming messages, the second will handle commands
connect :: UserName     -- ^ nick
        -> UserName     -- ^ user
        -> UserName     -- ^ realname
        -> Hirc ()
connect nick' user' realname = do

  srv <- asks server
  h <- liftIO $
    connectTo (host srv) (PortNumber (port srv)) `catch` \(_ :: IOException) -> do
    error $ "Connection to \"" ++ host srv ++ ":" ++ show (port srv) ++ "\" failed."
  liftIO $ do
    hSetBuffering h LineBuffering
    hSetBinaryMode h False
  modifyM_ $ \s -> s { connectedHandle = Just h }

  _ <- forkM listenForMessages
  _ <- forkM receiveCommand

  sendCmd $ Send (nick nick')
  sendCmd $ Send (user user' "*" "*" realname)
  modifyM_ $ \s -> s
    { ircNickname = nick'
    , ircUsername = user'
    , ircRealname = realname
    }

  waitFor001

  return ()
 where
  -- Wait for the 001 message before "giving away" our connection
  waitFor001 :: Hirc ()
  waitFor001 = do
    msg <- getMsg
    logM 3 $ "waitFor001 --- " ++ show msg
    case msg of
         Message { msg_command = "001" } -> return ()
         _                               -> waitFor001

-- Sending/receiving commands/messages

sendCmd :: ConnectionCommand -> Hirc ()
sendCmd c = do
  cmd <- asks cmdChan
  liftIO $ writeChan cmd c

getMsg :: Hirc Message
getMsg = do
  msg <- asks msgChan
  liftIO $ readChan msg

sendMsg :: Message -> Hirc ()
sendMsg m = requireHandle $ \h -> liftIO . hPutStrLn h $ encode m

-- Wait for commands, execute them
receiveCommand :: Hirc ()
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

notice :: To -> String -> Message
notice t s = mkMessage "NOTICE" [t,s]

-- Listen on handle and put incoming messages to our Chan
listenForMessages :: Hirc ()
listenForMessages = forever $ do
  l <- safeGetLine
  logM 3 $ "listenForMessage --- " ++ show l
  case join $ fmap decode l of
       Just m  -> do
         msg <- asks msgChan
         liftIO $ writeChan msg m
       Nothing -> return () -- todo

safeGetLine :: Hirc (Maybe String)
safeGetLine = requireHandle $ \h ->
  either (\(_::SomeException) -> Nothing) Just <$>
    liftIO (try $ hGetLine h)

--
-- Other
--

requireHandle :: (Handle -> Hirc a)
              -> Hirc a
requireHandle hirc = do
  mh <- gets connectedHandle
  case mh of
       Just h  -> hirc h
       Nothing -> throwError H_NotConnected
