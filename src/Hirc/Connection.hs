{-# LANGUAGE OverloadedStrings #-}

module Hirc.Connection
    (
      -- * Connection
      connect
    , sendCmd
    , getMsg

      -- * Settings
    , stdReconnect

      -- * Data types
    , module Hirc.Connection.Managed
    , module Network.IRC
    ) where

import Control.Exception
    ( IOException,
      finally,
      handle,
      throw,
      AsyncException(ThreadKilled) )
import Control.Concurrent
    ( newChan, readChan, writeChan, forkIO, killThread, myThreadId )
import Control.Concurrent.STM
    ( atomically,
      retry,
      STM,
      TVar,
      newTVarIO,
      readTVar,
      writeTVar,
      swapTVar )
import Control.Monad ( forever, unless )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Function ( fix )
import Network.Connection (initConnectionContext, connectTo, ConnectionParams (ConnectionParams), connectionClose, connectionGetLine, connectionPut)
import Network.IRC

import Hirc.Types.Connection
    ( ConnectionCommand(..),
      Nickname,
      Reconnect(Reconnect),
      IrcServer(port, host) )
import Hirc.Types.Hirc
    ( LogInstance,
      IrcInstance(IrcInstance, msgChan, cmdThreadId, listenThreadId,
                  networkConnection, cmdChan),
      IrcDefinition(IrcDefinition) )
import Hirc.Connection.Managed
import Hirc.Logging ( logMaybeIO )

requireTVar :: TVar (Maybe a) -> STM a
requireTVar v = readTVar v >>= maybe retry return

-- | Standard reconnect settings with 1 hours delay between retries
stdReconnect :: Int -> Reconnect
stdReconnect t = Reconnect t 0 (60 * 60 * 1) Nothing

-- | Connect to a IRC server, returns two functions. The first one will return
-- incoming messages, the second will handle commands
connect :: IrcDefinition -> Maybe LogInstance -> IO (Either IOException IrcInstance)
connect def@(IrcDefinition srv chans nick' user' rn) mlog = handle (return . Left) $ do

  -- try to connect to server
  ctxt <- initConnectionContext
  h <- connectTo ctxt $ ConnectionParams
    (host srv)
    (port srv)
    Nothing
    Nothing

  -- fix handle settings
  -- TODO: how to use with Network.Connection ?
  --hSetBuffering  h LineBuffering
  --hSetBinaryMode h False
  --hSetEncoding   h utf8

  --
  -- create instance object
  --

  -- IO & thread communication
  hv <- newTVarIO $ Just h
  cc <- newChan -- commands
  mc <- newChan -- messages
  lv <- newTVarIO Nothing -- listen worker thread ID
  cv <- newTVarIO Nothing -- command worker thread ID

  -- current IRC state
  csv <- newTVarIO [] -- current channels
  nv  <- newTVarIO nick'
  uv  <- newTVarIO user'
  rv  <- newTVarIO rn

  let inst = IrcInstance def hv lv cv cc mc csv nv uv rv

  --
  -- start workerthreads
  --

  -- wait until thread ID is set before doing anything
  let wait v = atomically $ requireTVar v

  listenTID <- forkIO $ wait lv >> listenForMessages inst mlog
  cmdTID    <- forkIO $ wait cv >> handleIrcCommands inst mlog `finally` shutdown inst mlog

  -- set worker thread IDs
  atomically $ do
    writeTVar lv (Just listenTID)
    writeTVar cv (Just cmdTID)

  --
  -- Initial interaction with IRC server
  --

  -- give command to send user information to server
  sendCmd inst $ Send (nick (B8.pack nick'))
  sendCmd inst $ Send (user (B8.pack user') "*" "*" (B8.pack rn))

  -- Wait for the 001 message before "giving away" our connection
  fix $ \loop -> do
    msg <- getMsg inst
    logMaybeIO mlog 3 $ "waitFor001 --- " ++ show msg
    case msg of
      Message { msg_command = "001" } -> return ()
      _                               -> loop

  -- send join commands for all channels
  mapM_ (sendCmd inst . Join) chans

  --
  -- Return instance
  --

  return $ Right inst


-- | Shutdown IRC instance, but do not kill own thread
shutdown :: IrcInstance -> Maybe LogInstance -> IO ()
shutdown inst mlog = do

  (mc, tids) <- atomically $ do

    -- get current handle
    mc <- swapTVar (networkConnection inst) Nothing

    -- get all worker thread IDs
    c  <- swapTVar (cmdThreadId inst)    Nothing
    l  <- swapTVar (listenThreadId inst) Nothing
    let tids = [ tid | Just tid <- [c,l]]

    return (mc, tids)

  logMaybeIO mlog 3 $ "Shutting down IRC instance. Thread IDs = " ++ show tids

  -- close handle
  maybe (return ()) connectionClose mc

  -- kill other worker threads (not own!)
  myTID <- myThreadId
  mapM_ (\tid -> unless (tid == myTID) (killThread tid)) tids


-------------------------------------------------------------------------------
-- Worker Threads

-- Wait for commands, execute them
handleIrcCommands :: IrcInstance -> Maybe LogInstance -> IO ()
handleIrcCommands inst _mlog = forever $ do

  c <- readChan (cmdChan inst)
  case c of

       Send msg           -> sendMsg $ msg
       PrivMsg to msg     -> sendMsg $ privmsg (B8.pack to) (T.encodeUtf8 msg)
       Notice  to msg     -> sendMsg $ notice  to msg
       Join chan          -> sendMsg $ joinChan chan
       Part chan          -> sendMsg $ part chan
       Nick new           -> sendMsg $ nick (B8.pack new)

       Ping               -> sendMsg $ Message Nothing "PING" []
       Pong               -> sendMsg $ Message Nothing "PONG" []

       Quit msg           ->
         sendMsg (quit (T.encodeUtf8 <$> msg)) `finally` throw ThreadKilled

 where
  -- | Send a message in current Hirc instance
  sendMsg :: Message -> IO ()
  sendMsg m = do

    c <- atomically $ requireTVar (networkConnection inst)

    -- send encoded message
    connectionPut c $ encode m `BS.append` "\n"

-- Listen on handle and put incoming messages to our Chan
listenForMessages :: IrcInstance -> Maybe LogInstance -> IO ()
listenForMessages inst mlog = forever $ do

  -- get current handle & output chan
  c <- atomically $ requireTVar (networkConnection inst)

  bs <- connectionGetLine 10000 c
  case decode bs of

    -- successful message decode
    Just m -> writeChan (msgChan inst) m

    -- fallback TODO
    _ -> logMaybeIO mlog 1 $ "Could not decode bytestring: " ++ B8.unpack bs


-------------------------------------------------------------------------------
-- Helper functions

-- Sending/receiving commands/messages

sendCmd :: IrcInstance -> ConnectionCommand -> IO ()
sendCmd inst c = writeChan (cmdChan inst) c

getMsg :: IrcInstance -> IO Message
getMsg inst = readChan (msgChan inst)

-- IRC messages

mkMessage :: String -> [Parameter] -> Message
mkMessage cmd params = Message Nothing (B8.pack cmd) params

notice :: Nickname -> Text -> Message
notice n t = mkMessage "NOTICE" [B8.pack n, T.encodeUtf8 t]
