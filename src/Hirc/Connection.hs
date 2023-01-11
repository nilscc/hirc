{-# LANGUAGE OverloadedStrings #-}

module Hirc.Connection
    (
      -- * Connection
      createInstance
    , connect
    , sendCmd

      -- * Settings
    , stdReconnect

      -- * Data types
    , module Network.IRC

      -- * Other
    , requireTVar
    , awaitTVar
    ) where

import Control.Exception
    ( finally,
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
      swapTVar, readTVarIO, newBroadcastTChan, newBroadcastTChanIO, dupTChan, readTChan, writeTChan )
import Control.Monad ( forever, unless, when )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Function ( fix )
import Network.Connection (initConnectionContext, connectTo, ConnectionParams (ConnectionParams, connectionHostname, connectionPort, connectionUseSecure, connectionUseSocks), connectionClose, connectionGetLine, connectionPut, TLSSettings (TLSSettingsSimple, settingDisableCertificateValidation))
import Network.IRC

import Hirc.Types.Connection
    ( ConnectionCommand(..),
      NickName,
      ChannelName,
      Reconnect(Reconnect),
      IrcServer(port, host) )
import Hirc.Types.Hirc ( LogInstance(..), IrcInstance(..), IrcDefinition(..), HircInstance (ircInstance) )
--import Hirc.Connection.Managed
import Hirc.Logging ( logMaybeIO )
import Data.Maybe (isNothing)

requireTVar :: TVar (Maybe a) -> STM a
requireTVar v = readTVar v >>= maybe retry return

  -- wait until thread ID is set before doing anything
awaitTVar :: TVar (Maybe a) -> STM a
awaitTVar = requireTVar

-- | Standard reconnect settings with 1 hours delay between retries
stdReconnect :: Int -> Reconnect
stdReconnect t = Reconnect t 0 (60 * 60 * 1) Nothing

-- | Connect to a IRC server, returns two functions. The first one will return
-- incoming messages, the second will handle commands
createInstance :: IrcDefinition -> IO IrcInstance
createInstance def@(IrcDefinition srv chans nick' user' rn) = do
  --
  -- create instance object
  --

  -- IO & thread communication
  ctxt <- newTVarIO Nothing
  nc <- newTVarIO $ Nothing
  cc <- newChan -- commands
  mc <- newBroadcastTChanIO -- messages
  lv <- newTVarIO Nothing -- listen worker thread ID
  cv <- newTVarIO Nothing -- command worker thread ID

  -- current IRC state
  csv <- newTVarIO $ ircChannels def
  nv  <- newTVarIO nick'
  uv  <- newTVarIO user'
  rv  <- newTVarIO rn

  return
    IrcInstance
      { ircInstanceServer = ircServer def,
        connectionContext = ctxt,
        networkConnection = nc,
        listenThreadId = lv,
        cmdThreadId = cv,
        cmdChan = cc,
        msgBroadcast = mc,
        currentChannels = csv,
        currentNickname = nv,
        currentUsername = uv,
        currentRealname = rv
      }

connect :: IrcInstance -> Maybe LogInstance -> IO ()
connect ircInstance mLogInstance = do

  --
  -- Connect to server
  --

  -- check if connection context is already set, and create new one if not
  mCtxt <- readTVarIO $ connectionContext ircInstance
  when (isNothing mCtxt) $ do
    ctxt <- initConnectionContext
    atomically $ do
      mCtxt' <- readTVar $ connectionContext ircInstance
      when (isNothing mCtxt') $ writeTVar (connectionContext ircInstance) (Just ctxt)

  -- load connection context from instance
  ctxt <- atomically $ requireTVar (connectionContext ircInstance)

  -- define connection parameters
  let ircServer = ircInstanceServer ircInstance
      params = ConnectionParams {
        connectionHostname = host ircServer,
        connectionPort = port ircServer, 
        connectionUseSecure = Just (TLSSettingsSimple True False False),
        connectionUseSocks = Nothing }

  -- create connection
  con <- connectTo ctxt params

  -- store connection in instance
  atomically $ writeTVar (networkConnection ircInstance) (Just con)

  -- fix handle settings
  -- TODO: how to use with Network.Connection ? necessary?
  --hSetBuffering  h LineBuffering
  --hSetBinaryMode h False
  --hSetEncoding   h utf8

  --
  -- start workerthreads
  --

  let wait = atomically . awaitTVar

  listenTID <- forkIO $ do
    -- wait for thread to be setup properly
    _ <- wait $ listenThreadId ircInstance
    listenForMessages ircInstance mLogInstance

  cmdTID <- forkIO $ do
    -- wait for thread to be setup properly
    _ <- wait $ cmdThreadId ircInstance
    handleIrcCommands ircInstance mLogInstance `finally` shutdown ircInstance mLogInstance

  -- set worker thread IDs
  atomically $ do
    writeTVar (listenThreadId ircInstance) (Just listenTID)
    writeTVar (cmdThreadId ircInstance) (Just cmdTID)

  --
  -- Initial interaction with IRC server
  --

  -- get current nick and username
  (nick', user', real') <- atomically $ (,,)
    <$> readTVar (currentNickname ircInstance)
    <*> readTVar (currentUsername ircInstance)
    <*> readTVar (currentRealname ircInstance)

  -- give command to send user information to server
  sendCmd ircInstance $ Send (nick (B8.pack nick'))
  sendCmd ircInstance $ Send (user (B8.pack user') "*" "*" (B8.pack real'))

  -- Wait for the 001 message before "giving away" our connection
  msgChan <- atomically $ dupTChan (msgBroadcast ircInstance)
  fix $ \loop -> do
    msg <- atomically $ readTChan msgChan
    logMaybeIO mLogInstance 3 $ "waitFor001 --- " ++ show msg
    case msg of
      Message { msg_command = "001" } -> return ()
      _                               -> loop

  -- send join commands for all channels
  chans <- readTVarIO $ currentChannels ircInstance
  mapM_ (sendCmd ircInstance . Join) chans


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
       Notice  to msg     -> sendMsg $ notice to msg
       Join chan          -> sendMsg $ joinChan (B8.pack chan)
       Part chan          -> sendMsg $ part (B8.pack chan)
       Nick new           -> sendMsg $ nick (B8.pack new)

       Ping               -> sendMsg $ Message Nothing "PING" []
       Pong params        -> sendMsg $ Message Nothing "PONG" (map B8.pack params)

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
    Just m -> atomically $ writeTChan (msgBroadcast inst) m

    -- fallback TODO
    _ -> logMaybeIO mlog 1 $ "Could not decode bytestring: " ++ B8.unpack bs


-------------------------------------------------------------------------------
-- Helper functions

-- Sending/receiving commands/messages

sendCmd :: IrcInstance -> ConnectionCommand -> IO ()
sendCmd inst = writeChan (cmdChan inst)

-- IRC messages

mkMessage :: String -> [Parameter] -> Message
mkMessage cmd = Message Nothing (B8.pack cmd)

notice :: NickName -> Text -> Message
notice n t = mkMessage "NOTICE" [B8.pack n, T.encodeUtf8 t]
