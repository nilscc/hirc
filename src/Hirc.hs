{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS -fno-warn-incomplete-patterns
            -fno-warn-missing-fields
            -fno-warn-unused-do-bind
            #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hirc
  ( -- requireHandle

    -- * Hirc types & functions
  --  Hirc (..), newHirc
    run
  --, HircM
  --, HircError (..)
  --, HircState (..)
  --, HircSettings (..)
  , HircDefinition (..)
  , IrcDefinition (..)
  --, LogDefinition

  , ask, asks
  , getNickname, getUsername, getRealname, changeNickname

    -- * Module types & functions
  , Module, newModule
  , ModuleM, ModuleMessageM, IsModule (..)

    -- ** Module acid states
  , module Hirc.Acid

    -- * Messages & user commands
  , userCommand, onValidPrefix, onNickChange, onCommand
  , done, doneAfter, getCurrentChannel
  , withNickname, withUsername, withNickAndUser, withServer, withParams
  , MessageM, HircM
  , ContainsMessage (..)
  , IsHircCommand (..)

    -- * IRC types & functions
  , answer, say, whisper, sayIn
  , joinChannel, partChannel, sendNotice, quitServer
  , Message (..)
  , IrcServer (..)
  , Reconnect (..)
  , stdReconnect
  --, ConnectionCommand (..)
  , ChannelName
  , NickName
  , UserName
  , RealName

    -- * Concurrency
  -- , newThread
  -- , wait

    -- * Logging
  , LogSettings (..)
  , logM

    -- * Other
  , module Hirc.Utils

  , module Control.Monad.Reader

  ) where

import Data.ByteString.Char8 as B8 (pack, unpack)
import Control.Concurrent
    ( ThreadId, forkIO, throwTo )
import Control.Concurrent.STM (TVar)
import Control.Monad (when, forever, unless, forM, forM_, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReaderT, ReaderT(..))
import qualified Control.Monad.Reader as R
import Control.Exception (AsyncException(..), throw, SomeException, IOException, PatternMatchFail, BlockedIndefinitelyOnSTM)
import Control.Exception.Peel (handle, catch, finally)
import Control.Monad.IO.Peel (MonadPeelIO(..))
import Control.Monad.Error.Class (catchError, MonadError (throwError))
import Data.Maybe ( catMaybes )
import Data.Text as T (pack)
import Text.Regex.Posix ( (=~) )

import Hirc.Acid
import Hirc.Commands ( userCommand )
import Hirc.Connection
    ( Message(..), Channel, stdReconnect, connect, sendCmd, awaitTVar )
import Hirc.Messages
    ( handleIncomingMessage,
      done,
      doneAfter,
      getCurrentChannel,
      onCommand,
      withParams,
      withNickname,
      withUsername,
      withNickAndUser,
      withServer )
import Hirc.Logging ( logMaybeIO )
import Hirc.Types.Commands ( IsHircCommand(..) )
import Hirc.Types.Connection
    ( ConnectionCommand(Nick, PrivMsg, Part, Quit, Join, Pong, Notice),
      RealName,
      UserName,
      NickName,
      ChannelName,
      Reconnect(..),
      IrcServer(..) )
import Hirc.Types.Hirc
    ( IsModule(..),
      ModuleMessageM,
      ModuleM,
      Module(..),
      ContainsMessage(..),
      MessageM,
      HircInstance(..),
      HircDefinition(..),
      IrcDefinition(..),
      LogDefinition,
      LogSettings(..),
      IrcInstance (currentNickname, currentUsername, currentRealname, listenThreadId, cmdThreadId, msgBroadcast),
      LogInstance (logThreadId),
      HircM,
      HircError (HircConnectionLost),
      ContainsIrcInstance (askIrcInstance),
      CanSend,
      ContainsLogInstance (askLogInstance) )
import Hirc.Types.Instances () -- instances only
import Hirc.Utils
import Control.Concurrent.STM (readTVarIO, atomically, writeTVar, TChan, writeTChan, newTVarIO, newTChanIO, tryReadTChan, TMVar, takeTMVar, readTMVar, dupTChan)
import Hirc.Connection.Managed (runHircM, forkHircM)
import GHC.Conc.Sync (STM(STM))
import Control.Monad.Fix (fix)
import Hirc.Types (ContainsIrcInstance(askIrcInstance))

--------------------------------------------------------------------------------
-- Hirc instances

-- | Run Hirc instances. Needs to be run in the main thread to make sure all
-- modules get shut down correctly!   Otherwise: TODO
run :: MonadIO m => [HircDefinition] -> m ()
run hircs = liftIO $ do
  ct <- newTChanIO
  let
    async UserInterrupt = liftIO $ do
      tids <- atomically $ getTChanContents ct
      mapM_ (`throwTo` UserInterrupt) tids
    async e = throw e

  handle async $
    mapM_ (`runHircM` defEventLoop ct) hircs

 where
  getTChanContents :: TChan a -> STM [a]
  getTChanContents tc = do
    mval <- tryReadTChan tc
    case mval of
      Just val -> (val :) <$> getTChanContents tc
      Nothing  -> return []

--------------------------------------------------------------------------------
-- Modules

-- | Create an uninitialized module. Make sure to define `initModule'!
newModule :: IsModule m => m -> Module
newModule m = Module m undefined

ask :: (IsModule s, MonadIO m) => ReaderT (TVar (ModuleState s)) m (ModuleState s)
ask = do
  tvar <- R.ask
  liftIO $ readTVarIO tvar

asks :: (IsModule s, MonadIO m)
  => (ModuleState s -> a)
  -> ReaderT (TVar (ModuleState s)) m a
asks f = f <$> ask

--------------------------------------------------------------------------------
-- IRC stuff

sendCmd' :: CanSend m => ConnectionCommand -> m ()
sendCmd' cmd = do
  i <- askIrcInstance
  liftIO $ sendCmd i cmd

-- | Reply in a query to the user of the current message
whisper :: CanSend m => String -> m ()
whisper txt = withNickname $ \n -> sendCmd' $ PrivMsg n (T.pack txt)

reply :: CanSend m => (Either NickName ChannelName -> m ()) -> m ()
reply m = withParams $ \[c,_] -> do
  n <- getNickname
  if c == n then
    -- private message to user
    withNickname $ m . Left
   else
    -- public message to channel
    m $ Right c

-- | Answer and add the nick in public channels
answer :: CanSend m => String -> m ()
answer text =
  reply $
    either (\n ->
             sendCmd' $ PrivMsg n (T.pack text))
           (\c -> withNickname $ \n ->
             sayIn c $ n ++ ": " ++ text)

-- | Answer without prefix the nick in a public channel
say :: CanSend m => String -> m ()
say text =
  reply $ \to ->
    sendCmd' $ PrivMsg (either id id to) (T.pack text)

sayIn :: CanSend m => ChannelName -> String -> m ()
sayIn c text = sendCmd' $ PrivMsg c (T.pack text)

joinChannel :: CanSend m => ChannelName -> m ()
joinChannel ch = sendCmd' $ Join ch -- TODO: store current channels somewhere?

partChannel :: CanSend m => ChannelName -> m ()
partChannel ch = sendCmd' $ Part ch

sendNotice :: CanSend m => NickName -> String -> m ()
sendNotice n s = sendCmd' $ Notice n (T.pack s)

quitServer :: CanSend m => Maybe String -- ^ optional quit message
           -> m ()
quitServer mqmsg = sendCmd' $ Quit (T.pack <$> mqmsg)



--------------------------------------------------------------------------------
-- IRC tests

-- | Require prefixing user commands with the name of the bot (in a public
-- channel)
onValidPrefix :: (IsHircCommand m (String -> m ()), CanSend m) => m () -> m ()
onValidPrefix wm =
  onCommand "PRIVMSG" $ withParams $ \[c,_] -> do
    myNick <- getNickname
    if c == myNick then
      -- direct query
      wm
     else
      -- public channel with valid prefix
      userCommand $ \(validPrefix myNick -> True) -> wm
 where
  validPrefix :: NickName -> String -> Bool
  validPrefix n s = s =~ ("^" ++ escape n ++ "[:,.-\\!]?$")

  escape n = foldr esc "" n
  esc '\\' r = "\\\\" ++ r
  esc '['  r = "\\[" ++ r
  esc ']'  r = "\\]" ++ r
  esc '{'  r = "\\{" ++ r
  esc '}'  r = "\\}" ++ r
  esc a    r = a:r

-- | If a user changes his nick, the function receives the old nickname, the
-- username and the new nickname as arguments.
onNickChange :: CanSend m
             => (NickName -> UserName -> NickName -> m ())
             -> m ()
onNickChange m =
  onCommand "NICK" $
    withNickAndUser $ \old u ->
    withParams      $ \[new] -> do
      myNick <- getNickname
      myUser <- getUsername
      unless (old == myNick && u == myUser) $
        m old u new

--------------------------------------------------------------------------------
-- Handling incoming messages

logM :: ContainsLogInstance m => Int -> String -> m ()
logM l m = do
  mli <- askLogInstance
  liftIO $ logMaybeIO mli l m

-- irc commands: join
joinCmd :: (CanSend m, ContainsLogInstance m) => String -> m ()
joinCmd chan = do
  logM 1 $ "Joining: \"" ++ chan ++ "\""
  sendCmd' $ Join chan

-- irc commands: pong
pongCmd :: (CanSend m, ContainsLogInstance m) => [String] -> m ()
pongCmd params = do
  logM 4 $ "PING? PONG! " ++ show params
  sendCmd' $ Pong params

-- CTCP stuff
isCTCP :: String -> Bool
isCTCP s = not (null s)
        && head s == '\001' 
        && last s == '\001'

handleCTCP :: (CanSend m, ContainsLogInstance m) => String -> m ()

handleCTCP "\001VERSION\001" =
  withNickname $ \to -> do
    logM 2 $ "Sending CTCP VERSION reply to \"" ++ to ++ "\""
    sendCmd' $ Notice to (T.pack "\001VERSION hirc v0.2\001")

handleCTCP t =
  logM 2 $ "Unhandled CTCP: " ++ t

-- | Send a request to the server to change your own nickname.
changeNickname :: CanSend m => NickName -> m ()
changeNickname n = sendCmd' $ Nick n

setNickname :: ContainsIrcInstance m => String -> m ()
setNickname n = do
  i <- askIrcInstance
  liftIO . atomically $ writeTVar (currentNickname i) n

-- | Get the your own nickname.
getNickname :: ContainsIrcInstance m => m String
getNickname = do
  i <- askIrcInstance
  liftIO . readTVarIO $ currentNickname i

-- | Get your own username. If there is a leading '~' it is discarded.
getUsername :: ContainsIrcInstance m => m String
getUsername = do
  i <- askIrcInstance
  liftIO . readTVarIO $ currentUsername i

-- | Get your own realname.
getRealname :: ContainsIrcInstance m => m String
getRealname = do
  i <- askIrcInstance
  liftIO . readTVarIO $ currentRealname i

-- | Default event loop
defEventLoop :: TChan ThreadId -> HircM ()
defEventLoop chanThreadIds = do
  hircInst <- R.ask 

  -- start waiting for thread IDs
  liftIO $ do
    t1 <- forkIO $ atomically $ do
        ircInst <- awaitTVar (ircInstance hircInst)
        tid <- awaitTVar (listenThreadId ircInst)
        writeTChan chanThreadIds tid
    t2 <- forkIO $ atomically $ do
        ircInst <- awaitTVar (ircInstance hircInst)
        tid <- awaitTVar (cmdThreadId ircInst)
        writeTChan chanThreadIds tid
    t3 <- forkIO $ atomically $ do
        logInst <- awaitTVar (logInstance hircInst)
        tid <- awaitTVar (logThreadId logInst)
        writeTChan chanThreadIds tid
    atomically $ do
      writeTChan chanThreadIds t1
      writeTChan chanThreadIds t2
      writeTChan chanThreadIds t3

  -- connect to IRC server
  ircInst <- askIrcInstance
  mLogInst <- askLogInstance

  liftIO $ connect ircInst mLogInst

  -- init modules
  modsUninitialized <- liftIO $ readTVarIO (modules hircInst)
  mods <- fmap catMaybes . forM modsUninitialized $ \(Module mm _) ->
    catchError (Just . Module mm <$> initModule mm) $ \e -> do
      logM 1 $ "Initialization of module \"" ++ moduleName mm ++ "\" failed:" ++ show e
      return Nothing

  -- fork off modules!
  (res:_) <- forM mods $ \mod -> do
    (tid, res) <- defaultModuleLoop mod
    liftIO . atomically $ writeTChan chanThreadIds tid
    return res

  -- TODO: wait for all threads to finish
  res' <- liftIO $ atomically $ readTMVar res
  case res' of
    Left e -> liftIO . print $ "Module finished with error: " ++ show e
    Right _ -> return ()


defaultModuleLoop :: Module -> HircM (ThreadId, TMVar (Either HircError ()))
defaultModuleLoop (Module mm s) = do

  let 
      --logIOException :: (ContainsLogInstance m, MonadError HircError m) => HircError -> m ()
      logIOException (e :: IOException) = do
        logM 1 $ "IO exception: " ++ show e
        throwError HircConnectionLost
      logSTMException (e :: BlockedIndefinitelyOnSTM) = do
        logM 1 $ "Blocked indefinitely on STM transaction: " ++ show e
        throwError HircConnectionLost
      logModuleException (e :: SomeException) = do
        logM 1 $ "Exception: " ++ show e
        --throwError e

      handleAll :: (MonadPeelIO m, ContainsLogInstance m, MonadError HircError m) => m () -> m()
      handleAll = handle logIOException
                . handle logModuleException
                . handle logSTMException

  ts <- liftIO $ newTVarIO s
  forkHircM $ runReaderT `flip` ts $ do
    -- run start up functions
    runMaybe $ onStartup mm

    -- duplicate broadcast chan
    ircInst <- askIrcInstance
    msgChan <- liftIO $ atomically $ dupTChan (msgBroadcast ircInst)

    handleAll . forever . handleIncomingMessage msgChan $ do

      onCommand "PING" $ withParams pongCmd

      onCommand "NICK" $
        withNickAndUser $ \n u ->
        withParams      $ \[new] ->
        doneAfter       $ do
          myNick <- getNickname
          myUser <- getUsername
          when (n == myNick && u == myUser) $ do
             setNickname new
             logM 1 $ "Nick changed to \"" ++ new ++ "\""

      onCommand "INVITE" $
        withParams $ \[_,chan] -> joinCmd chan

      onCommand "PRIVMSG" $ do

        -- run CTCP shit, this stuff is a TODO
        withParams $ \[_,text] ->
          when (isCTCP text) $ do
            logM 2 $ "CTCP call: " ++ text
            handleCTCP text `catch` \(e :: SomeException) -> do
              logM 1 $ "CTCP exception: " ++ show e
              return ()
            done

      -- pass message to module
      runMaybe $ onMessage mm
  
 where
  runMaybe (Just go) = go
  runMaybe _         = return ()

--------------------------------------------------------------------------------
-- Concurrency

-- -- | Start a new thread and add it to the list of managed threads.
-- newThread :: MessageM () -> MessageM ThreadId
-- newThread m = do
--   r <- ask
--   tid <- lift $ forkM' (runReaderT m r :: HircM ())
--   tch <- lift $ asks managedThreadsChan
--   liftIO $ writeChan tch tid
--   return tid
-- 
-- wait :: Int   -- ^ number of seconds
--      -> MessageM ()
-- wait sec = liftIO $ threadDelay (sec * 1000000)
-- 