{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fno-warn-unused-do-bind #-}

module Hirc.Connection.Managed
  ( -- * Manage and run HircM
    runHircM
  , forkHircM

    -- * Running managed connections
  --   manage
  -- , runManaged
  ) where

import Prelude

--import Control.Concurrent
import Control.Concurrent.STM ( newTVarIO, readTVar, STM, TVar, readTVarIO, newEmptyTMVarIO, atomically )
import Control.Concurrent.STM.TMVar ( TMVar, putTMVar )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( runExceptT )
import Control.Monad.Reader ( ReaderT(runReaderT), MonadReader (ask) )

import Hirc.Types.Hirc
    ( HircInstance(..),
      LogInstance(..),
      HircDefinition(..),
      HircError,
      HircM(HircM, unHircM), IrcDefinition (ircServer), IrcInstance (listenThreadId, cmdThreadId), Module (Module) )
import Hirc.Logging ( debugHircSettings, startLogging )
import Control.Concurrent (newChan, ThreadId, forkIO)
import Hirc.Connection (createInstance)
import Data.Maybe (catMaybes)


--------------------------------------------------------------------------------
-- | Run managed sessions
{-
runManaged :: MonadIO m => Chan ThreadId -> ManagedM a -> m a
runManaged ctid m = do
    c <- liftIO $ newChan
    let settings = ManagedSettings
          { logChanM       = c
          , logSettingsM   = debugManagedSettings
          , managedThreads = ctid
          }
    liftIO $ runReaderT (unManagedM $ startLogging >> m) settings
-}

--runIrcInstance :: MonadIO m => IrcDefinition -> m IrcInstance
--runIrcInstance def = do

--------------------------------------------------------------------------------
-- Manage sessions

runHircM :: MonadIO m => HircDefinition -> HircM a -> m (Either HircError a)
runHircM def (HircM r) = liftIO $ do

  -- prepare irc instance
  let ircDef = ircDefinition def
  ircInstTVar <- newTVarIO . Just =<< createInstance ircDef

  -- setup logging
  logThreadIdTVar <- newTVarIO Nothing
  logChan' <- newChan
  let
    logSet = debugHircSettings $ ircServer ircDef
    logInst = LogInstance logSet logThreadIdTVar logChan'

  startLogging logInst
  logInstTVar <- newTVarIO $ Just logInst

  -- setup modules
  modsTVar <- newTVarIO $ modulesDefinition def

  -- create final hirc instance
  let
    inst = HircInstance {
      modules = modsTVar,
      ircInstance = ircInstTVar,
      logInstance = logInstTVar }

  -- run hirc monad
  runExceptT $ runReaderT r inst

forkHircM :: HircM a -> HircM (ThreadId, TMVar (Either HircError a))
forkHircM m = do
  h <- ask
  liftIO $ do
    res <- newEmptyTMVarIO
    tid <- forkIO $ do
      r <- runExceptT $ runReaderT (unHircM m) h
      atomically $ putTMVar res r
    return (tid, res)

  
{-
runHircWithSettings :: HircSettings -> HircM () -> ManagedM ()
runHircWithSettings settings hirc = do
  merr <- runHircM settings hirc
  case merr of
       Left err -> handleHircError err settings hirc
       Right _  -> return ()
-}


{-
-- | Start and manage new connections
manage :: EventLoop
       -> Hirc
       -> ManagedM ()
manage eventLoop hirc = do
  cmd <- liftIO newChan
  msg <- liftIO newChan
  err <- liftIO newEmptyMVar
  lc  <- liftIO newChan
  tch <- asks managedThreads
  let srv = server hirc
      settings = HircSettings
        { runningHirc        = hirc
        , cmdChan            = cmd
        , msgChan            = msg
        , errMVar            = err
        , logChanH           = lc
        , logSettingsH       = debugHircSettings srv
        , managedThreadsChan = tch
        }
  tid <- liftIO . forkIO $ do
    _ <- runHircM settings (startLogging >> eventLoop `finally'` shutdown)
    return ()
  liftIO $ writeChan tch tid
  logM 1 $ "Managing new server: " ++ host srv ++ ":" ++ show (port srv)
 where
  -- `finally` lifted to MonadError
  finally' a b = a `catchError` (\_ -> b)

  shutdown = do
    logM 1 "Shutting down all modules…"
    onMods $ \(Module mm s) -> do
      logM 3 $ "Shutting down " ++ moduleName mm ++ "."
      maybe (return ()) ($ s) (shutdownModule mm)
    logM 1 "Modules offline."

  onMods :: (LogM m, MonadReader HircSettings m, MonadError HircError m)
         => (Module -> m ())
         -> m ()
  onMods f = do
    mods <- asks (modules . runningHirc)
    mapM_ (\m -> f m `catchError` moduleException m) mods

  moduleException :: LogM m => Module -> HircError -> m ()
  moduleException (Module mm _) e = do
    logM 1 $ "Module exception in \"" ++ moduleName mm ++ "\": " ++ show e
-}

--------------------------------------------------------------------------------
-- Error handling

{-
handleHircError :: HircError
                -> HircSettings
                -> HircM ()
                -> ManagedM ()
-}

{-
handleHircError H_ConnectionLost s hirc = do
  let srv = server s
      rec@(Reconnect t c w l) = reconnects srv
  now <- liftIO getCurrentTime
  if c < t then do
    let newRec = rec { recCount = c + 1, recLastTry = Just now }
    forkM $
      runHircWithSettings (updateRec s newRec) hirc
    return ()
   else
    case l of
         Nothing -> do
           logM $ "handleHircError (H_ConnectionLost), \
                  \the impossible happened: c >= t but no recLastTry"
           let newRec = rec { recLastTry = Just now }
           handleHircError H_ConnectionLost (updateRec s newRec) hirc
         Just time
           | addUTCTime w time <= now -> do
             let newRec = rec { recCount = 0, recLastTry = Just now }
             forkM $
               runHircWithSettings (updateRec s newRec) hirc
             return  ()
           | otherwise -> do
             forkM $ do
               delayUntil time
               let newRec = rec { recCount = 0, recLastTry = Just time }
               runHircWithSettings (updateRec s newRec) hirc
             return ()

  where
    updateRec :: HircSettings -> Reconnect -> HircSettings
    updateRec settings newRec =
      let srv    = server settings
          newSrv = srv { reconnects = newRec }
       in settings { server = newSrv }
-}

{-
handleHircError e _ _ = do
  logM 1 $ "Unknown error caught: " ++ show e
-}
