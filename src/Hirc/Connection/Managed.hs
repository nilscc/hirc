{-# OPTIONS -fno-warn-unused-do-bind #-}

module Hirc.Connection.Managed
  (

    -- * Running managed connections
    manage
  , runManaged
  ) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Concurrent.MState
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.IO.Peel
import Control.Exception.Peel

import Hirc.Types
import Hirc.Logging


--------------------------------------------------------------------------------
-- | Run managed sessions
runManaged :: MonadIO m => Chan ThreadId -> ManagedM a -> m a
runManaged ctid m = do
    c <- liftIO $ newChan
    let settings = ManagedSettings
          { logChanM       = c
          , logSettingsM   = debugManagedSettings
          , managedThreads = ctid
          }
    liftIO $ runReaderT (evalMState True (startLogging >> m) defState) settings
  where
    defState :: ManagedState
    defState = ManagedState

--------------------------------------------------------------------------------
-- Manage sessions

runHircM :: MonadIO m => HircSettings -> HircM a -> m (Either HircError a)
runHircM s r = liftIO $ runErrorT (runReaderT (evalMState True (setState >> r) defState) s)
  where
    defState :: HircState
    defState = HircState
      { connectedHandle = Nothing
      , ircNickname     = ""
      , ircUsername     = ""
      , ircRealname     = ""
      , runningModules  = []
      }
    setState :: HircM ()
    setState = do
      h <- asks runningHirc
      modify $ \hs -> hs
        { ircNickname = nickname h
        , ircUsername = username h
        , ircRealname = realname h
        }

runHircWithSettings :: HircSettings -> HircM () -> ManagedM ()
runHircWithSettings settings hirc = do
  merr <- runHircM settings hirc
  case merr of
       Left err -> handleHircError err settings hirc
       Right _  -> return ()


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
  tid <- forkM $
    runHircWithSettings settings (startLogging >> eventLoop `finally` shutdown)
  liftIO $ writeChan tch tid
  logM 1 $ "Managing new server: " ++ host srv ++ ":" ++ show (port srv)
 where
  shutdown = do
    logM 1 "Shutting down all modules…"
    onMods $ \(Module mm s) -> do
      logM 3 $ "Shutting down " ++ moduleName mm ++ "."
      maybe (return ()) ($ s) (shutdownModule mm)
    logM 1 "Modules offline."

  onMods :: ContainsHirc m
         => (Module -> m ())
         -> m ()
  onMods f = fmap runningModules getHircState
         >>= mapM_ (\m -> f m `catch` moduleException m)

  moduleException :: (LogM m, MonadPeelIO m) => Module -> SomeException -> m ()
  moduleException (Module mm _) e = do
    logM 1 $ "Module exception in \"" ++ moduleName mm ++ "\": " ++ show e

--------------------------------------------------------------------------------
-- Error handling

handleHircError :: HircError
                -> HircSettings
                -> HircM ()
                -> ManagedM ()

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

handleHircError e _ _ = do
  logM 1 $ "Unknown error caught: " ++ show e
