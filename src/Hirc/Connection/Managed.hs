{-# OPTIONS -fno-warn-unused-do-bind #-}

module Hirc.Connection.Managed
  (

    -- * Running managed connections
    manage
  , runManaged
  ) where

import Control.Concurrent
import Control.Concurrent.MState
import Control.Monad.Error
import Control.Monad.Reader
-- import Data.Time
import qualified Data.Map as M

import Hirc.Types
import Hirc.Logging
-- import Utils

--------------------------------------------------------------------------------
-- | Run managed sessions
runManaged :: MonadIO m => Managed a -> m a
runManaged m = do
    c <- liftIO $ newChan
    let settings = ManagedSettings
          { logChanM     = c
          , logSettingsM = debugManagedSettings
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
    defState = HircState Nothing "" "" "" M.empty
    setState :: HircM ()
    setState = do
      h <- asks runningHirc
      modify $ \hs -> hs
        { ircNickname = nickname h
        , ircUsername = username h
        , ircRealname = realname h
        }

runHircWithSettings :: HircSettings -> HircM () -> Managed ()
runHircWithSettings settings hirc = do
  merr <- runHircM settings hirc
  case merr of
       Left err -> handleHircError err settings hirc
       Right _  -> return ()


-- | Start and manage new connections
manage :: EventLoop
       -> Hirc
       -> Managed ()
manage eventLoop hirc = do
  cmd <- liftIO newChan
  msg <- liftIO newChan
  err <- liftIO newEmptyMVar
  lc  <- liftIO newChan
  let srv = server hirc
      settings = HircSettings
        { runningHirc  = hirc
        , cmdChan      = cmd
        , msgChan      = msg
        , errMVar      = err
        , logChanH     = lc
        , logSettingsH = debugHircSettings srv
        }
  forkM $
    runHircWithSettings settings (startLogging >> eventLoop)
  logM 1 $ "Managing new server: " ++ host srv ++ ":" ++ show (port srv)

--------------------------------------------------------------------------------
-- Error handling

handleHircError :: HircError
                -> HircSettings
                -> HircM ()
                -> Managed ()

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
