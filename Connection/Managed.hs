{-# OPTIONS -fno-warn-unused-do-bind #-}

module Connection.Managed
  (

    -- * Running managed connections
    run
  , manage
  ) where

import Control.Concurrent
import Control.Concurrent.MState
import Control.Monad.Error
import Control.Monad.Reader
import Data.Time

import Hirc
import Logging
import Utils

--
-- Run MStates
--

run :: Managed a -> IO a
run m = evalMState (startLoggingM >> m) defState
  where
    defState :: ManagedState
    defState = ManagedState Nothing

runHirc :: HircSettings -> Hirc a -> IO (Either HircError a)
runHirc s r = runErrorT (runReaderT (evalMState r defState) s)
  where
    defState :: HircState
    defState = HircState Nothing Nothing

runHircWithSettings :: HircSettings -> Hirc () -> Managed ()
runHircWithSettings settings hirc = do
  merr <- liftIO $ runHirc settings hirc
  case merr of
       Left err -> handleHircError err settings hirc
       Right _  -> return ()

-- | Start a new connection and prepare it to be managed by `manageConnections`
manage :: IrcServer
       -> Hirc ()
       -> Managed ()
manage srv hirc = do
  cmd <- liftIO newChan
  msg <- liftIO newChan
  err <- liftIO newEmptyMVar
  let settings = HircSettings
        { server  = srv
        , cmdChan = cmd
        , msgChan = msg
        , errMVar = err
        , runH    = hirc
        }
  forkM $
    runHircWithSettings settings (startLoggingH >> hirc)
  return ()

--
-- Error handling
--

handleHircError :: HircError
                -> HircSettings
                -> Hirc ()
                -> Managed ()

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

handleHircError e _ _ = do
  logM $ "Unknown error caught: " ++ show e