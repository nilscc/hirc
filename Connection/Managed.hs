module Connection.Managed
  ( Reconnect (..)
  , stdReconnect
  , ManagedServer

    -- * Running managed connections
  , runManaged
  , manageConnections
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Time
import qualified Control.Exception as E

import Types
import Utils (delayUntil)

-- | Standard reconnect settings with 6 hours waiting time between retries
stdReconnect :: Int -> Reconnect
stdReconnect t = Reconnect t 0 (60 * 60 * 6) Nothing

-- | Start a server connection and prepare it to be managed by
-- `manageReconnects`.
runManaged :: IrcServer 
           -> (IrcServer -> IO ())
           -> IO ManagedServer
runManaged srv connectionLoop = do
  waitForIt <- atomically newEmptyTMVar
  forkIO $
    connectionLoop srv `E.finally` do
      now <- getCurrentTime
      putStrLn $ "(" ++ show now ++ ") runManaged: connectionLoop crashed"
      atomically $ putTMVar waitForIt ()
  return $ ManagedServer (srv, waitForIt)

-- | Manage connections initiated by `runWithReconnects`
manageConnections :: [ManagedServer]
                  -> (IrcServer -> IO ())
                  -> IO ()
manageConnections srv_tm loop = do
  newList <- join . atomically $ waitForSTM srv_tm loop
  case newList of
       [] -> return ()
       _  -> manageConnections newList loop

--
-- internals
--

waitForSTM :: [ManagedServer]
           -> (IrcServer -> IO ())
           -> STM (IO [ManagedServer])
waitForSTM [] _ =
  retry
waitForSTM (ManagedServer (srv,tm):r) loop =
  orElse
    (do x <- takeTMVar tm
        return $ restartOrDelay srv tm r loop)
    (waitForSTM r loop)

restartOrDelay :: IrcServer
               -> TMVar ()
               -> [ManagedServer]
               -> (IrcServer -> IO ())
               -> IO [ManagedServer]
restartOrDelay srv tm r loop =
  if count < times then
    return (ManagedServer (newSrv,tm) : r)
   else do
    now <- getCurrentTime
    case last of
         Nothing -> restartSrv now
         Just t
           | addUTCTime wait t <= now -> restartSrv now
           | otherwise                -> delaySrv (addUTCTime wait t)
  where
    rec@(Reconnect times count wait last) = reconnects srv
    newRec = rec { recCount = count + 1 }
    newSrv = srv { reconnects = newRec }

    restartSrv now = do
      forkIO $ do
        -- wait five seconds
        threadDelay (1000000 * 5)
        loop srv `E.finally` atomically (putTMVar tm ())
      let rec' = rec { recLastTry = Just now, recCount = 0 }
          srv' = srv { reconnects = rec' }
      return (ManagedServer (srv', tm):r)

    delaySrv t = do
      forkIO $
        delayUntil t >> atomically (putTMVar tm ())
      let rec' = rec { recLastTry = Nothing, recCount = 0 }
          srv' = srv { reconnects = rec' }
      return (ManagedServer (srv', tm):r)
