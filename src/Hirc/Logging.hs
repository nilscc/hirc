{-# LANGUAGE FlexibleContexts #-}

module Hirc.Logging
  ( logM
  , startLogging
    -- * Log settings
  , debugHircSettings
  , debugManagedSettings
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.MState
import Control.Monad
import Control.Monad.IO.Peel
import Control.Monad.Trans
import Data.Time
import System.Directory

import Hirc.Types

--
-- Settings
--

debugHircSettings :: IrcServer -> LogSettings
debugHircSettings srv = LogSettings
  { logLevel      = 3
  , logPrintLevel = 1
  , logFile       = "logs/" ++ host srv ++ "_" ++ show (port srv) ++ ".log"
  }

debugManagedSettings :: LogSettings
debugManagedSettings = LogSettings
  { logLevel      = 3
  , logPrintLevel = 2
  , logFile       = "logs/managed.log"
  }

--
-- Logging: Hirc
--

-- | Log a message
logM :: LogM m => Int -> String -> m ()
logM l s = do
  now <- liftIO getCurrentTime
  let f = "(" ++ show now ++ ") " ++ s
  lc <- logChan
  liftIO $ writeChan lc (l,f)

logLoop :: LogM m => m ()
logLoop = do
  lc <- logChan
  LogSettings lF lP fp <- logSettings
  now <- liftIO getCurrentTime
  liftIO $
    writeFile fp $ "New logging session [" ++ show now ++ "]\n\n"
  forever $ do
    (l,s) <- liftIO $ readChan lc
    when (l <= lF) $
      liftIO $ appendFile fp (s++"\n")
    when (l <= lP) $
      liftIO $ putStrLn s

-- | Start the log loop in a `MState` thread
startLogging :: (LogM (MState t m), Forkable m, MonadPeelIO m)
  => MState t m ()
startLogging = do
  liftIO $ do
    e <- doesDirectoryExist "logs"
    unless e $ createDirectory "logs"
  forkM logLoop >> return ()
