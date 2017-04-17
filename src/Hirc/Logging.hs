{-# LANGUAGE FlexibleContexts #-}

module Hirc.Logging
  ( startLogging
  , logIO
    -- * Log settings
  , debugHircSettings
  , debugManagedSettings
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
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
logIO :: LogInstance -> Int -> String -> IO ()
logIO inst l s = do

  -- add timestamp to log message
  now <- getCurrentTime
  let f = "(" ++ show now ++ ") " ++ s

  -- write msg to log chan
  writeChan (logChan inst) (l,f)

-- | Store all log messages in the \"logs\" directory
startLogging :: LogInstance -> IO ()
startLogging inst = do

  -- lookup log settings
  let LogSettings lF lP fp = logSettings inst

  -- retrieve valid log chan
  let lc = logChan inst

  -- check if logging directory exists
  e <- doesDirectoryExist "logs"
  unless e $ createDirectory "logs"

  -- start a new thread which stores all log messages
  tid <- forkIO $ do
    now <- getCurrentTime
    writeFile fp $ "New logging session [" ++ show now ++ "]\n\n"
    forever $ do
      (l,s) <- readChan lc
      when (l <= lF) $
        appendFile fp (s++"\n")
      when (l <= lP) $
        putStrLn s

  -- store thread ID & kill old logging thread if necessary
  mOldTid <- atomically $ swapTVar (logThreadId inst) (Just tid)
  case mOldTid of
    Just oldTid -> killThread oldTid
    Nothing     -> return ()
