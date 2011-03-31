
module Logging where

import Control.Concurrent.Chan
import Control.Monad.Trans
import Data.Time

import Hirc

--
-- Logging: Hirc
--

logH :: String -> Hirc ()
logH s = do
  ml <- gets loggingH
  case ml of
       Just l  -> l s
       Nothing -> startLoggingH >> logH s

startLoggingH :: Hirc ()
startLoggingH = do
  IrcServer
    { host = h
    , port = p
    } <- asks server
  let logFile = "logs/" ++ h ++ "_" ++ show p ++ ".log"

  now <- liftIO $ getCurrentTime
  liftIO $ writeFile logFile $
    "New logging session  --  " ++ show now ++ "\n\n"

  logChan <- liftIO newChan
  _ <- forkM . liftIO $ do
    s <- readChan logChan
    appendFile logFile s

  let log' s = liftIO $ do
        t <- getCurrentTime
        writeChan logChan $
          "(" ++ show t ++ ") " ++ s ++ "\n"

  modifyM $ \s -> s { loggingH = Just log' }


--
-- Logging: Managed
--

logM :: String -> Managed ()
logM s = do
  ml <- gets loggingM
  case ml of
       Just l  -> l s
       Nothing -> startLoggingM >> logM s

startLoggingM :: Managed ()
startLoggingM = do
  let logFile = "logs/managed.log"

  now <- liftIO $ getCurrentTime
  liftIO $ writeFile logFile $
    "New logging session  --  " ++ show now ++ "\n\n"

  logChan <- liftIO newChan
  _ <- forkM . liftIO $ do
    s <- readChan logChan
    appendFile logFile s

  let log' s = liftIO $ do
        t <- getCurrentTime
        writeChan logChan $
          "(" ++ show t ++ ") " ++ s ++ "\n"
  modifyM $ \m ->  m { loggingM = Just log' }

