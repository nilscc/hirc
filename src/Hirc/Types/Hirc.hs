{-# LANGUAGE TypeSynonymInstances #-}

module Hirc.Types.Hirc where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.MState
import Control.Monad.IO.Peel
import Control.Monad.Reader
import Control.Monad.Error
import Network.IRC
import System.IO

import Hirc.Types.Connection


--------------------------------------------------------------------------------
-- The Hirc monad

type Hirc = MState HircState (ReaderT HircSettings (ErrorT HircError IO))

data HircError
  = H_NotConnected
  | H_ConnectionLost
  | H_ConnectionFailed
  | H_Other String
  deriving (Show, Eq)

instance Error HircError where
  strMsg = H_Other

data HircSettings = HircSettings
  { server          :: IrcServer
  , cmdChan         :: Chan ConnectionCommand
  , msgChan         :: Chan Message
  , errMVar         :: MVar HircError
  , runH            :: Hirc ()
  , logChanH        :: Chan (Int,String)
  , logSettingsH    :: LogSettings
  }

data HircState = HircState
  { connectedHandle :: Maybe Handle
  , ircNickname     :: String
  , ircUsername     :: String
  , ircRealname     :: String
  }


--------------------------------------------------------------------------------
-- The Managed monad

type Managed = MState ManagedState (ReaderT ManagedSettings IO)

data ManagedState = ManagedState

data ManagedSettings = ManagedSettings
  { logChanM      :: Chan (Int,String)
  , logSettingsM  :: LogSettings
  }


--------------------------------------------------------------------------------
-- Logging

class MonadPeelIO m => LogM m where
  logChan     :: m (Chan (Int,String))
  logSettings :: m LogSettings

instance LogM Hirc where
  logChan     = asks logChanH
  logSettings = asks logSettingsH

instance LogM Managed where
  logChan     = asks logChanM
  logSettings = asks logSettingsM

data LogSettings = LogSettings
  { logLevel      :: Int
  , logPrintLevel :: Int
  , logFile       :: FilePath
  }
