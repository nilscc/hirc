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

data Hirc = Hirc
  { server      :: IrcServer
  , channels    :: [Channel]
  , nickname    :: String
  , username    :: String
  , realname    :: String
  , eventQueue  :: HircM ()
  , modules     :: [Module]
  }

type HircM = MState HircState (ReaderT HircSettings (ErrorT HircError IO))

data HircError
  = H_NotConnected
  | H_ConnectionLost
  | H_ConnectionFailed
  | H_Other String
  deriving (Show, Eq)

instance Error HircError where
  strMsg = H_Other

data HircSettings = HircSettings
  { runningHirc     :: Hirc
  , cmdChan         :: Chan ConnectionCommand
  , msgChan         :: Chan Message
  , errMVar         :: MVar HircError
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
-- Message filter monad

type WithMessage = ReaderT Message HircM

class MonadPeelIO m => Filtered m where
  runFiltered :: m () -> WithMessage ()

instance Filtered WithMessage where
  runFiltered = id

instance Filtered HircM where
  runFiltered = lift

instance LogM WithMessage where
  logChan     = lift logChan
  logSettings = lift logSettings

-- | Modules use the Message monad
data Module = Module
  { moduleName :: String
  , runModule :: WithMessage ()
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

instance LogM HircM where
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
