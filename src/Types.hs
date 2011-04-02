{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.MState
import Control.Monad.IO.Peel
import Control.Monad.Reader
import Control.Monad.Error
import Data.Time
import Network
import Network.IRC
import System.IO


--------------------------------------------------------------------------------
-- Connections

data IrcServer = IrcServer
  { host        :: String
  , port        :: PortNumber
  , reconnects  :: Reconnect
  , channels    :: [String]
  }

data Reconnect = Reconnect
  { recTimes    :: Int
  , recCount    :: Int
  , recWait     :: NominalDiffTime
  , recLastTry  :: Maybe UTCTime
  }

type Nickname = String
type Username = String
type Realname = String

type To = Nickname

data ConnectionCommand
  = Send Message
  | PrivMsg To String  -- ^ private message
  | Notice To String
  | Join Channel
  | Part Channel
  | Ping
  | Pong
  | Quit (Maybe String)
  deriving Show


--------------------------------------------------------------------------------
-- The Managed monad

type Managed = MState ManagedState (ReaderT ManagedSettings IO)

data ManagedState = ManagedState

data ManagedSettings = ManagedSettings
  { logChanM      :: Chan (Int,String)
  , logSettingsM  :: LogSettings
  }


--------------------------------------------------------------------------------
-- The Hirc monad

type Hirc = MState HircState (ReaderT HircSettings (ErrorT HircError IO))

data HircError
  = H_NotConnected
  | H_ConnectionLost
  | H_ConnectionFailed
  | H_Other String
  deriving Show

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


--------------------------------------------------------------------------------
-- Message filters

type WithMessage = ReaderT Message Hirc

class MonadPeelIO m => Filtered m where
  runFiltered :: m () -> WithMessage ()

instance Filtered WithMessage where
  runFiltered = id

instance Filtered Hirc where
  runFiltered = lift
