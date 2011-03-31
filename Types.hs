module Types where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.MState
import Control.Monad.Reader
import Control.Monad.Error
import Data.Time
import Network
import Network.IRC
import System.IO

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
  | Join Channel
  | Part Channel
  | Ping
  | Pong
  | Quit (Maybe String)
  deriving Show

-- The Managed monad

type Managed a = MState ManagedState IO a

data ManagedState = ManagedState
  { loggingM      :: Maybe (String -> Managed ())
  }

-- The Hirc monad

type Hirc a = MState HircState (ReaderT HircSettings (ErrorT HircError IO)) a

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
  -- , disconnectH     :: Hirc ()
  }

data HircState = HircState
  { connectedHandle :: Maybe Handle
  , loggingH        :: Maybe (String -> Hirc ())
  }
