module Types where

import Control.Concurrent.STM
import Data.Time
import Network

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

newtype ManagedServer = ManagedServer (IrcServer, TMVar ())
