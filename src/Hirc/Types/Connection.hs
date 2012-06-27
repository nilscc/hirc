module Hirc.Types.Connection where

import Data.Time
import Network
import Network.IRC

--------------------------------------------------------------------------------
-- Connection

data IrcServer = IrcServer
  { host        :: String
  , port        :: PortNumber
  , reconnects  :: Reconnect
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

