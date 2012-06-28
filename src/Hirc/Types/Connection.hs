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

data ConnectionCommand
  = Send Message
  | PrivMsg Nickname String  -- ^ private message
  | Notice Nickname String
  | Join Channel
  | Part Channel
  | Nick Nickname
  | Ping
  | Pong
  | Quit (Maybe String)
  deriving Show

