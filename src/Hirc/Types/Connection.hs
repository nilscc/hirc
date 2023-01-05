module Hirc.Types.Connection where

import Data.Time ( NominalDiffTime, UTCTime )
import Data.Text (Text)
import Network.Socket ( PortNumber )
import Network.IRC ( Message, Channel )

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
  | PrivMsg Nickname Text  -- ^ private message
  | Notice  Nickname Text
  | Join Channel
  | Part Channel
  | Nick Nickname
  | Ping
  | Pong
  | Quit (Maybe Text)
  deriving Show

