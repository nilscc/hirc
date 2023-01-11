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

type NickName = String
type UserName = String
type RealName = String
type ChannelName = String

data ConnectionCommand
  = Send Message
  | PrivMsg NickName Text  -- ^ private message
  | Notice  NickName Text
  | Join ChannelName
  | Part ChannelName
  | Nick NickName
  | Ping
  | Pong [String]
  | Quit (Maybe Text)
  deriving Show

