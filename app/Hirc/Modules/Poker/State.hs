module Hirc.Modules.Poker.State where

import Data.Map (Map)
import Control.Concurrent.STM
import Control.Monad.Reader

import Hirc
import Hirc.Modules.Poker.Game

newtype PokerState = PokerState
  { games :: Map ChannelName Game
  }

type PokerSTM = ReaderT (TVar PokerState, NickName, UserName, Maybe ChannelName) STM