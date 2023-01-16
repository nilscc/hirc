{-# LANGUAGE LambdaCase #-}

module Hirc.Modules.Poker.Module where

import qualified Data.Map as M

import Hirc
import Hirc.Modules.Poker.Game

newtype PokerState = PokerState
  { games :: M.Map ChannelName Game
  }

data PokerModule = PokerModule

type PokerM a = ModuleMessageM PokerModule a

initPokerState :: HircM PokerState
initPokerState = return PokerState
  { games = M.empty
  }