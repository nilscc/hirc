{-# LANGUAGE LambdaCase #-}

module Hirc.Modules.Poker.Module where

import qualified Data.Map as M

import Hirc
import Hirc.Modules.Poker.Bank
import Hirc.Modules.Poker.Game

data PokerState = PokerState
  { games :: M.Map ChannelName Game
  , bank :: Bank
  }

data PokerModule = PokerModule

type PokerM a = ModuleMessageM PokerModule a

initPokerState :: HircM PokerState
initPokerState = return PokerState
  { games = M.empty
  , bank = emptyBank -- TODO: Load from persistent storage
  }