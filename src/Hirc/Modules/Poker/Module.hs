{-# LANGUAGE LambdaCase #-}

module Hirc.Modules.Poker.Module where

import qualified Data.Map as M

import Hirc
import Hirc.Modules.Poker.Bank
import Hirc.Modules.Poker.Game
import Control.Monad.Random (StdGen, getStdGen)

type GameState = Either (Game StdGen) GameResult

data PokerState = PokerState
  { games :: M.Map ChannelName GameState
  , bank :: Bank
  , stdGen :: StdGen
  }
  deriving (Show)

data PokerModule = PokerModule

type PokerM a = ModuleMessageM PokerModule a

initPokerState :: HircM PokerState
initPokerState = do
  sg <- getStdGen
  return PokerState
    { games = M.empty
    , bank = emptyBank -- TODO: Load from persistent storage
    , stdGen = sg
    }