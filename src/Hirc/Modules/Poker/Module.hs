{-# LANGUAGE ImportQualifiedPost #-}

module Hirc.Modules.Poker.Module where

import Control.Monad.Random (StdGen, getStdGen)
import Control.Monad.Trans (liftIO)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Hirc
import Hirc.Modules.Poker.Bank
import Hirc.Modules.Poker.Game

type GameState = Either Game GameResult

data PokerState = PokerState
  { games :: M.Map ChannelName GameState,
    bank :: Bank,
    stdGen :: StdGen
  }
  deriving (Show)

data PokerModule = PokerModule

type PokerM a = ModuleMessageM PokerModule a

initPokerState :: HircM PokerState
initPokerState = do
  b <- liftIO $ loadFromJson "bank.json"
  sg <- getStdGen
  return
    PokerState
      { games = M.empty,
        bank = fromMaybe emptyBank b,
        stdGen = sg
      }