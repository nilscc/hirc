{-# LANGUAGE DeriveAnyClass #-}

module Hirc.Modules.Poker.Exception where

import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad.Reader as R
import Hirc
import Hirc.Modules.Poker.Bank (Money)

data PokerException
  = GameNotAvailable
  | GameAlreadyStarted
  | NotInChannel
  | PlayerNotFound
  | InsufficientFunds {need :: Money, have :: Money}
  | RaiseTooSmall Money
  | NotEnoughPlayers
  | CallFirst Money
  | WrongGameState
  deriving (Show, Exception, Eq)
