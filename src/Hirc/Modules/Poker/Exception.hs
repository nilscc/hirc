{-# LANGUAGE DeriveAnyClass #-}

module Hirc.Modules.Poker.Exception where

import Control.Concurrent.STM
import Control.Monad.Reader as R
import Control.Exception (Exception)

import Hirc
import Hirc.Modules.Poker.Bank (Money)

data PokerException
  = GameNotAvailable
  | GameAlreadyStarted
  | NotInChannel
  | PlayerNotFound
  | InsufficientFunds { need :: Money, have :: Money }
  | RaiseTooSmall Money
  | NotEnoughPlayers
  | CheckInstead
  | CallFirst Money
  | WrongGameState
  deriving (Show, Exception, Eq)
