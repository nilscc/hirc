{-# LANGUAGE DeriveAnyClass #-}

module Hirc.Modules.Poker.Exception where

import Control.Concurrent.STM
import Control.Monad.Reader as R
import Control.Exception (Exception)

import Hirc

data PokerException
  = GameNotAvailable
  | GameAlreadyStarted
  | NotInChannel
  | PlayerNotFound
  | InsufficientFunds
  | GameUpdateFailed
  deriving (Show, Exception, Eq)
