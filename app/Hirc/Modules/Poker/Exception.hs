{-# LANGUAGE DeriveAnyClass #-}

module Hirc.Modules.Poker.Exception where

import Control.Concurrent.STM
import Control.Monad.Reader as R
import Control.Exception (Exception)

import Hirc
import Hirc.Modules.Poker.State

data PokerException
  = GameNotAvailable
  | GameAlreadyStarted
  | NotInChannel
  | PlayerNotFound
  | InsufficientFunds
  deriving (Show, Exception, Eq)

throwP :: PokerException -> PokerSTM a
throwP = lift . throwSTM

catchP :: PokerSTM a -> (PokerException -> PokerSTM a) -> PokerSTM a
catchP m h = do
  r <- R.ask
  lift $ runReaderT m r `catchSTM` \e -> runReaderT (h e) r

handleP :: (PokerException -> PokerSTM a) -> PokerSTM a -> PokerSTM a
handleP = flip catchP

handlePE :: PokerException -> (PokerException -> PokerSTM a) -> PokerSTM a -> PokerSTM a
handlePE e = handlePEs [e]

handlePEs :: [PokerException] -> (PokerException -> PokerSTM a) -> PokerSTM a -> PokerSTM a
handlePEs es h = handleP $ \e' -> if e' `elem` es then h e' else throwP e'

ignore :: [PokerException] -> PokerSTM () -> PokerSTM ()
ignore = ignoreConst ()

ignoreConst :: a -> [PokerException] -> PokerSTM a -> PokerSTM a
ignoreConst a es = handleP $ \e -> if e `elem` es then return a else throwP e