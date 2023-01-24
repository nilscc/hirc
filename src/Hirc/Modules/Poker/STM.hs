{-# LANGUAGE NamedFieldPuns #-}

module Hirc.Modules.Poker.STM where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import Control.Concurrent.STM
    ( TVar, STM, catchSTM, readTVar, retry, throwSTM, modifyTVar )
import qualified Control.Concurrent.STM as STM
import Control.Monad (unless, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R

import Hirc
import Hirc.Modules.Poker.Game
import Hirc.Modules.Poker.Module
import Hirc.Modules.Poker.Exception
import Hirc.Modules.Poker.Bank (Bank, Money)
import Control.Monad.Random (StdGen, RandomGen (split))


--------------------------------------------------------------------------------
-- Main type definition
--

type PokerSTM = ReaderT (TVar PokerState, NickName, UserName, Maybe ChannelName) STM


--------------------------------------------------------------------------------
-- Lifted STM operations

orElse :: PokerSTM a -> PokerSTM a -> PokerSTM a
orElse a b = do
  r <- R.ask
  lift $ STM.orElse (runReaderT a r) (runReaderT b r)

checkSTM :: Bool -> PokerSTM ()
checkSTM = lift . STM.check

--------------------------------------------------------------------------------
-- STM Exception handling
--

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


--------------------------------------------------------------------------------
-- Bank
--

askBank :: PokerSTM Bank
askBank = bank <$> askPokerState

updateBank :: (Bank -> Bank) -> PokerSTM ()
updateBank f = updatePokerState $ \pokerState -> pokerState
  { bank = f (bank pokerState)
  }

putBank :: Bank -> PokerSTM ()
putBank b = updatePokerState $ \ps -> ps { bank = b }

--------------------------------------------------------------------------------
-- Poker state
--

askNick :: PokerSTM NickName
askNick = do
  (_,n,_,_) <- R.ask
  return n

askUser :: PokerSTM UserName
askUser = do
  (_,_,u,_) <- R.ask
  return u

askChan :: PokerSTM (Maybe ChannelName)
askChan = do
  (_,_,_,mc) <- R.ask
  return mc

requireChan :: PokerSTM ChannelName
requireChan = do
  mc <- askChan
  maybe (lift retry) return mc

askPokerState :: PokerSTM PokerState
askPokerState = do
  (tvar,_,_,_) <- R.ask
  lift $ readTVar tvar

updatePokerState :: (PokerState -> PokerState) -> PokerSTM ()
updatePokerState f = do
  (tvar,_,_,_) <- R.ask
  lift $ modifyTVar tvar f


--------------------------------------------------------------------------------
-- Game state
--

-- | Update game if exists, or create a new game for current channel if none
-- have been started before.
updateGame' :: (Game StdGen -> Maybe (Game StdGen)) -> PokerSTM ()
updateGame' f = do
  chan <- requireChan
  updatePokerState $ \pokerState ->
    if M.member chan (games pokerState) then
      pokerState { games = M.update f chan (games pokerState) }
     else
      let (g1,g2) = split $ stdGen pokerState
      in case f (newGame g1) of
        Just g -> pokerState
          { games = M.insert chan g (games pokerState)
          , stdGen = g2
          }
        Nothing -> pokerState

updateGame :: (Game StdGen -> Game StdGen) -> PokerSTM ()
updateGame f = updateGame' $ Just . f

putGame :: Game StdGen -> PokerSTM ()
putGame g = do
  chan <- requireChan
  updatePokerState $ \ps -> ps
    { games = M.insert chan g (games ps) }

askMaybeGame :: PokerSTM (Maybe (Game StdGen))
askMaybeGame = do
  ps <- askPokerState
  ch <- requireChan
  return $ M.lookup ch (games ps)

askGame :: PokerSTM (Game StdGen)
askGame = maybe (lift retry) return =<< askMaybeGame


--------------------------------------------------------------------------------
-- Players
--

askPlayer :: PokerSTM Player
askPlayer = do
  u <- askUser
  g <- askGame
  maybe (lift retry) return $ findPlayer u g

updatePlayer :: UserName -> (Player -> Player) -> PokerSTM ()
updatePlayer u f = do
  g <- askGame
  unless (isJust $ findPlayer u g) $
    throwP PlayerNotFound
  putGame g
    { players = map (\p -> if playerUsername p == u then f p else p) (players g)
    }

putPlayer :: Player -> PokerSTM ()
putPlayer p = updatePlayer (playerUsername p) (const p)

userInGame :: PokerSTM Bool
userInGame = (True <$ askPlayer) `orElse` return False

askPlayers :: PokerSTM [Player]
askPlayers = players <$> askGame

askCurrentPosition :: PokerSTM Position
askCurrentPosition = currentPosition <$> askGame

askCurrentPlayer :: PokerSTM Player
askCurrentPlayer = do
  g <- askGame
  return $ players g !! currentPosition g

isCurrentPlayer :: PokerSTM Bool
isCurrentPlayer = do
  p <- askPlayer
  cp <- askCurrentPlayer
  return $ p == cp

askCurrentPot :: PokerSTM Money
askCurrentPot = maximum . map playerPot <$> askPlayers

askLastRaise :: PokerSTM (Maybe (Position, Money))
askLastRaise = lastRaise <$> askGame

askToCall :: Player -> PokerSTM Money
askToCall pl = do
  pot <- askCurrentPot
  return $ pot - playerPot pl

askCurrentOrder :: PokerSTM [Player]
askCurrentOrder = toOrder <$> askGame
 where
  toOrder Game{ currentPosition, players } =
    let (a,b) = L.splitAt currentPosition players
     in b ++ a

askFirstPosition :: PokerSTM Player
askFirstPosition = (!! 0) . players <$> askGame

bet :: Player -> Money -> PokerSTM ()
bet p m = do
  g <- askGame

  -- check if player has enough money.
  -- lookup player from game, as his money/pot might have be different
  case findPlayer (playerUsername p) g of
    Just p'
      | playerStack p' >= m -> do
        putPlayer p'
          { playerStack = playerStack p' - m
          , playerPot = playerPot p' + m
          }
      | otherwise -> throwP InsufficientFunds
    _ -> throwP PlayerNotFound