{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Hirc.Modules.Poker.STM where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R

import Hirc
import Hirc.Modules.Poker.Game
import Hirc.Modules.Poker.Module
import Hirc.Modules.Poker.Exception


--------------------------------------------------------------------------------
-- Main type definition
--

type PokerSTM = ReaderT (TVar PokerState, NickName, UserName, Maybe ChannelName) STM


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
-- Ask/Update/Put
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

-- | Update game if exists, or create a new game for current channel if none
-- have been started before.
updateGame :: (Game -> Game) -> PokerSTM ()
updateGame f = updateGame' $ \case
  Nothing -> Just $ f newGame
  Just g -> Just $ f g

updateGame' :: (Maybe Game -> Maybe Game) -> PokerSTM ()
updateGame' f = do
  mc <- askChan
  case mc of
    Just chan -> updatePokerState $ \pokerState ->
      pokerState { games = M.alter f chan (games pokerState) }
    _ -> lift $ throwSTM NotInChannel

askPokerState :: PokerSTM PokerState
askPokerState = do
  (tvar,_,_,_) <- R.ask
  lift $ readTVar tvar

updatePokerState :: (PokerState -> PokerState) -> PokerSTM ()
updatePokerState f = do
  (tvar,_,_,_) <- R.ask
  lift $ modifyTVar tvar f

askMaybeGame :: PokerSTM (Maybe Game)
askMaybeGame = do
  (tvar,_,_,mchan) <- R.ask
  maybe (lift $ throwSTM NotInChannel) `flip` mchan $ \chan -> do
    pokerState <- lift $ readTVar tvar
    return $ M.lookup chan (games pokerState)

askGame :: PokerSTM Game
askGame = do
  mg <- askMaybeGame
  case mg of
    Just g -> return g
    Nothing -> lift $ throwSTM GameNotAvailable

putGame :: Game -> PokerSTM ()
putGame g = updateGame' $ const (Just g)

askPlayer :: PokerSTM Player
askPlayer = do
  u <- askUser
  g <- askGame
  maybe (lift $ throwSTM PlayerNotFound) return $
    findPlayer g u

updatePlayer :: UserName -> (Player -> Player) -> PokerSTM ()
updatePlayer u f = do
  g <- askGame
  unless (isJust $ findPlayer g u) $
    throwP PlayerNotFound
  putGame g
    { players = map (\p -> if playerUsername p == u then f p else p) (players g)
    }

putPlayer :: Player -> PokerSTM ()
putPlayer p = updatePlayer (playerUsername p) (const p)

userInGame :: PokerSTM Bool
userInGame = handleP (return False `const`) $ True <$ askPlayer

askPlayers :: PokerSTM [Player]
askPlayers = ignoreConst [] [GameNotAvailable] $
  players <$> askGame

askCurrentPlayer :: PokerSTM Player
askCurrentPlayer = do
  g <- askGame
  return $ players g !! currentPosition g

askCurrentPot :: PokerSTM Money
askCurrentPot = maximum . map playerPot <$> askPlayers

askToCall :: PokerSTM Money
askToCall = do
  pl <- askPlayer
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

  -- check if player has enough money
  case findPlayer g (playerUsername p) of
    Just p
      | playerMoney p >= m -> do
        putPlayer p
          { playerMoney = playerMoney p - m
          , playerPot = playerPot p + m
          }
      | otherwise -> throwP InsufficientFunds
    _ -> throwP PlayerNotFound