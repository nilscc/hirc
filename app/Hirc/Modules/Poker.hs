{-# LANGUAGE ScopedTypeVariables, TypeFamilies, NamedFieldPuns, LambdaCase, DeriveAnyClass, TupleSections, RankNTypes #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Poker
  ( pokerModule
  ) where

import Data.Time.Clock (UTCTime)
import Control.Monad (mzero, join, forM_, unless, when)
import Control.Monad.State
import Control.Exception.Peel (handle, Exception, throw, SomeException (SomeException))
import qualified Control.Monad.Reader as R
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.List hiding (delete)
import System.Random

import Hirc
import Hirc.Modules.Poker.Cards
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar, throwSTM, catchSTM)
import GHC.Conc (TVar(TVar), STM (STM))
import Control.Monad.ST (runST)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Except (runExceptT, ExceptT)
import Data.Acid.TemplateHaskell (tyVarBndrName)

--------------------------------------------------------------------------------
-- Settings

type Money = Integer

startingMoney :: Money
startingMoney = 10000

bigBlind, smallBlind :: Money
bigBlind   = 200
smallBlind = 100

--------------------------------------------------------------------------------
-- The main module

data PokerModule = PokerModule

pokerModule :: Module
pokerModule = newModule PokerModule

newtype PokerState = PokerState
  { games :: Map ChannelName Game
  }

initPokerState :: HircM PokerState
initPokerState = return PokerState
  { games = M.empty
  }

instance IsModule PokerModule where
  type ModuleState PokerModule = PokerState
  moduleName _ = "Poker"
  initModule _ = initPokerState
  shutdownModule _ = Nothing
  onStartup _ = Nothing
  onMessage _ = Just runPokerModule

type PokerM a = ModuleMessageM PokerModule a

runPokerModule :: PokerM ()
runPokerModule = handle pokerExceptions $ do

  -- handle nick changes of players
  onCommand "NICK" $ doneAfter updatePlayers

  --
  -- START DEBUG
  --

  userCommand $ \"random" -> do
    sg <- askStdGen
    g <- runSTM $ do
      randomizeDeck sg
      askGame
    logM 2 $ show (deck g)

  userCommand $ \"deck" -> do
    g <- runSTM askGame
    logM 2 $ show (deck g)

  userCommand $ \"next" -> doneAfter $ do
    runSTM nextPlayer
    showCurrentPlayer

  userCommand $ \"game" -> doneAfter $ do
    mg <- runSTM askGame
    logM 2 $ show mg

  --
  -- END DEBUG
  --

  userCommand $ \"poker" "help"     -> doneAfter showHelp

  userCommand $ \"players"          -> doneAfter showPlayers
  userCommand $ \"pl"               -> doneAfter showPlayers

  {-
  userCommand $ \"money"            -> doneAfter showMoney
  userCommand $ \"mo"               -> doneAfter showMoney
  -}

  inState NewGame $ do
    userCommand $ \"poker" "join"   -> doneAfter addPlayer
    userCommand $ \"poker" "quit"   -> doneAfter removePlayer

    userCommand $ \"deal" "cards"   -> doneAfter deal
    userCommand $ \"deal"           -> doneAfter deal
    userCommand $ \"dc"             -> doneAfter deal

  inStates [PreFlop] $ do
    userCommand $ \"turn"           -> doneAfter showCurrentPlayer
    userCommand $ \"tu"             -> doneAfter showCurrentPlayer

    userCommand $ \"order"          -> doneAfter showCurrentOrder
    userCommand $ \"or"             -> doneAfter showCurrentOrder

    userCommand $ \"hand"           -> doneAfter showHand


  {-
  userCommand $ \"cards"          -> doneAfter showCurrentCards
  userCommand $ \"ca"             -> doneAfter showCurrentCards

  userCommand $ \"check"          -> doneAfter check
  userCommand $ \"call"           -> doneAfter call
  userCommand $ \"raise" amount   -> doneAfter $ raise amount
  userCommand $ \"bet"   amount   -> doneAfter $ raise amount
  --userCommand $ \"fold" "now"     -> doneAfter fold'
  userCommand $ \"fold"           -> doneAfter fold
  userCommand $ \"all" "in"       -> doneAfter allIn
  -}
 where
  pokerExceptions :: SomeException -> PokerM ()
  pokerExceptions e = do
    msg <- getMessage
    logM 1 $ "Exception on message: " ++ show e ++ "\n" ++ show msg

--------------------------------------------------------------------------------
-- Types

data Player = Player
  { playerNickname :: NickName
  , playerUsername :: UserName
  , playerMoney :: Money
  , playerPot   :: Money
  , playerHand  :: Maybe Hand
  }
  deriving (Eq, Show)

data GameState
  = NewGame
  | PreFlop
  | GameEnd
  deriving (Eq, Show, Ord)

data Game = Game
  { players         :: [Player]
  , currentPosition :: Int
  , currentState    :: GameState

  , blinds          :: (Money, Money) -- small/big blind
  , pot             :: Money
  , sidePots        :: [([Player], Money)]

  , deck            :: [Card]

  --, chatMessages  :: [(UTCTime, Player, String)]
  --, inputBuffer   :: [Char]
  -- TODO: , inputMode     :: InputMode
  , stdGen        :: Maybe StdGen
  }
  deriving (Eq, Show)

newGame :: Game
newGame = Game
  { players = []
  , currentPosition = 0
  , currentState = NewGame
  , blinds = (smallBlind, bigBlind)
  , pot = 0
  , sidePots = []
  , deck = fullDeck
  , stdGen = Nothing
  }

askStdGen :: PokerM StdGen
askStdGen = do
  g <- runSTM askGame
  case stdGen g of
    Just sg -> return sg
    Nothing -> do
      sg <- liftIO initStdGen
      runSTM $ updateGame $ \g -> g { stdGen = Just sg }
      return sg

inState :: GameState -> PokerM () -> PokerM ()
inState s = inStates [s]

inStates :: [GameState] -> PokerM () -> PokerM ()
inStates s m = handle newGameRule $ do
  g <- runSTM askGame 
  when (currentState g `elem` s) m
 where
  newGameRule GameNotAvailable
    | NewGame `elem` s = m
  newGameRule _ = return ()


--------------------------------------------------------------------------------
-- Exceptions
--

data PokerException
  = GameNotAvailable
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

--------------------------------------------------------------------------------
-- Helper


type PokerSTM = ReaderT (TVar PokerState, NickName, UserName, Maybe ChannelName) STM

runSTM :: PokerSTM a -> PokerM a
runSTM rstm = do
  mchan <- getCurrentChannel
  withNickAndUser $ \n u -> do
    tvar <- R.ask
    liftIO $ atomically $ runReaderT rstm (tvar, n, u, mchan)

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

askGame :: PokerSTM Game
askGame = do
  (tvar,_,_,mchan) <- R.ask
  maybe (lift $ throwSTM NotInChannel) `flip` mchan $ \chan -> do
    pokerState <- lift $ readTVar tvar
    case M.lookup chan (games pokerState) of
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

findPlayer :: Game -> UserName -> Maybe Player
findPlayer g u =
  L.find ((u ==) . playerUsername) (players g)

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

askCurrentState :: PokerSTM GameState
askCurrentState = currentState <$> askGame

--------------------------------------------------------------------------------
-- Information

showColors :: PokerM ()
showColors = do
  let c1 = head fullDeck
      c2 = fullDeck !! 13
      c3 = fullDeck !! 26
      c4 = fullDeck !! 39
  answer $ unwords (map colorCard [c1,c2,c3,c4])

-- Help
showHelp :: PokerM ()
showHelp = do
  whisper "Playing Texas Hold'em, available commands are:"
  whisper "    <bot>: poker help         --   show this help"
  whisper "    <bot>: poker join         --   join a new game"
  whisper "    <bot>: poker leave        --   leave the current game"
  whisper "    mo[ney]                   --   show your current wealth"
  whisper "    pl[ayers]                 --   show who is playing in the next game"
  whisper "    deal [cards]              --   deal cards, start a new game"
  whisper "While playing:"
  whisper "    raise/bet <num>           --   bet a new sum (at least big blind or the amount of the last raise)"
  whisper "    check/call/fold           --   check, call the current bet or fold your cards"
  whisper "    all in                    --   go all in"
  whisper "    or[der]                   --   show current order"
  whisper "    ca[rds]                   --   show your own and all community cards"
  whisper "    tu[rn]                    --   show whose turn it currently is"

showPlayers :: PokerM ()
showPlayers = do
  logM 1 "Test"
  g <- runSTM askGame
  if null (players g) then
    say "No players yet."
   else
    say $ "Current players: " ++ intercalate ", " (map playerNickname (players g))

addPlayer :: PokerM ()
addPlayer = (logM 2 "addPlayer" >>) . join . runSTM $ do

  (ig,newGame) <- ((, return ()) <$> userInGame) `catchP` \GameNotAvailable -> do
    updateGame' $ \Nothing -> Just newGame
    return (False, logM 2 "New game created.")

  if ig then
    return $ do
      newGame
      answer "You're already in the game!"
   else do
    setState NewGame
    n <- askNick
    u <- askUser
    updateGame $ addPlayer' Player
      { playerNickname = n
      , playerUsername = u
      , playerMoney = startingMoney -- TODO: load money from bank
      , playerPot = 0
      , playerHand = Nothing
      }
    return $ do
      newGame
      say $ "Player \"" ++ n ++ "\" joins the game."
 where
  addPlayer' p g = g { players = players g ++ [p] }

removePlayer :: PokerM ()
removePlayer = handle playerNotFound $ do
  join . runSTM $ do
    p <- askPlayer
    if isNothing (playerHand p) then do
      updateGame $ \g -> g
        { players = L.filter
            ((playerUsername p /=) . playerUsername)
            (players g)
        }
      return $ say ("\"" ++ playerNickname p ++ "\" left the game.")
     else
      return $ answer "You have to fold first."
 where
  playerNotFound PlayerNotFound = done

updatePlayers :: PokerM ()
updatePlayers = withParams $ \[newNick] -> runSTM $ do

  u <- askUser
  let changeNick p@Player{ playerUsername }
        | u == playerUsername = p { playerNickname = newNick }
        | otherwise = p
      updateNick g = g { players = map changeNick (players g) }

  -- change all players in all games
  updatePokerState $ \pokerState -> 
    pokerState { games = M.map updateNick (games pokerState) }


{-
showMoney :: MessageM ()
showMoney = withUsername $ \u -> do
  wealth <- loadMoney u
  answer $ "Your current wealth is " ++ show wealth ++ "."

loadMoney :: UserName -> MessageM Integer
loadMoney u = do
  mmo <- loadGlobal "money"
  case mmo of
       Just mo | Just wealth <- lookupMap u mo ->
         return wealth
       _ -> do
         -- add the starting money if the current player has no money at all
         updateGlobal "money" $ \mmo' ->
           case mmo' of
               Just mo
                 | memberMap u mo -> mo
                 | otherwise ->
                   insertMap u startingMoney mo
               Nothing ->
                   singletonMap u startingMoney
         return startingMoney
-}

askCurrentOrder :: PokerSTM [Player]
askCurrentOrder = toOrder <$> askGame
 where
  toOrder Game{ currentPosition, players } =
    let (a,b) = L.splitAt currentPosition players
     in b ++ a

askFirstPosition :: PokerSTM Player
askFirstPosition = (!! 0) . players <$> askGame

{-
getCurrentOrder :: MessageM (Maybe [(NickName, UserName, Integer, Integer)])
getCurrentOrder = do
  mo <- load "order"
  case mo of
       Just o | Just order <- fromList o -> do
         nicks <- fmap (map fromJust)                  $ mapM getPlayerNickname (order)
         mons  <- fmap (map $ fromMaybe startingMoney) $ mapM getPlayerWealth   (order)
         pot   <- fmap (map $ fromMaybe 0)             $ mapM getPlayerPot      (order)
         return $ Just $ zip4 nicks order mons pot
       _ -> return Nothing

getPlayerNickname :: UserName -> MessageM (Maybe NickName)
getPlayerNickname u = do
  mps <- load "players"
  return $
    case mps of
         Just ps -> lookupMap u ps
         _       -> Nothing

getPlayerWealth :: UserName -> MessageM (Maybe Integer)
getPlayerWealth u = do
  loadGlobal "money" ~> u

getPlayerPot :: UserName -> MessageM (Maybe Integer)
getPlayerPot u = do
  load "pot" ~> u

getPotMax :: MessageM (Maybe Integer)
getPotMax = do
  fmap getMax `fmap` load "pot"
 where
  getMax = maximum . elemsMap

getAmountToCall :: UserName -> MessageM Integer
getAmountToCall u = do
  mp <- getPlayerPot u
  mx <- getPotMax
  return $ fromMaybe 0 mx - fromMaybe 0 mp

showCurrentOrder :: MessageM ()
showCurrentOrder = do
  mo <- getCurrentOrder
  mfp <- load "first position"
  let formatNames (n,u,w,p) =
        (if Just u == mfp then "*" else "")  -- first player indication
        ++ n  -- nick
        ++ " (" ++ show p ++ "/" ++ show w ++ ")"  -- (current pot/total wealth)

  case mo of
       Just nicks -> say $ "Currently playing: " ++ intercalate ", " (map formatNames nicks)
       Nothing    -> say "Cannot figure out current order – sorry!"
 where

showCurrentPlayer :: MessageM ()
showCurrentPlayer = do
  cur <- getCurrentPlayer
  s   <- load "state"
  case cur of
       Just (n,u) -> do
         mp <- load "pot"
         let p  = fromMaybe (singletonMap "" (0 :: Integer)) mp
             mx, up, tc :: Integer
             mx = maximum (elemsMap p)
             up = fromMaybe mx (lookupMap u p)
             tc = mx - up
         say $ "It's " ++ n ++ "s turn" ++ if tc > 0 then " (" ++ show tc ++ " to call)." else "."
       Nothing | s == Nothing || s == Just "end" ->
         say "No one is playing at the moment."
       Nothing ->
         endGame
-}

showCurrentPlayer :: PokerM ()
showCurrentPlayer = do
  (s, p, pot) <- runSTM $ (,,)
    <$> askCurrentState
    <*> askCurrentPlayer
    <*> askMaxPot

  let tc = pot - playerPot p

  if s `elem` [NewGame, GameEnd] then do
    n <- getNickname
    answer $ "No game in progress. Join a game by typing \"" ++ n ++ ": poker join\", then start it via \"deal cards\"."
   else do
    -- calculate 
    say $ "Current player is: " ++ playerNickname p ++ (if tc > 0 then " (" ++ show tc ++ " to call)" else "")
 where
  askMaxPot = maximum . map playerPot <$> askPlayers


showCurrentOrder :: PokerM ()
showCurrentOrder = do
  (order, fpos) <- runSTM $ (,) <$> askCurrentOrder <*> askFirstPosition

  let formatNames p =
         -- first player indication
        (if p == fpos then "*" else "")
         -- nick
        ++ playerNickname p
         -- (current pot/total wealth)
        ++ " (" ++ show (playerPot p) ++ "/" ++ show (playerMoney p) ++ ")"

  say $ "Currently playing: " ++ intercalate ", " (map formatNames order)

showHand :: PokerM ()
showHand = do
  p <- runSTM askPlayer
  showHand' p

showHand' :: Player -> PokerM ()
showHand' p
  | Just (Hand hand) <- playerHand p =
    whisperTo (playerNickname p) $
      "Your hand: " ++ unwords (map colorCard hand)
  | otherwise =
    withNickAndUser $ \n u ->
      logM 1 $ "No hand found: " ++ show p ++ " (" ++ n ++ " / " ++ u ++ ")"

{-
showCurrentCards :: MessageM ()
showCurrentCards = withUsername $ \u -> do
  showCommunityCards
  showPlayerCards u

showPlayerCards :: UserName -> MessageM ()
showPlayerCards u = do
  mn <- getPlayerNickname u
  mh <- getPlayerCards u
  case (mn, mh) of
       (Just n, Just hand) -> sendNotice n $ "Your hand: " ++ intercalate " " (map colorCard hand)
       _                   -> return ()

showCommunityCards :: MessageM ()
showCommunityCards = do
  ccs <- getCommunityCards
  case ccs of
       (Just (c1,c2,c3), Nothing, Nothing) ->
         say $ "Flop cards: " ++ intercalate " " (map colorCard [c1,c2,c3])
       (Just (c1,c2,c3), Just tc, Nothing) ->
         say $ "Turn cards: " ++ intercalate " " (map colorCard [c1,c2,c3]) ++ "  " ++ colorCard tc
       (Just (c1,c2,c3), Just tc, Just rc) ->
         say $ "River cards: " ++ intercalate " " (map colorCard [c1,c2,c3]) ++ "  " ++ colorCard tc ++ "  " ++ colorCard rc
       _ ->
         say "There are no community cards yet."

getPlayerCards :: UserName -> MessageM (Maybe [Card])
getPlayerCards u = do
  mr <- load "hands" ~> u
  return $
    case mr of
         Just (fromList -> Just l) -> fmap sort $ sequence $ map toCard l
         _ -> Nothing

getCommunityCards :: MessageM (Maybe (Card,Card,Card), Maybe Card, Maybe Card)
getCommunityCards = do
  -- load flop cards
  mfres <- load "flop"
  --Just (Just fl) <- load "flop"
  --let Just fcs = fromList fl
  let fcs = do fres       <- mfres
               fl         <- fres
               [s1,s2,s3] <- fromList fl
               c1         <- toCard s1
               c2         <- toCard s2
               c3         <- toCard s3
               return (c1,c2,c3)
  -- load turn card
  mtres <- load "turn"
  let tc = do tres <- mtres
              ts   <- tres
              toCard ts
  -- load river card
  mrres <- load "river"
  let rc = do rres <- mrres
              rs   <- rres
              toCard rs
  return (fcs, tc, rc)
  

--------------------------------------------------------------------------------
-- Setting up the game environment

{ -
startGame :: MessageM ()
startGame = withNickAndUser $ \n u -> do
  state <- load "state"
  if (state == Nothing || state == Just "end") then do
    answerTo $ \who ->
      case who of
           Left  _ -> answer "Sorry, but this game needs to be run in a public channel."
           Right _ -> do
             store "state" "new game"
             store "game started by" u
             hirc <- getNickname
             say $ "Poker game started by " ++ n ++ ". Say \"" ++ hirc ++ ": join poker\" to join this game."
             addPlayer n u
   else do
    answer "Poker game already in progress. You can play only one game per channel."
-}



--------------------------------------------------------------------------------
-- Play

deal :: PokerM ()
deal = do
  sg <- askStdGen
  join . runSTM $ do
    pls <- askPlayers
    if length pls < 2 then
      return $ say "Cannot start a game of poker with less than 2 players."
     else do
      setState PreFlop
      randomizeOrder sg
      randomizeDeck sg
      dealCards
      blinds <- payBlinds
      pls' <- askPlayers
      return $ do
        say $ "Starting a new round! The players are: " ++ intercalate ", " (map playerNickname pls')
        say "Dealing hands…"
        notifyHands
        blinds
        showCurrentPlayer

-- Send NOTICE to all players with their hands
notifyHands :: PokerM ()
notifyHands = do
  g <- runSTM askGame
  forM_ (players g) showHand'

setState :: GameState -> PokerSTM ()
setState s = updateGame $ \g -> g { currentState = s}

dealCards :: PokerSTM ()
dealCards = updateGame $ \g ->
  let -- number of players
      n = length (players g)
      -- the card stack to distribute among players
      (cards,deck') = L.splitAt (3*n) (deck g)
      -- split cards into 3 rounds
      (c1,cs) = L.splitAt n cards
      (c2,c3) = L.splitAt n cs
      -- the hands
      hands = L.zipWith3 (\a b c -> Hand [a,b,c]) c1 c2 c3

   in g { deck = deck'
        , players = [ p { playerHand = Just h } | (p,h) <- zip (players g) hands ]
        }

payBlinds :: PokerSTM (PokerM ())
payBlinds = do

  -- lookup small and big blinds
  g <- askGame
  let (sb,bb) = blinds g

  -- pay small blind
  p1 <- askCurrentPlayer
  bet p1 sb
  nextPlayer

  -- pay big blind
  p2 <- askCurrentPlayer
  bet p2 bb
  nextPlayer

  -- done
  return $ do
    say $ playerNickname p1 ++ " pays " ++ show sb ++ " (small blind)."
    say $ playerNickname p2 ++ " pays " ++ show bb ++ " (big blind)."

bet :: Player -> Money -> PokerSTM ()
bet p m = do
  g <- askGame

  -- check if player has enough money
  case findPlayer g (playerUsername p) of
    Just p
      | playerMoney p >= m -> putPlayer p
        { playerMoney = playerMoney p - m
        , playerPot = playerPot p + m
        }
      | otherwise -> throwP InsufficientFunds
    _ -> throwP PlayerNotFound


  {-
  s <- load "state"
  when (s == Nothing || s == Just "end") $ do
    mpls <- load "players"
    case mpls of
        Just pls | sizeMap pls >= 2 -> do
          randomizeOrder pls
          Just nicks <- getCurrentOrder
          say $ "Starting a new round! The players are: " ++ intercalate ", " (map fst4 nicks)
          say "Dealing hands…"
          dealCards pls
          store "state" "preflop"
          payBlinds
          nextPlayer
          Just (_,cp) <- getCurrentPlayer
          store "round started by" cp
          showCurrentPlayer
        _ -> say "Cannot start a game of poker with less than 2 players."
  -}

randomizeOrder :: StdGen -> PokerSTM ()
randomizeOrder sg = updateGame $ \g ->
  g { players = shuffle sg (players g) }

randomizeDeck :: StdGen -> PokerSTM ()
randomizeDeck sg = updateGame $ \g ->
  g { deck = shuffle sg (deck g) }

{-
dealCards :: Map -> MessageM ()
dealCards ps = do
  cards <- shuffle fullDeck
  let cc = 3*sizeMap ps -- cards count
  forM_ [0..cc-1] $ \i -> do -- iterate over 3 * #players
    let card = cards !! i
    Just (_n,u) <- getCurrentPlayer
    update "hands" $ \mh ->
      case mh of
           Just allHands -> alterMap (add card) u allHands
           Nothing       -> singletonMap u (singletonList (show card))
    nextPlayer
  store "cards" (toList $ map show $ drop cc cards)
  forM_ (keysMap ps) showPlayerCards
 where
  add card (Just playerHands) = Just $ appendList playerHands (singletonList (show card))
  add card Nothing            = Just $ singletonList (show card)

payBlinds :: MessageM ()
payBlinds = require "state" "preflop" $ do
  Just (n,u) <- getCurrentPlayer
  -- store "firs" position
  store "first position" u
  -- pay small blind
  bet u smallBlind
  say $ n ++ " pays " ++ show smallBlind ++ " (small blind)."
  -- pay big blind
  nextPlayer
  Just (n',u') <- getCurrentPlayer
  bet u' bigBlind
  say $ n' ++ " pays " ++ show bigBlind ++ " (big blind)."
  store "last raise" ((Nothing :: Maybe UserName), bigBlind)

bet :: UserName -> Money -> MessageM ()
bet u m = do
  -- update total money of player
  updateGlobal "money" $ \mmo ->
    case mmo of
         Just mo -> alterMap pay u mo
         Nothing -> singletonMap u (startingMoney - m)
  -- update current user pot
  update "pot" $ \mp ->
    case mp of
         Just p  -> alterMap inc u p
         Nothing -> singletonMap u m
 where
  pay Nothing  = Just $ startingMoney - m
  pay (Just c) = Just $ c - m
  inc Nothing  = Just m
  inc (Just c) = Just $ c + m

check :: MessageM ()
check = requireCurrentPlayer $
  withNickAndUser $ \n u -> do
    tc <- getAmountToCall u
    if tc == 0 then do
       say $ n ++ " checks."
       nextPlayer
       showCurrentPlayer
     else
       answer "You can't do that!"

call :: MessageM ()
call = requireCurrentPlayer $
  withNickAndUser $ \n u -> do
    pw <- fromMaybe 0 `fmap` getPlayerWealth u
    tc <- getAmountToCall u
    if tc > 0 && pw >= tc then do
       say $ n ++ " calls " ++ show tc ++ "."
       bet u tc
       nextPlayer
       showCurrentPlayer
     else if pw < tc then
       answer $ "You don't have enough money to do that."
     else
       answer "You can't do that."

raise :: String -> MessageM ()
raise (readsafe -> Just r) = requireCurrentPlayer $
  withNickAndUser $ \n u -> do
    pw <- fromMaybe 0 `fmap` getPlayerWealth u
    tc <- getAmountToCall u
    let tot = tc + r
    mx <- fromMaybe 0 `fmap` getPotMax
    lr <- maybe 0 (snd :: (Maybe UserName, Integer) -> Integer) `fmap` load "last raise"
    if pw >= tot && r >= lr then do
       say $ n ++ " raises the pot by " ++ show r ++ " to a total of " ++ show (mx+r) ++ "."
       bet u tot
       store "last raise" (Just u,r)
       nextPlayer
       showCurrentPlayer
     else if r < lr then
       answer $ "You must bet at least " ++ show lr ++ "."
     else
       answer $ "You don't have enough money to do that."
raise _ = requireCurrentPlayer $
  answer $ "Invalid raise."

fold :: MessageM ()
fold = requireCurrentPlayer $ do
  fold'
  nextPlayer

fold' :: MessageM ()
fold' = do
  withNickAndUser $ \n u -> do
    say $ n ++ " folds."
    update "order" $
      maybe emptyMap (deleteMap u)
    showCurrentPlayer

allIn :: MessageM ()
allIn = undefined

endGame :: MessageM ()
endGame = do
  say "Ending game."
  store "state" "end"
  delete "flop"
  delete "turn"
  delete "river"
  delete "hands"
  delete "cards"
  delete "last raise"
  delete "pot"
  delete "order"
  done

requireCurrentPlayer :: MessageM () -> MessageM ()
requireCurrentPlayer m = withUsername $ \u -> do
  mc <- getCurrentPlayer
  case mc of
       Just (_,u') | u == u' -> m
       _                     -> answer "It's not your turn."


--------------------------------------------------------------------------------
-- Organizing players & cards

getCurrentPlayer :: MessageM (Maybe (NickName, UserName))
getCurrentPlayer = do
  mo <- load "order"
  mp <- load "players"
  return $
    case (mo, mp) of
         (Just o, Just p)
           | Just u <- headList o
           , Just n <- lookupMap u p ->
             Just (n,u)
         _ -> Nothing
-}

nextPlayer :: PokerSTM ()
nextPlayer = do
  -- TODO
  -- increment position
  updateGame $ \g -> g
    { currentPosition = (currentPosition g + 1) `mod` length (players g)
    }

{-
nextPlayer :: MessageM ()
nextPlayer = do
  mo' <- load "order"
  if fmap lengthList mo' <= Just 1 then
     endGame
   else do
     update "order" $ \mo ->
       case mo of
           Just l | Just (f :: UserName) <- headList l ->
                appendList (tailList l) (singletonList f)
           _ -> emptyList -- shouldn't happen anyway
     mc  <- getCurrentPlayer
     mlr <- load "last raise" :: MessageM (Maybe (Maybe UserName, Money))
     mgs <- load "round started by"
     let playerIsLastRaise    = fmap fst mlr == Just (fmap snd mc)
         playerHasStartedGame = Nothing == join (fmap fst mlr) && mgs == fmap snd mc
     when (playerIsLastRaise || playerHasStartedGame) nextPhase

nextPhase :: MessageM ()
nextPhase = do
  s <- load "state"
  case s of
       Just "preflop" -> flop
       Just "flop"    -> turn
       Just "turn"    -> river
       Just "river"   -> showdown


--------------------------------------------------------------------------------
-- Phase transitions

burnCard :: MessageM ()
burnCard = update "cards" $ maybe emptyList tailList

takeCard :: MessageM Card
takeCard = do
  Just (Just (Just c)) <- fmap (fmap toCard . headList) `fmap` load "cards"
  update "cards" $ maybe emptyList tailList
  return c

-- start round with first position player
startFromFirstPosition :: MessageM ()
startFromFirstPosition = do
  Just (fp :: UserName) <- load "first position"
  store "round started by" fp
  let skip = do Just (_,cp) <- getCurrentPlayer
                unless (cp == fp) (nextPlayer >> skip)
  skip
  update "last raise" $ \(Just (_,lr)) ->
    ((Nothing :: Maybe UserName), (lr :: Integer))

flop :: MessageM ()
flop = require "state" "preflop" $ do
  say "First betting round ends."
  store "state" "flop"
  burnCard
  c1 <- takeCard
  c2 <- takeCard
  c3 <- takeCard
  let fcs = map show [c1,c2,c3]
  store "flop" $ Just (toList fcs)
  showCommunityCards
  startFromFirstPosition

turn :: MessageM ()
turn = require "state" "flop" $ do
  say "Second betting round ends."
  store "state" "turn"
  -- pick turn card
  burnCard
  tc <- takeCard
  let tcs = show tc
  store "turn" $ (Just tcs)
  -- show cards
  showCommunityCards
  startFromFirstPosition

river :: MessageM ()
river = require "state" "turn" $ do
  say "Third betting round ends. This is the last betting round!"
  store "state" "river"
  -- pick river card
  burnCard
  rc <- takeCard
  let rcs = show rc
  store "river" $ (Just rcs)
  -- show cards
  showCommunityCards
  startFromFirstPosition

showdown :: MessageM ()
showdown = require "state" "river" $ do
  say "Showdown!"
  startFromFirstPosition
  endGame

-}
--------------------------------------------------------------------------------
-- Helper functions

shuffle :: StdGen -> [a] -> [a]
shuffle sg ls = do
  execState `flip` ls $
    forM_ [1..length ls-1] $ \i -> do
      let j = randomRs (0,i) sg !! i
      modify $ swap j i
 where
  swap j i l =
    let lj = l !! j
        li = l !! i
     in replace i lj (replace j li l)
  replace n y xs = take n xs ++ [y] ++ drop (n+1) xs