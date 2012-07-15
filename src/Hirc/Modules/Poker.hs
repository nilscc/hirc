{-# LANGUAGE ScopedTypeVariables, PatternGuards, ViewPatterns #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Poker
  ( pokerModule
  ) where

import Control.Monad.State
import Data.Maybe
import Data.List hiding (delete)
import System.Random

import Hirc
import Hirc.Modules.Poker.Cards

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

pokerModule :: Module
pokerModule = Module "Poker" (Just updatePlayers) $ do

  userCommand $ \"color" ->
    let c1 = fullDeck !! 0
        c2 = fullDeck !! 13
        c3 = fullDeck !! 26
        c4 = fullDeck !! 39
     in answer $ intercalate " " (map colorCard [c1,c2,c3,c4])

  onValidPrefix $ do

    userCommand $ \"poker" "help"  -> showHelp

    userCommand $ \"poker" "join"  -> acceptPlayers
    userCommand $ \"poker" "start" -> acceptPlayers

    userCommand $ \"poker" "quit"  -> quitGame
    userCommand $ \"poker" "leave" -> quitGame

  userCommand $ \"players"        -> showPlayers        >> done
  userCommand $ \"pl"             -> showPlayers        >> done

  userCommand $ \"turn"           -> showCurrentPlayer  >> done
  userCommand $ \"tu"             -> showCurrentPlayer  >> done

  userCommand $ \"money"          -> showMoney          >> done
  userCommand $ \"mo"             -> showMoney          >> done

  userCommand $ \"order"          -> showCurrentOrder   >> done
  userCommand $ \"or"             -> showCurrentOrder   >> done

  userCommand $ \"cards"          -> showCurrentCards   >> done
  userCommand $ \"ca"             -> showCurrentCards   >> done

  userCommand $ \"deal" "cards"   -> deal               >> done
  userCommand $ \"deal"           -> deal               >> done
  --userCommand $ \"dc"             -> deal               >> done

  userCommand $ \"check"          -> check              >> done
  userCommand $ \"call"           -> call               >> done
  userCommand $ \"raise" amount   -> raise amount       >> done
  userCommand $ \"bet"   amount   -> raise amount       >> done
  --userCommand $ \"fold" "now"     -> fold'              >> done
  userCommand $ \"fold"           -> fold               >> done
  userCommand $ \"all" "in"       -> allIn              >> done

-- On nickchange: update player names
updatePlayers :: Username -> Nickname -> MessageM ()
updatePlayers un nn = do
  updateAll "players" $ \_chan m ->
    if memberMap un m
       then insertMap un nn m
       else m

--------------------------------------------------------------------------------
-- Types

-- data Player = Player
--   { playerName  :: String
--   , playerMoney :: Int
--   , playerPot   :: Int
--   , playerHand  :: Maybe Hand
--   }
--   deriving (Eq, Show)
-- 
-- data Game = Game
--   { players       :: [Player]
--   , player        :: Player
--   , currentPlayer :: Player
--   , history       :: [Action]
--   , blinds        :: (Int, Int) -- small/big blind
--   , currentState  :: GameState
--   , pot           :: Int
--   , sidePots      :: [([Player], Int)]
--   , deck          :: [Card]
--   , chatMessages  :: [(UTCTime, Player, String)]
--   , inputBuffer   :: [Char]
--   , inputMode     :: InputMode
--   }
--   deriving (Eq, Show)

--type Poker a = MonadPeelIO m => MState Game m a


--------------------------------------------------------------------------------
-- Information

-- Help
showHelp :: MessageM ()
showHelp = do
  whisper "Playing Texas Hold'em, available commands are:"
  whisper "    <bot>: poker help         --   show this help"
  whisper "    <bot>: poker join/start   --   join a new game"
  whisper "    <bot>: poker leave/quit   --   leave the current game"
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

showPlayers :: MessageM ()
showPlayers = do
  s <- load "state"
  if s == Nothing || s == Just "end" then do
     mps <- load "players"
     case mps of
         Just pls | not (nullMap pls) ->
               say $ "Currently playing poker: " ++ intercalate ", " (elemsMap pls)
         _ -> say "There is noone playing poker at the moment!"
   else
     showCurrentOrder

showMoney :: MessageM ()
showMoney = withUsername $ \u -> do
  wealth <- loadMoney u
  answer $ "Your current wealth is " ++ show wealth ++ "."

loadMoney :: Username -> MessageM Integer
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

getCurrentOrder :: MessageM (Maybe [(Nickname, Username, Integer, Integer)])
getCurrentOrder = do
  mo <- load "order"
  case mo of
       Just o | Just order <- fromList o -> do
         nicks <- fmap (map fromJust)                  $ mapM getPlayerNickname (order)
         mons  <- fmap (map $ fromMaybe startingMoney) $ mapM getPlayerWealth   (order)
         pot   <- fmap (map $ fromMaybe 0)             $ mapM getPlayerPot      (order)
         return $ Just $ zip4 nicks order mons pot
       _ -> return Nothing

getPlayerNickname :: Username -> MessageM (Maybe Nickname)
getPlayerNickname u = do
  mps <- load "players"
  return $
    case mps of
         Just ps -> lookupMap u ps
         _       -> Nothing

getPlayerWealth :: Username -> MessageM (Maybe Integer)
getPlayerWealth u = do
  loadGlobal "money" ~> u

getPlayerPot :: Username -> MessageM (Maybe Integer)
getPlayerPot u = do
  load "pot" ~> u

getPotMax :: MessageM (Maybe Integer)
getPotMax = do
  fmap getMax `fmap` load "pot"
 where
  getMax = maximum . elemsMap

getAmountToCall :: Username -> MessageM Integer
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

showCurrentCards :: MessageM ()
showCurrentCards = withUsername $ \u -> do
  showCommunityCards
  showPlayerCards u

showPlayerCards :: Username -> MessageM ()
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

getPlayerCards :: Username -> MessageM (Maybe [Card])
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

{-
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

acceptPlayers :: MessageM ()
acceptPlayers = withNickAndUser addPlayer

addPlayer :: Nickname -> Username -> MessageM ()
addPlayer n u = do
  mps <- load "players"
  if maybe False (memberMap u) mps then
     answer "You're already in the game!"
   else if (fmap sizeMap mps >= Just 15) then
     answer "Sorry, there are already too many players in the game!"
   else do
     update "players" $ \ms ->
       case ms of
            Nothing -> singletonMap u n
            Just m  -> insertMap u n m
     say $ "Player \"" ++ n ++ "\" joins the game."

quitGame :: MessageM ()
quitGame = withNickAndUser $ \n u -> do
  s <- load "state"
  if s == Nothing || s == Just "end" then do
     update "players" $ \mm ->
       case mm of
            Just m  -> deleteMap u m
            Nothing -> emptyMap
     say $ "" ++ n ++ " left the game!"
   else
     answer $ "You have to fold first."


--------------------------------------------------------------------------------
-- Play

deal :: MessageM ()
deal = do
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

randomizeOrder :: Map -> MessageM ()
randomizeOrder pls = do
  let usernames = keysMap pls
  order <- shuffle usernames
  store "order" (toList order)
  store "current player" (head order)

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
  store "last raise" ((Nothing :: Maybe Username), bigBlind)

bet :: Username -> Money -> MessageM ()
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
    lr <- maybe 0 (snd :: (Maybe Username, Integer) -> Integer) `fmap` load "last raise"
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

getCurrentPlayer :: MessageM (Maybe (Nickname, Username))
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

nextPlayer :: MessageM ()
nextPlayer = do
  mo' <- load "order"
  if fmap lengthList mo' <= Just 1 then
     endGame
   else do
     update "order" $ \mo ->
       case mo of
           Just l | Just (f :: Username) <- headList l ->
                appendList (tailList l) (singletonList f)
           _ -> emptyList -- shouldn't happen anyway
     mc  <- getCurrentPlayer
     mlr <- load "last raise" :: MessageM (Maybe (Maybe Username, Money))
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
  Just (fp :: Username) <- load "first position"
  store "round started by" fp
  let skip = do Just (_,cp) <- getCurrentPlayer
                unless (cp == fp) (nextPlayer >> skip)
  skip
  update "last raise" $ \(Just (_,lr)) ->
    ((Nothing :: Maybe Username), (lr :: Integer))

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

--------------------------------------------------------------------------------
-- Helper functions

shuffle :: MonadIO m => [a] -> m [a]
shuffle ls = do
  sg <- liftIO newStdGen
  execStateT `flip` ls $
    forM_ [1..length ls-1] $ \i -> do
      let j = randomRs (0,i) sg !! i
      modify $ swap j i
 where
  swap j i l =
    let lj = l !! j
        li = l !! i
     in replace i lj (replace j li l)
  replace n y xs = take n xs ++ [y] ++ drop (n+1) xs

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

readsafe :: Read a => String -> Maybe a
readsafe s =
  case reads s of
       [(x,"")] -> Just x
       _        -> Nothing
