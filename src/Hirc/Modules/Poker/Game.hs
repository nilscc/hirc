{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hirc.Modules.Poker.Game where

import Control.Exception
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import Data.Function (on)
import Data.List (filter, find, sortOn)
import Data.List.Extra (groupOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Ord (Down (Down))
import Data.Text.Internal.Fusion.Size qualified as M
import Hirc
import Hirc.Modules.Poker.Bank (Money)
import Hirc.Modules.Poker.Cards
import Hirc.Modules.Poker.Exception
import Hirc.Modules.Poker.Player (Player (..))
import Hirc.Modules.Poker.Pot (Pot (..))
import Hirc.Modules.Poker.Pot qualified as P
import System.Random (RandomGen, StdGen, split)
import System.Random.Shuffle (shuffle')

--------------------------------------------------------------------------------
-- Settings

startingMoney :: Money
startingMoney = 1000

bigBlind, smallBlind :: Money
bigBlind = 20
smallBlind = 10

--------------------------------------------------------------------------------

type Position = Int

type FlopCards = (Card, Card, Card)

type TurnCard = Card

type RiverCard = Card

data CommunityCards
  = PreFlop
  | Flop FlopCards
  | Turn (FlopCards, TurnCard)
  | River (FlopCards, TurnCard, RiverCard)
  deriving (Eq, Show)

data Game = Game
  { players :: [UserName],
    playerNicks :: Map UserName NickName,
    playerStacks :: Map UserName Money,
    playerHands :: Map UserName Hand,
    currentPosition :: Position,
    lastRaise :: Maybe ((Position, UserName), Money),
    blinds :: (Money, Money), -- small/big blind
    mainPot :: Pot,
    sidePots :: [Pot],
    deck :: [Card],
    communityCards :: CommunityCards,
    rndGen :: StdGen
  }
  deriving (Eq, Show)

data GameResult
  = Showdown [(Player, Rank)] Money (Rank, [Player])
  | LastManTakesItAll Player Money
  deriving (Show, Eq)

data GameUpdate
  = GameUpdated Game
  | GameEnded GameResult
  | GameUpdateFailed PokerException
  deriving (Show)

ok :: Game -> GameUpdate
ok = GameUpdated

failed :: PokerException -> GameUpdate
failed = GameUpdateFailed

onOK :: (Game -> GameUpdate) -> GameUpdate -> GameUpdate
onOK f (GameUpdated g) = f g
onOK _ gu = gu

newGame :: StdGen -> Game
newGame gen =
  Game
    { players = [],
      playerNicks = M.empty,
      playerStacks = M.empty,
      playerHands = M.empty,
      currentPosition = 0,
      lastRaise = Nothing,
      blinds = (smallBlind, bigBlind),
      mainPot = P.empty,
      sidePots = [],
      deck = shuffle' fullDeck (length fullDeck) g1,
      communityCards = PreFlop,
      rndGen = g2
    }
  where
    (g1, g2) = split gen

--------------------------------------------------------------------------------
-- Queries
--

isNewGame :: Game -> Bool
isNewGame g =
  PreFlop == communityCards g
    && M.null (playerHands g)
    && totalPotSize g == 0
    && null (sidePots g)
    && currentPosition g == 0

isPreFlop :: Game -> Bool
isPreFlop g
  | PreFlop <- communityCards g =
      all ((2 ==) . length . hCards) (playerHands g)
        && totalPotSize g >= sum (blinds g)
  | otherwise =
      False

isFlop :: Game -> Bool
isFlop g
  | Flop _ <- communityCards g =
      all ((2 ==) . length . hCards) (playerHands g)
        && totalPotSize g >= (snd (blinds g) * fromIntegral (length (players g)))
  | otherwise = False

isTurn :: Game -> Bool
isTurn g
  | Turn _ <- communityCards g =
      all ((2 ==) . length . hCards) (playerHands g)
        && totalPotSize g >= (snd (blinds g) * fromIntegral (length (players g)))
  | otherwise = False

isRiver :: Game -> Bool
isRiver g
  | River _ <- communityCards g =
      all ((2 ==) . length . hCards) (playerHands g)
        && totalPotSize g >= (snd (blinds g) * fromIntegral (length (players g)))
  | otherwise = False

isActiveGame :: Game -> Bool
isActiveGame g = isPreFlop g || isFlop g || isTurn g || isRiver g

--------------------------------------------------------------------------------
-- Player management
--

findPlayer :: UserName -> Game -> Maybe Player
findPlayer u g = do
  s <- M.lookup u (playerStacks g)
  n <- M.lookup u (playerNicks g)
  return
    Player
      { playerUsername = u,
        playerNickname = n,
        playerHand = M.lookup u (playerHands g),
        playerPot = fromMaybe 0 $ M.lookup u (potPlayers $ mainPot g),
        playerStack = s
      }

currentUserName :: Game -> UserName
currentUserName g = players g !! currentPosition g

currentPlayer :: Game -> Player
currentPlayer g =
  let u = players g !! currentPosition g
      ms = M.lookup u $ playerStacks g
      pot = M.lookup u . potPlayers $ mainPot g
      mh = M.lookup u $ playerHands g
      n = fromJust $ M.lookup u $ playerNicks g
   in Player
        { playerUsername = u,
          playerNickname = n,
          playerStack = fromMaybe 0 ms,
          playerHand = mh,
          playerPot = fromMaybe 0 pot
        }

updatePlayer :: UserName -> (Player -> Player) -> Game -> Game
updatePlayer u f g =
  let Just p = findPlayer u g
      Player {playerStack, playerPot, playerHand} = f p
   in g
        { playerHands = M.alter (const playerHand) u (playerHands g),
          mainPot = P.set u playerPot (mainPot g),
          playerStacks = M.insert u playerStack (playerStacks g)
        }

updateCurrentPlayer :: (Player -> Player) -> Game -> Game
updateCurrentPlayer f g = updatePlayer (currentUserName g) f g

playersByNickName :: Game -> [NickName]
playersByNickName g = map (fromJust . (`M.lookup` playerNicks g)) (players g)

--------------------------------------------------------------------------------
-- Start of game
--

newPlayer :: NickName -> UserName -> Money -> Player
newPlayer nick user balance =
  Player
    { playerUsername = user,
      playerNickname = nick,
      playerStack = balance,
      playerPot = 0,
      playerHand = Nothing
    }

joinPlayer :: Player -> Game -> GameUpdate
joinPlayer p g =
  ok
    g
      { players = players g ++ [u],
        playerNicks = M.insert u n (playerNicks g),
        playerStacks = M.insert u s (playerStacks g)
      }
  where
    u = playerUsername p
    n = playerNickname p
    s = playerStack p

partPlayer :: Player -> Game -> GameUpdate
partPlayer p g =
  ok
    g
      { players = filter (u /=) (players g),
        playerNicks = M.delete u (playerNicks g),
        playerStacks = M.delete u (playerStacks g)
      }
  where
    u = playerUsername p

dealCards :: Game -> GameUpdate
dealCards g
  | n < 2 = failed NotEnoughPlayers
  | otherwise =
      ok
        g
          { deck = deck',
            -- shuffle order of players
            players = shuffle' (players g) n g1,
            -- store hands
            playerHands = M.fromList (zip (players g) hands),
            -- store new random gen
            rndGen = g2
          }
  where
    -- split random generator
    (g1, g2) = split $ rndGen g
    -- number of players
    n = length (players g)
    -- the card stack to distribute among players
    (cards, deck') = splitAt (2 * n) (deck g)
    -- split cards into 2 rounds
    (c1, c2) = splitAt n cards
    -- the hands
    hands = zipWith (\a b -> Hand [a, b]) c1 c2

payBlinds :: Game -> GameUpdate
payBlinds g
  | (u1 : u2 : pls) <- players g =
      ok
        g
          { players = pls ++ [u1, u2],
            mainPot = P.put u2 bb $ P.put u1 sb $ mainPot g,
            playerStacks =
              M.adjust (subtract sb) u1
                . M.adjust (subtract bb) u2
                $ playerStacks g
          }
  | otherwise = failed NotEnoughPlayers
  where
    (sb, bb) = blinds g

--------------------------------------------------------------------------------
-- During game
--

mainPotHeight :: Game -> Money
mainPotHeight g = P.height (mainPot g)

mainPotSize :: Game -> Money
mainPotSize g = P.size (mainPot g)

sidePotsSize :: Game -> Money
sidePotsSize g = sum $ map P.size (sidePots g)

totalPotSize :: Game -> Money
totalPotSize g = mainPotSize g + sidePotsSize g

toCall :: Game -> Money
toCall g = P.toCall u (mainPot g) + sum (map (P.toCall u) (sidePots g))
  where
    u = currentUserName g

bet :: Money -> Game -> GameUpdate
bet m g
  | playerStack p < m =
      failed
        InsufficientFunds
          { need = m,
            have = playerStack p
          }
  | otherwise = ok $ updateCurrentPlayer `flip` g $ \p ->
      p
        { playerStack = playerStack p - m,
          playerPot = playerPot p + m
        }
  where
    p = currentPlayer g

-- | Increment current position to next player
incPosition :: Game -> Game
incPosition g =
  g
    { currentPosition = (currentPosition g + 1) `mod` length (players g)
    }

call :: Game -> GameUpdate
call g
  | tc == 0 = failed CheckInstead
  | otherwise = onOK (next . incPosition) $ bet tc g
  where
    tc = toCall g

check :: Game -> GameUpdate
check g
  | tc == 0 = next $ incPosition g
  | otherwise = failed $ CallFirst tc
  where
    tc = toCall g

raise :: Money -> Game -> GameUpdate
raise m g
  -- check if raise is below last raise
  | Just (_, lr) <- lastRaise g,
    m < lr =
      failed $ RaiseTooSmall lr
  -- check if raise is below big blind
  | (_, bb) <- blinds g,
    m < bb =
      failed $ RaiseTooSmall bb
  -- perform raise
  | otherwise =
      onOK (next . incPosition) $
        bet
          (tc + m)
          g
            { lastRaise = Just ((pos, usr), m)
            }
  where
    tc = toCall g
    pos = currentPosition g
    usr = playerUsername $ currentPlayer g

fold :: Game -> GameUpdate
fold g =
  if length players' <= 1
    then GameEnded $ endGame g'
    else next g'
  where
    -- remove current player from players list
    (xs, u : ys) = splitAt (currentPosition g) (players g)
    players' = xs ++ ys

    -- keep current position, but potentially move it back to 0 if the last player
    -- folded
    pos' = currentPosition g `mod` length players'

    -- There are four main steps to folding:
    --  * Update game with new player list
    --  * Update position (mod it by new number of players)
    --  * Move player pot into community pot.
    --  * Move position of last raise if necessary.
    g' =
      g
        { players = players',
          currentPosition = pos',
          mainPot = P.fold u (mainPot g),
          lastRaise = do
            -- check if last raise was *before* current raise and subtract 1 from its position
            ((pos, usr), plr) <- lastRaise g
            let pos' = if currentPosition g < pos then pos - 1 else pos
            return ((pos' `mod` length players', usr), plr)
        }

allIn :: Game -> GameUpdate
allIn g = undefined

--------------------------------------------------------------------------------
-- Game phases
--

-- | Check if next phase is to be started
next :: Game -> GameUpdate
next g
  | n <= 1 = GameEnded $ endGame g
  | otherwise = case lastRaise g of
      Nothing
        | 0 == currentPosition g,
          toCall g == 0 ->
            incPhase g
      Just ((lpos, lu), mon)
        | lpos == currentPosition g ->
            if lu == cu
              then incPhase g
              else ok g {lastRaise = Just ((lpos, cu), mon)}
      _ -> ok g
  where
    ps = players g
    n = length ps
    cu = playerUsername $ currentPlayer g

incPhase :: Game -> GameUpdate
incPhase g = case communityCards g of
  PreFlop -> ok showFlop
  Flop f -> ok $ showTurn f
  Turn (f, t) -> ok $ showRiver f t
  River _ -> GameEnded $ endGame g
  where
    -- always burn first card
    (_ : a : b : c : r) = deck g
    -- show next cards
    showFlop = g {deck = r, communityCards = Flop (a, b, c)}
    showTurn f = g {deck = b : c : r, communityCards = Turn (f, a)}
    showRiver f t = g {deck = b : c : r, communityCards = River (f, t, a)}

isNextPhase :: Game -> Bool
isNextPhase g
  | PreFlop <- communityCards g = False
  | otherwise = case (currentPosition g, lastRaise g) of
      (0, Nothing) -> True
      (p, Just ((l, u), _)) ->
        p == l && playerUsername (currentPlayer g) == u
      _ -> False

--------------------------------------------------------------------------------
-- End game
--

endGame :: Game -> GameResult
endGame g
  | [u] <- players g =
      LastManTakesItAll (fromJust $ findPlayer u g) (mainPotSize g)
  | otherwise =
      Showdown rankedPlayers (mainPotSize g) (snd $ head winners, map fst winners)
  where
    River ((f1, f2, f3), t, r) = communityCards g
    cc = [f1, f2, f3, t, r]
    bestHands = M.map (fromJust . (rank <=< (findBestHand . (cc ++) . hCards))) (playerHands g)
    ranked = sortOn (Down . snd) $ M.toList bestHands
    rankedPlayers = map (\(u, r) -> (fromJust (findPlayer u g), r)) ranked
    winners : _ = groupOn snd rankedPlayers