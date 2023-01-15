module Hirc.Modules.Poker.Game where

import Data.Maybe (isJust, isNothing)
import Data.List (find)
import System.Random (StdGen)

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

data Player = Player
  { playerNickname :: NickName
  , playerUsername :: UserName
  , playerMoney :: Money
  , playerPot   :: Money
  , playerHand  :: Maybe Hand
  }
  deriving (Eq, Show)

type Position = Int

data Game = Game
  { players           :: [Player]
  , currentPosition   :: Position

  , lastRaise         :: Maybe (Position, Money)

  , blinds            :: (Money, Money) -- small/big blind
  , sidePots          :: [([Player], Money)]

  , deck              :: [Card]
  , flop              :: Maybe (Card, Card, Card)
  , turn              :: Maybe Card
  , river             :: Maybe Card

  --, chatMessages    :: [(UTCTime, Player, String)]
  --, inputBuffer     :: [Char]
  -- TODO: , inputMode     :: InputMode
  , stdGen        :: Maybe StdGen
  }
  deriving (Eq, Show)

newGame :: Game
newGame = Game
  { players = []
  , currentPosition = 0
  , lastRaise = Nothing
  , blinds = (smallBlind, bigBlind)
  , sidePots = []
  , deck = []
  , flop = Nothing
  , turn = Nothing
  , river = Nothing
  , stdGen = Nothing
  }

potHeight :: Game -> Money
potHeight g = maximum (map playerPot (players g))

totalPotSize :: Game -> Money
totalPotSize g =
  let sumPlayerPots = sum (map playerPot (players g))
      sumSidePots   = sum (map snd (sidePots g))
   in sumPlayerPots + sumSidePots

isNewGame :: Game -> Bool
isNewGame g =
  null (deck g) &&
  isNothing (flop g) &&
  isNothing (turn g) &&
  isNothing (river g) &&
  all (isNothing . playerHand) (players g) &&
  totalPotSize g == 0 &&
  null (sidePots g) &&
  currentPosition g == 0

isPreFlop :: Game -> Bool
isPreFlop g =
  isNothing (flop g) &&
  isNothing (turn g) &&
  isNothing (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= sum (blinds g)

isFlop :: Game -> Bool
isFlop g =
  isJust (flop g) &&
  isNothing (turn g) &&
  isNothing (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))

isTurn :: Game -> Bool
isTurn g =
  isJust (flop g) &&
  isJust (turn g) &&
  isNothing (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))

isRiver :: Game -> Bool
isRiver g =
  isJust (flop g) &&
  isJust (turn g) &&
  isJust (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))

isActiveGame :: Game -> Bool
isActiveGame g = isPreFlop g || isFlop g || isTurn g || isRiver g

findPlayer :: Game -> UserName -> Maybe Player
findPlayer g u =
  find ((u ==) . playerUsername) (players g)

dealCards :: Game -> Game
dealCards g = 
  let -- number of players
      n = length (players g)
      -- the card stack to distribute among players
      (cards,deck') = splitAt (2*n) (deck g)
      -- split cards into 2 rounds
      (c1,c2) = splitAt n cards
      -- the hands
      hands = zipWith (\a b -> Hand [a,b]) c1 c2
   in g { deck = deck'
        , players = [ p { playerHand = Just h } | (p,h) <- zip (players g) hands ]
        }

burnCard :: Game -> Game
burnCard g = g { deck = tail (deck g) }

showFlop :: Game -> Game
showFlop = showF . burnCard
 where
  showF g
    | (a:b:c:_) <- deck g
    , Nothing <- flop g
    , Nothing <- turn g
    , Nothing <- river g = g
      { deck = drop 3 (deck g)
      , flop = Just (a,b,c)
      }
    | otherwise = g

showTurn :: Game -> Game
showTurn = showT . burnCard
 where
  showT g
    | (a:_) <- deck g
    , Just _ <- flop g
    , Nothing <- turn g
    , Nothing <- river g = g
      { deck = tail (deck g)
      , turn = Just a
      }
    | otherwise = g

showRiver :: Game -> Game
showRiver = showR . burnCard
 where
  showR g
    | (a:_) <- deck g
    , Just _ <- flop g
    , Just _ <- turn g
    , Nothing <- river g = g
      { deck = tail (deck g)
      , river = Just a
      }
    | otherwise = g

