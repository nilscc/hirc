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

data Game = Game
  { players         :: [Player]
  , currentPosition :: Int

  , blinds          :: (Money, Money) -- small/big blind
  , sidePots        :: [([Player], Money)]

  , deck            :: [Card]
  , flop            :: Maybe (Card, Card, Card)
  , turn            :: Maybe Card
  , river           :: Maybe Card

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
  length (deck g) == 5 &&
  isNothing (flop g) &&
  isNothing (turn g) &&
  isNothing (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= sum (blinds g)

isFlop :: Game -> Bool
isFlop g =
  length (deck g) == 2 &&
  isJust (flop g) &&
  isNothing (turn g) &&
  isNothing (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))

isTurn :: Game -> Bool
isTurn g =
  length (deck g) == 1 &&
  isJust (flop g) &&
  isJust (turn g) &&
  isNothing (river g) &&
  all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
  totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))

isRiver :: Game -> Bool
isRiver g =
  null (deck g) &&
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