module Hirc.Modules.Poker.Game where

import Control.Exception
import Data.Maybe (isJust, isNothing)
import Data.List (find, filter)
import System.Random (StdGen, RandomGen, split)

import Hirc
import Hirc.Modules.Poker.Exception
import Hirc.Modules.Poker.Cards
import Hirc.Modules.Poker.Bank (Money)
import Control.Monad.IO.Class (MonadIO)
import System.Random.Shuffle (shuffle')

--------------------------------------------------------------------------------
-- Settings

startingMoney :: Money
startingMoney = 10000

bigBlind, smallBlind :: Money
bigBlind   = 200
smallBlind = 100

--------------------------------------------------------------------------------

data Player = Player
  { playerNickname :: NickName
  , playerUsername :: UserName
  , playerStack :: Money
  , playerPot   :: Money
  , playerHand  :: Maybe Hand
  }
  deriving (Eq, Show)

type Position = Int

type FlopCards = (Card, Card, Card)
type TurnCard = Card
type RiverCard = Card

data CommunityCards
  = PreFlop
  | Flop  FlopCards
  | Turn  (FlopCards, TurnCard)
  | River (FlopCards, TurnCard, RiverCard)
  deriving (Eq, Show)

data Game g = Game
  { players           :: [Player]
  , currentPosition   :: Position

  , lastRaise         :: Maybe ((Position, UserName), Money)

  , blinds            :: (Money, Money) -- small/big blind
  , pot               :: Money
  , sidePots          :: [([UserName], Money)]

  , deck              :: [Card]
  , communityCards    :: CommunityCards
  , rndGen            :: g
  }
  deriving (Eq, Show)

newGame :: RandomGen g => g -> Game g
newGame gen = Game
  { players = []
  , currentPosition = 0
  , lastRaise = Nothing
  , blinds = (smallBlind, bigBlind)
  , pot = 0
  , sidePots = []
  , deck = shuffle' fullDeck (length fullDeck) g1
  , communityCards = PreFlop
  , rndGen = g2
  }
 where
  (g1,g2) = split gen


--------------------------------------------------------------------------------
-- Queries
--

isNewGame :: Game g -> Bool
isNewGame g =
  PreFlop == communityCards g &&
  all (isNothing . playerHand) (players g) &&
  totalPotSize g == 0 &&
  null (sidePots g) &&
  currentPosition g == 0

isPreFlop :: Game g -> Bool
isPreFlop g
  | PreFlop <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= sum (blinds g)
  | otherwise
  = False

isFlop :: Game g -> Bool
isFlop g
  | Flop _ <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))
  | otherwise = False

isTurn :: Game g -> Bool
isTurn g
  | Turn _ <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))
  | otherwise = False

isRiver :: Game g -> Bool
isRiver g
  | River _ <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))
  | otherwise = False

isActiveGame :: Game g -> Bool
isActiveGame g = isPreFlop g || isFlop g || isTurn g || isRiver g

--------------------------------------------------------------------------------
-- Player management
--

findPlayer :: UserName -> Game g -> Maybe Player
findPlayer u g =
  find ((u ==) . playerUsername) (players g)

updateCurrentPlayer :: (Player -> Player) -> Game g -> Game g
updateCurrentPlayer f g =
  let (l, p:r) = splitAt (currentPosition g) (players g)
   in g { players = l ++ f p : r }


--------------------------------------------------------------------------------
-- Start of game
--

newPlayer :: NickName -> UserName -> Money -> Player
newPlayer nick user balance = Player
  { playerNickname = nick
  , playerUsername = user
  , playerStack = balance
  , playerPot = 0
  , playerHand = Nothing
  }

joinPlayer :: Player -> Game g -> Game g
joinPlayer p g = g
  { players = players g ++ [ p { playerPot = 0, playerHand = Nothing }]
  }

partPlayer :: Player -> Game g -> Game g
partPlayer p g = g
  { players = filter ((playerUsername p /=) . playerUsername) (players g)
  }

dealCards :: RandomGen g => Game g -> Game g
dealCards g = g
  { deck = deck'
  -- shuffle order of players
  , players = shuffle' players' n g1
  -- store new random gen
  , rndGen = g2
  }
 where
  -- split random generator
  (g1,g2) = split $ rndGen g
  -- number of players
  n = length (players g)
  -- the card stack to distribute among players
  (cards,deck') = splitAt (2*n) (deck g)
  -- split cards into 2 rounds
  (c1,c2) = splitAt n cards
  -- the hands
  hands = zipWith (\a b -> Hand [a,b]) c1 c2
  -- give each player their hand
  players' = [ p { playerHand = Just h } | (p,h) <- zip (players g) hands ]

payBlinds :: Game g -> Game g
payBlinds g =
  let (p1:p2:pls) = players g
      (sb,bb) = blinds g
   in g { players = pls ++ [bet p1 sb,bet p2 bb]
        }
 where
  bet p m = p { playerStack = playerStack p - m
              , playerPot = playerPot p + m }


--------------------------------------------------------------------------------
-- During game
--

mainPotHeight :: Game g -> Money
mainPotHeight g = maximum $ map playerPot (players g)

totalPotSize :: Game g -> Money
totalPotSize g =
  let sumPlayerPots = sum (map playerPot (players g))
      sumSidePots   = sum (map snd (sidePots g))
   in sumPlayerPots + sumSidePots + pot g

toCall :: Game g -> Money
toCall g = mainPotHeight g - playerPot p
 where
  p = players g !! currentPosition g


bet :: Money -> Game g -> Game g
bet m = updateCurrentPlayer $ \p ->
  if playerStack p < m then
    throw InsufficientFunds
   else
    p { playerStack = playerStack p - m
      , playerPot = playerPot p + m
      }

newtype GameResult = GameResult
  { pots :: [(Money, [Player])]
  }
  deriving (Show, Eq)

type GameUpdate g = Either (Game g) GameResult

-- | Increment current position to next player
incPosition :: Game g -> Game g
incPosition g = g
  { currentPosition = (currentPosition g + 1) `mod` length (players g) }

call :: Game g -> GameUpdate g
call g
  | toCall g == 0 = throw GameUpdateFailed
  | otherwise     = next . incPosition $ bet (toCall g) g

check :: Game g -> GameUpdate g
check g
  | toCall g == 0 = next $ incPosition g
  | otherwise     = throw GameUpdateFailed

raise :: Money -> Game g -> GameUpdate g
raise m g = next . incPosition $ g'
  { lastRaise = Just ((pos, usr), m)
  }
 where
  g' = bet (toCall g + m) g
  pos = currentPosition g
  usr = playerUsername $ players g !! pos

fold :: Game g -> GameUpdate g
fold g =
  if length players' <= 1 then
    Right $ endGame g'
   else
    next g'
 where
  -- remove current player from players list
  (xs,p:ys) = splitAt (currentPosition g) (players g)
  players' = xs ++ ys

  -- keep current position, but potentially move it back to 0 if the last player
  -- folded
  pos' = currentPosition g `mod` length players'

  -- There are four main steps to folding:
  --  * Update game with new player list
  --  * Update position (mod it by new number of players)
  --  * Move player pot into community pot.
  --  * Move position of last raise if necessary.
  g' = g
    { players = players'
    , currentPosition = pos'
    , pot = pot g + playerPot p
    , lastRaise = do
        -- check if last raise was *before* current raise and subtract 1 from its position
        ((pos,usr),plr) <- lastRaise g
        let pos' = if currentPosition g < pos then pos - 1 else pos
        return ((pos' `mod` length players', usr), plr)
    }

allIn :: Game g -> GameUpdate g
allIn g = undefined

--------------------------------------------------------------------------------
-- Game phases
--

-- | Check if next phase is to be started
next :: Game g -> GameUpdate g
next g
  | n <= 1 = Right $ endGame g
  | otherwise = case lastRaise g of
      Nothing
        | 0 == currentPosition g -> incPhase g
      Just ((lpos, lu), mon)
        | lpos == currentPosition g -> 
          if lu == cu then
            incPhase g
           else
            Left g { lastRaise = Just ((lpos, cu), mon ) }
      _ -> Left g
 where
  ps = players g
  n  = length ps
  cu = playerUsername $ ps !! currentPosition g

endGame :: Game g -> GameResult
endGame _ = GameResult { pots = [] }

incPhase :: Game g -> GameUpdate g
incPhase g = case communityCards g of
  PreFlop    -> Left showFlop
  Flop f     -> Left $ showTurn f
  Turn (f,t) -> Left $ showRiver f t
  River _    -> Right $ endGame g
 where
  -- always burn first card
  (_:a:b:c:r) = deck g
  -- show next cards
  showFlop      = g { deck = r, communityCards = Flop (a,b,c) }
  showTurn  f   = g { deck = b:c:r, communityCards = Turn (f,a) } 
  showRiver f t = g { deck = b:c:r, communityCards = River (f,t,a) } 


--------------------------------------------------------------------------------
-- LEGACY UPDATES: TODO DELETE
--

{-# DEPRECATED showFlop  "Use check/call/raise/fold/allIn instead" #-}
{-# DEPRECATED showTurn  "Use check/call/raise/fold/allIn instead" #-}
{-# DEPRECATED showRiver "Use check/call/raise/fold/allIn instead" #-}

showFlop :: Game g -> Game g
showFlop g
  -- always "burn" the first card
  | (_:a:b:c:dck) <- deck g
  , PreFlop       <- communityCards g
  = g { deck = dck, communityCards = Flop (a,b,c) }
  | otherwise = throw GameUpdateFailed

showTurn :: Game g -> Game g
showTurn g
  -- always "burn" the first card
  | (_:t:dck) <- deck g
  , Flop fc   <- communityCards g
  = g { deck = dck, communityCards = Turn (fc, t) }
  | otherwise = throw GameUpdateFailed

showRiver :: Game g -> Game g
showRiver g
  -- always "burn" the first card
  | (_:r:dck)   <- deck g
  , Turn (fc,t) <- communityCards g
  = g { deck = dck, communityCards = River (fc, t, r) }
  | otherwise = throw GameUpdateFailed