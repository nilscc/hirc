module Hirc.Modules.Poker.Game where

import Control.Exception
import Data.Maybe (isJust, isNothing)
import Data.List (find, filter, sortOn)
import System.Random (StdGen, RandomGen, split)

import Hirc
import Hirc.Modules.Poker.Exception
import Hirc.Modules.Poker.Cards
import Hirc.Modules.Poker.Bank (Money)
import Control.Monad.IO.Class (MonadIO)
import System.Random.Shuffle (shuffle')
import Data.List.Extra (groupOn)
import Data.Ord (Down(Down))

--------------------------------------------------------------------------------
-- Settings

startingMoney :: Money
startingMoney = 1000

bigBlind, smallBlind :: Money
bigBlind   = 20
smallBlind = 10

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

data Game = Game
  { players           :: [Player]
  , currentPosition   :: Position

  , lastRaise         :: Maybe ((Position, UserName), Money)

  , blinds            :: (Money, Money) -- small/big blind
  , pot               :: Money
  , sidePots          :: [([UserName], Money)]

  , deck              :: [Card]
  , communityCards    :: CommunityCards
  , rndGen            :: StdGen
  }
  deriving (Eq, Show)

data GameResult
  = Showdown [(Player, Rank)] Money (Rank, [Player])
  | LastManTakesItAll Player Money
  deriving (Show, Eq)

data GameUpdate
  = GameUpdated      Game
  | GameEnded        GameResult
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

isNewGame :: Game -> Bool
isNewGame g =
  PreFlop == communityCards g &&
  all (isNothing . playerHand) (players g) &&
  totalPotSize g == 0 &&
  null (sidePots g) &&
  currentPosition g == 0

isPreFlop :: Game -> Bool
isPreFlop g
  | PreFlop <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= sum (blinds g)
  | otherwise
  = False

isFlop :: Game -> Bool
isFlop g
  | Flop _ <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))
  | otherwise = False

isTurn :: Game -> Bool
isTurn g
  | Turn _ <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))
  | otherwise = False

isRiver :: Game -> Bool
isRiver g
  | River _ <- communityCards g
  = all (maybe False ((2 ==) . length . hCards) . playerHand) (players g) &&
    totalPotSize g >= (snd (blinds g) * fromIntegral(length (players g)))
  | otherwise = False

isActiveGame :: Game -> Bool
isActiveGame g = isPreFlop g || isFlop g || isTurn g || isRiver g

--------------------------------------------------------------------------------
-- Player management
--

findPlayer :: UserName -> Game -> Maybe Player
findPlayer u g =
  find ((u ==) . playerUsername) (players g)

currentPlayer :: Game -> Player
currentPlayer g = players g !! currentPosition g

updateCurrentPlayer :: (Player -> Player) -> Game -> Game
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

joinPlayer :: Player -> Game -> GameUpdate
joinPlayer p g = ok g
  { players = players g ++ [ p { playerPot = 0, playerHand = Nothing }]
  }

partPlayer :: Player -> Game -> GameUpdate
partPlayer p g = ok g
  { players = filter ((playerUsername p /=) . playerUsername) (players g)
  }

dealCards :: Game -> GameUpdate
dealCards g 
  | n < 2 = failed NotEnoughPlayers
  | otherwise = ok g
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

payBlinds :: Game -> GameUpdate
payBlinds g 
  | (p1:p2:pls) <- players g
  = ok $ g { players = pls ++ [bet p1 sb,bet p2 bb] }
  | otherwise = failed NotEnoughPlayers
 where
  (sb, bb) = blinds g
  bet p m = p { playerStack = playerStack p - m
              , playerPot = playerPot p + m }


--------------------------------------------------------------------------------
-- During game
--

mainPotHeight :: Game -> Money
mainPotHeight g = maximum $ map playerPot (players g)

mainPotSize :: Game -> Money
mainPotSize g = 
  let sumPlayerPots = sum (map playerPot (players g))
   in sumPlayerPots + pot g

totalPotSize :: Game -> Money
totalPotSize g =
  let sumSidePots = sum (map snd (sidePots g))
   in sumSidePots + mainPotSize g

toCall :: Game -> Money
toCall g = mainPotHeight g - playerPot p
 where
  p = currentPlayer g


bet :: Money -> Game -> GameUpdate
bet m g
  | playerStack p < m = failed InsufficientFunds
    { need = m
    , have = playerStack p
    }
  | otherwise = ok $ updateCurrentPlayer `flip` g $ \p -> p
    { playerStack = playerStack p - m
    , playerPot = playerPot p + m
    }
 where
  p = currentPlayer g

-- | Increment current position to next player
incPosition :: Game -> Game
incPosition g = g
  { currentPosition = (currentPosition g + 1) `mod` length (players g) }

call :: Game -> GameUpdate
call g
  | tc == 0   = failed CheckInstead
  | otherwise = onOK (next . incPosition) $ bet tc g
 where
  tc = toCall g

check :: Game -> GameUpdate
check g
  | tc == 0   = next $ incPosition g
  | otherwise = failed $ CallFirst tc
 where
  tc = toCall g

raise :: Money -> Game -> GameUpdate
raise m g
  -- check if raise is below last raise
  | Just (_,lr) <- lastRaise g
  , m < lr = failed $ RaiseTooSmall lr
  -- check if raise is below big blind
  | (_,bb) <- blinds g
  , m < bb = failed $ RaiseTooSmall bb
  -- perform raise
  | otherwise = onOK (next . incPosition) $ bet (tc + m) g
    { lastRaise = Just ((pos, usr), m)
    }
 where
  tc = toCall g
  pos = currentPosition g
  usr = playerUsername $ currentPlayer g

fold :: Game -> GameUpdate
fold g =
  if length players' <= 1 then
    GameEnded $ endGame g'
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
        | 0 == currentPosition g -> incPhase g
      Just ((lpos, lu), mon)
        | lpos == currentPosition g -> 
          if lu == cu then
            incPhase g
           else
            ok g { lastRaise = Just ((lpos, cu), mon ) }
      _ -> ok g
 where
  ps = players g
  n  = length ps
  cu = playerUsername $ currentPlayer g

incPhase :: Game -> GameUpdate
incPhase g = case communityCards g of
  PreFlop    -> ok showFlop
  Flop f     -> ok $ showTurn f
  Turn (f,t) -> ok $ showRiver f t
  River _    -> GameEnded $ endGame g
 where
  -- always burn first card
  (_:a:b:c:r) = deck g
  -- show next cards
  showFlop      = g { deck = r, communityCards = Flop (a,b,c) }
  showTurn  f   = g { deck = b:c:r, communityCards = Turn (f,a) } 
  showRiver f t = g { deck = b:c:r, communityCards = River (f,t,a) } 

isNextPhase :: Game -> Bool
isNextPhase g 
  | PreFlop <- communityCards g = False
  | otherwise = case (currentPosition g, lastRaise g) of
    (0, Nothing) -> True
    (p, Just ((l,u),_)) ->
      p == l && playerUsername (currentPlayer g) == u
    _ -> False

--------------------------------------------------------------------------------
-- End game
--

endGame :: Game -> GameResult
endGame g
  | [p] <- players g
  = LastManTakesItAll p{ playerPot = 0 } (mainPotSize g)
  | otherwise
  = Showdown ranked (mainPotSize g) (snd $ head winners, map fst winners)
 where
  River ((f1,f2,f3),t,r) = communityCards g
  cc = [f1,f2,f3,t,r]
  bestHand p =
    let Just r = rank =<< findBestHand . (cc ++) . hCards  =<< playerHand p
     in (p { playerPot = 0 },r)
  ranked = sortOn (Down . snd) . map bestHand $ players g
  winners:_ = groupOn snd ranked