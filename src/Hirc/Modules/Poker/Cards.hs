module Hirc.Modules.Poker.Cards
    (
      -- * Ranking API
      rank
      -- * Input parsing
    , toHand
    , toCard
    , toValue
    , toSuite
    , colorCard
      -- * Data Types
    , Hand(..)
    , Rank(..)
    , Card(..)
    , Value(..)
    , Suite(..)
    , fullDeck
    )
  where


import Control.Applicative
import Control.Monad
import Data.Function
import Data.Maybe
import Data.List


--------------------------------------------------------------------------------
-- Data Types

data Suite = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord)

data Value = Number Int | Jack | Queen | King | Ace
  deriving (Eq, Ord)

data Card = Card
    { cValue :: Value
    , cSuite :: Suite
    }
  deriving (Eq, Ord)

newtype Hand = Hand
    { hCards :: [Card]
    }

data Rank
    = HighCard [Value]
    | Pair Value [Card]
    | TwoPairs Value Value Card
    | ThreeOfAKind Value
    | Straight Card
    | Flush Rank
    | FullHouse Value
    | FourOfAKind Value
    | StraightFlush Card
  deriving (Eq, Ord, Show)

instance Ord Hand where
    compare = compare `on` rank

instance Eq Hand where
    (==) = (==) `on` rank

--------------------------------------------------------------------------------
-- Stuff

fullDeck :: [Card]
fullDeck = [ Card v s | s <- [Clubs, Diamonds, Hearts, Spades]
                      , v <- map Number [2..10] ++ [Jack, Queen, King, Ace] ]

--------------------------------------------------------------------------------
-- Ranking algorithms

rank :: Hand -> Maybe Rank
rank h = let fs = [straightFlush, fourOfAKind, fullHouse, flush, straight, threeOfAKind, twoPairs, pair, highCard]
          in listToMaybe $ mapMaybe (\f -> f h) fs

straightFlush :: Hand -> Maybe Rank
straightFlush (Hand cs) = do
  guard $ notNull cs && sameSuite cs && consValues cs
  return $ StraightFlush (maximum cs)

fourOfAKind :: Hand -> Maybe Rank
fourOfAKind (Hand cs) = do
    (c:_) <- find ((==) 4 . length) $ sortAndGroup cs
    return $ FourOfAKind (cValue c)

fullHouse :: Hand -> Maybe Rank
fullHouse (Hand cs) = do
    (c:_) <- find ((==) 3 . length) $ sortAndGroup cs
    _ <- find ((==) 2 . length) $ sortAndGroup cs
    return $ FullHouse (cValue c)

flush :: Hand -> Maybe Rank
flush h@(Hand cs) = do
  guard (notNull cs && sameSuite cs)
  return $ Flush (fromJust $ highCard h)

straight :: Hand -> Maybe Rank
straight (Hand cs) = do
  guard (notNull cs && consValues cs)
  return $ Straight (maximum cs)

threeOfAKind :: Hand -> Maybe Rank
threeOfAKind (Hand cs) = do
    (c:_) <- find ((==) 3 . length) $ sortAndGroup cs
    return $ ThreeOfAKind (cValue c)

twoPairs :: Hand -> Maybe Rank
twoPairs h = do
    Pair v1 cs <- pair h
    Pair v2 [c] <- pair $ Hand cs
    return $ TwoPairs (max v1 v2) (min v1 v2) c

pair :: Hand -> Maybe Rank
pair (Hand cs) = do
    cs'@(c:_) <- find ((==) 2 . length) $ sortAndGroup cs
    return $ Pair (cValue c) (reverse $ sort $ filter (not . (`elem` cs')) cs)

highCard :: Hand -> Maybe Rank
highCard (Hand cs) = do
  guard $ notNull cs
  return $ HighCard $ reverse $ sort $ map cValue cs


sortAndGroup :: [Card] -> [[Card]]
sortAndGroup = groupBy ((==) `on` cValue) . sortBy (compare `on` cValue)

consValues :: [Card] -> Bool
consValues cs = fst $ foldl (\(a, pv) v -> (a && (next pv == v), v)) (True, head vs) (tail vs)
  where
    vs = sort $ map cValue cs
    next v = case v of
               Number i -> if i == 10 then Jack else Number (i + 1)
               Jack     -> Queen
               Queen    -> King
               King     -> Ace
               Ace      -> Number (-1)

sameSuite :: [Card] -> Bool
sameSuite cs = let ss = map cSuite cs in all ((==) $ head ss) ss

notNull :: [a] -> Bool
notNull = not . null


--------------------------------------------------------------------------------
-- Input

toSuite :: Char -> Maybe Suite
toSuite s = case s of
              'C' -> Just Clubs
              'D' -> Just Diamonds
              'H' -> Just Hearts
              'S' -> Just Spades
              _   -> Nothing

toValue :: String -> Maybe Value
toValue s = case s of
              "J" -> Just Jack
              "Q" -> Just Queen
              "K" -> Just King
              "A" -> Just Ace
              n   -> if n `elem` map show ([2..10] :: [Int])
                       then Just $ Number (read n)
                       else Nothing

toCard :: String -> Maybe Card
toCard "" = Nothing
toCard s = Card <$> toValue (init s) <*> toSuite (last s)

toHand :: String -> Maybe Hand
toHand s = Hand <$> if length cards == 5 then Just cards else Nothing
    where
        cards = (mapMaybe toCard . words) s


--------------------------------------------------------------------------------
-- Output

-- ♠️♣️♥️♦️
instance Show Suite where
    show s = case s of
               Clubs -> "C"
               Diamonds -> "D"
               Hearts -> "H"
               Spades -> "S"

showSuiteUnicode :: Suite -> String
showSuiteUnicode s = case s of
  Clubs -> "♣️" -- "C"
  Diamonds -> "♦️" -- "D"
  Hearts -> "♥️" -- "H"
  Spades -> "♠️" --"S"

instance Show Value where
    show v = case v of
               Number i -> show i
               Jack -> "J"
               Queen -> "Q"
               King -> "K"
               Ace -> "A"

instance Show Card where
    show c = show (cValue c) ++ show (cSuite c)

showUnicode :: Card -> [Char]
showUnicode c = show (cValue c) ++ showSuiteUnicode (cSuite c)

instance Show Hand where
    show h = unwords $ map show (hCards h)

colorCard :: Card -> String
colorCard c =
  "\STX\ETX" ++ colorcode ++ " " ++ showUnicode c ++ " \SI"
 where
  colorcode = case cSuite c of
                   Clubs    -> "01,00"
                   Spades   -> "01,00" -- "00,02"
                   Diamonds -> "04,00"
                   Hearts   -> "04,00" -- "00,03"
