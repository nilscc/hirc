{-# LANGUAGE ImportQualifiedPost #-}

module Hirc.Modules.Poker.Pot where

import Data.Map (Map (..))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set (..))
import Data.Set qualified as S
import Hirc.Modules.Poker.Bank (Money)
import Hirc.Types.Connection (UserName)

data Pot = Pot
  { potPlayers :: Map UserName Money,
    potCommunity :: Money
  }
  deriving (Eq, Show)

empty = Pot {potPlayers = M.empty, potCommunity = 0}

height :: Pot -> Money
height p
  | M.null ps = 0
  | otherwise = maximum ps
  where
    ps = potPlayers p

size :: Pot -> Money
size p = sum (potPlayers p) + potCommunity p

toCall :: UserName -> Pot -> Money
toCall u p =
  height p - fromMaybe 0 (M.lookup u (potPlayers p))

set :: UserName -> Money -> Pot -> Pot
set u m p = p {potPlayers = M.insert u m (potPlayers p)}

put :: UserName -> Money -> Pot -> Pot
put u m p =
  p
    { potPlayers = M.alter (Just . maybe m (m +)) u (potPlayers p)
    }

fold :: UserName -> Pot -> Pot
fold u p =
  p
    { potPlayers = M.delete u (potPlayers p),
      potCommunity = potCommunity p + fromMaybe 0 (M.lookup u (potPlayers p))
    }

-- | Split
split :: UserName -> Pot -> Maybe (Pot, Pot)
split u p
  | Just m <- M.lookup u (potPlayers p) =
      Just (p1, p2 m)
  | otherwise =
      Nothing
  where
    p1 =
      p
        { potPlayers = M.delete u (potPlayers p),
          potCommunity = 0
        }
    p2 m =
      p
        { potPlayers = M.singleton u m,
          potCommunity = potCommunity p
        }