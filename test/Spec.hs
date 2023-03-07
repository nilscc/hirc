module Main where

import Hirc.Modules.Poker.AllInTests (pokerAllInSpec)
import Hirc.Modules.Poker.CardsTest (pokerCardsSpec)
import Hirc.Modules.Poker.GameTest (pokerGameSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  pokerGameSpec
  pokerCardsSpec
  pokerAllInSpec