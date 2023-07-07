module Main where

import Hirc.Modules.Poker.AllInTests (pokerAllInSpec)
import Hirc.Modules.Poker.CardsTest (pokerCardsSpec)
import Hirc.Modules.Poker.GameTest (pokerGameSpec)
import Hirc.Modules.Poker.PotTests (pokerPotSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  pokerPotSpec
  pokerGameSpec
  pokerCardsSpec
  pokerAllInSpec