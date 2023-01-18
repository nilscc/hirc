module Main where

import Test.Hspec

import Hirc.Modules.Poker.GameTest
import Hirc.Modules.Poker.CardsTest

main :: IO ()
main = hspec $ do
    pokerGameSpec
    pokerCardsSpec