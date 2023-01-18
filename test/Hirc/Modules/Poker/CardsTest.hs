module Hirc.Modules.Poker.CardsTest (pokerCardsSpec) where

import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy )

import Hirc.Modules.Poker.Cards
import Data.Maybe (mapMaybe)

pokerCardsSpec :: Spec
pokerCardsSpec = do
    let cc = toCards "2C 5D JH QD AC"
        p1 = Hand $ cc ++ toCards "3S KS"
        p2 = Hand $ cc ++ toCards "AS 2S"
        p3 = Hand $ cc ++ toCards "10D KC"

    describe "card ranks" $ do
        it "should be correct" $ do
            rank p1 `shouldBe` Just (HighCard [Ace, King, Queen, Jack, Number 5, Number 3, Number 2])
            rank p2 `shouldBe` Just (TwoPairs Ace (Number 2) (Card Queen Diamonds))
            rank p3 `shouldBe` Just (Straight (Card Ace Clubs))
        
        it "should compare hands based on their ranks" $ do
            p1 `shouldSatisfy` (< p2)
            p2 `shouldSatisfy` (< p3)

 where
  toCards = mapMaybe toCard . words