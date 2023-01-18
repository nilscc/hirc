module Hirc.Modules.Poker.CardsTest (pokerCardsSpec) where

import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy, xit, pending )

import Hirc.Modules.Poker.Cards
import Data.Maybe (mapMaybe)

pokerCardsSpec :: Spec
pokerCardsSpec = do
    describe "Card Ord instance" $ do

        it "should create the correct order" $ do
            toCard "2C" `shouldSatisfy` (< toCard "2D")
            toCard "2C" `shouldSatisfy` (< toCard "3C")
            toCard "2D" `shouldSatisfy` (< toCard "3C")
            toCard "2C" `shouldSatisfy` (< toCard "JC")

    describe "Rank Ord instance" $ do

        -- test cards
        let Just c1 = toCard "2C" -- Card (Number 2) Clubs
            Just c2 = toCard "JD" -- Card Jack Diamonds
            Just c3 = toCard "KH" -- Card King Hearts
            Just c4 = toCard "AS" -- Card Ace Spades

        it "maintains correct order" $ do
            Pair Ace [c3,c2,c1] `shouldSatisfy` (< Pair Ace [c4,c3,c2])
            Pair Ace [c3,c2,c1] `shouldSatisfy` (< TwoPairs King Jack c1)

    describe "card ranks & findBestHand" $ do

        let cc = toCards "2C 5D JH QD AC"
            Just p1 = findBestHand $ cc ++ toCards "3S KS"
            Just p2 = findBestHand $ cc ++ toCards "AS 2S"
            Just p3 = findBestHand $ cc ++ toCards "10D KC"

        it "should find the best hand" $ do
            p1 `shouldBe` Hand (toCards "AC KS QD JH 5D")
            p2 `shouldBe` Hand (toCards "AC AS QD 2S 2C")
            p3 `shouldBe` Hand (toCards "AC KC QD JH 10D")

        it "should calculate the right rank" $ do
            rank p1 `shouldBe` Just (HighCard [Ace, King, Queen, Jack, Number 5])
            rank p2 `shouldBe` Just (TwoPairs Ace (Number 2) (Card Queen Diamonds))
            rank p3 `shouldBe` Just (Straight (Card Ace Clubs))
        
        it "should compare hands based on their ranks" $ do
            p1 `shouldSatisfy` (< p2)
            p2 `shouldSatisfy` (< p3)

 where
  toCards = mapMaybe toCard . words