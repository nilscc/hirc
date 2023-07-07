{-# LANGUAGE ImportQualifiedPost #-}

module Hirc.Modules.Poker.PotTests where

import Hirc.Modules.Poker.Bank (Money)
import Hirc.Modules.Poker.Pot (Pot (..))
import Hirc.Modules.Poker.Pot qualified as P
import Hirc.Types.Connection (UserName)
import Test.Hspec
import Test.Hspec.Expectations

buildPot :: [(UserName, Money)] -> Pot
buildPot = foldr (uncurry P.put) P.empty

-- Main spec

pokerPotSpec :: Spec
pokerPotSpec = do
  potProperties
  potSplit

-- Sub specs

potProperties :: Spec
potProperties = describe "Pot properties" $ do
  it "should have sane default values for empty pots" $ do
    P.height P.empty `shouldBe` 0
    P.size P.empty `shouldBe` 0
    P.toCall "p1" P.empty `shouldBe` 0
    P.toCall "p2" P.empty `shouldBe` 0
    P.toCall "??" P.empty `shouldBe` 0

  let pot =
        buildPot
          [ ("p1", 100),
            ("p2", 200)
          ]

  it "should calculate the correct pot height and size" $ do
    P.height pot `shouldBe` 200
    P.size pot `shouldBe` 300

  it "should calculate correct call amount" $ do
    P.toCall "p1" pot `shouldBe` 100
    P.toCall "p2" pot `shouldBe` 0
    P.toCall "??" pot `shouldBe` 200

  it "should move player pot to community pot on fold" $ do
    P.fold "p1" pot `shouldSatisfy` (100 ==) . potCommunity
    P.fold "p2" pot `shouldSatisfy` (200 ==) . potCommunity
    P.fold "??" pot `shouldSatisfy` (0 ==) . potCommunity

  it "should keep pot size constant on fold" $ do
    P.size (P.fold "p1" pot) `shouldBe` P.size pot
    P.size (P.fold "p2" pot) `shouldBe` P.size pot
    P.size (P.fold "??" pot) `shouldBe` P.size pot

potSplit :: Spec
potSplit = describe "Split pot" $ do
  let p1 = ("p1", 100)
      p2 = ("p2", 100)
      p3 = ("p3", 200)
      pot = buildPot [p1, p2, p3]

  it "should move correct player" $ do
    P.split "p3" pot
      `shouldBe` Just
        ( buildPot [p1, p2],
          buildPot [p3]
        )

  it "should update community pot correctly" $ do
    P.split "p3" pot {potCommunity = 50}
      `shouldBe` Just
        ( buildPot [p1, p2],
          (buildPot [p3]) {potCommunity = 50}
        )