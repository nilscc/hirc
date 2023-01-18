module Hirc.Modules.Poker.GameTest (pokerGameSpec) where

import Test.Hspec ( describe, it, shouldBe, Spec )

import Hirc
import Hirc.Modules.Poker.Game

newPlayer :: NickName -> Player
newPlayer nick = Player
  { playerHand = Nothing
  , playerStack = 10000
  , playerNickname = nick
  , playerUsername = nick
  , playerPot = 0
  }

pokerGameSpec :: Spec
pokerGameSpec = do
  describe "join and part" $ do

    it "should reset player pots and hand" $ do
      let p = (newPlayer "player1") { playerPot = 1000 }
          g = joinPlayer p $ newGame
          --p' = 
      playerPot (head $ players g) `shouldBe` 0
      playerHand (head $ players g) `shouldBe` Nothing

    it "should remove the correct player only" $ do
      let p1 = newPlayer "p1"
          p2 = newPlayer "p2"
          p3 = newPlayer "p3"
          g = partPlayer p2 $
              joinPlayer p3 $
              joinPlayer p2 $
              joinPlayer p1 $
              newGame
      players g `shouldBe` [p1, p3]

    