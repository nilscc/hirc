module Hirc.Modules.Poker.AllInTests where

import System.Random
import Hirc.Modules.Poker.GameTest (newPlayer', join')
import Test.Hspec (Spec, describe, it, shouldBe)
import Hirc.Modules.Poker.Game (joinPlayer, newGame)

pokerAllInSpec :: Spec
pokerAllInSpec = do
  describe "simple two player all in" $ do

    let p1 = newPlayer' "p1"
        p2 = newPlayer' "p2"
        g1 = join' p2 $ join' p1 $ newGame (mkStdGen 42)

    

    it "" $ do
      True `shouldBe` True