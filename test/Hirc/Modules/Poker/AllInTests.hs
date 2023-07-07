module Hirc.Modules.Poker.AllInTests where

import System.Random
import Hirc.Modules.Poker.GameTest (join')
import Test.Hspec (Spec, describe, it, shouldBe)
import Hirc.Modules.Poker.Player (Player)
import Hirc.Modules.Poker.Game (joinPlayer, newGame, newPlayer)
import Hirc (UserName)
import Hirc.Modules.Poker.Bank (Money)

newPlayer' :: UserName -> Money -> Player
newPlayer' u = newPlayer u u

pokerAllInSpec :: Spec
pokerAllInSpec = do
  describe "simple two player all in" $ do

    let p1 = newPlayer' "p1" 100
        p2 = newPlayer' "p2" 10
        g1 = join' p2 $ join' p1 $ newGame (mkStdGen 42)

    

    it "" $ do
      True `shouldBe` True