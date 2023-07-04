module Hirc.Modules.Poker.GameTest where

import Control.Monad.IO.Class
import Test.Hspec
import Test.Hspec.Expectations
import System.Random
import Hirc
import Hirc.Modules.Poker.Game
import Control.Exception (throw, assert)
import Data.Either (fromRight, fromLeft, isRight, isLeft)
import Hirc.Modules.Poker.Bank (Money)
import System.Random (StdGen)
import Data.Maybe (isJust)
import Hirc.Modules.Poker.Game (Game(communityCards), isNextPhase)

newPlayer' n = newPlayer n n 10000

-- First 4 players
p1 = newPlayer' "p0"
p2 = newPlayer' "p1"
p3 = newPlayer' "p2"
p4 = newPlayer' "p3"

isOK :: GameUpdate -> Bool
isOK GameUpdated{} = True
isOK GameEnded{} = True
isOK _ = False

isOngoing :: GameUpdate -> Bool
isOngoing GameUpdated{} = True
isOngoing _ = False

-- Continue playing
cont :: (Game -> GameUpdate) -> Game -> Game
cont f g = case f g of
  GameUpdated g -> g
  e -> error $ "Unexpected game update: " ++ show e

check' :: Game -> Game
check' = cont check

call' :: Game -> Game
call' = cont call

fold' :: Game -> Game
fold' = cont fold

raise' :: Money -> Game -> Game
raise' x = cont $ raise x

join' :: Player -> Game -> Game
join' p = cont (joinPlayer p)

part' :: Player -> Game -> Game
part' p = cont (partPlayer p)

-- Default starting game
g0, g1, g2, g3, g4 :: Game

-- new game
g0 = join' p4
   . join' p3
   . join' p2
   . join' p1
   $ newGame (mkStdGen 42)

-- game pre flop
g1 = cont payBlinds $ cont dealCards g0
-- flop game
g2 = check' $ call' $ call' $ call' g1
-- turn game
g3 = check' $ check' $ check' $ check' g2
-- river game
g4 = check' $ check' $ check' $ check' g3

g5 :: GameUpdate

-- end game result
g5 = check . check' . check' $ check' g4

pokerGameSpec :: Spec
pokerGameSpec = do

  testSetupSpec
  joinPartSpec
  checkSpec
  foldSpec
  allInSpec
  nextPhaseSpec

nextPhaseSpec :: Spec
nextPhaseSpec = describe "next phase" $ do
  it "should give correct result in a game without raises" $ do
    isNextPhase g0 `shouldBe` False
    isNextPhase g1 `shouldBe` False -- preflop game
    isNextPhase g2 `shouldBe` True -- flop game
    isNextPhase (check' g2) `shouldBe` False
    isNextPhase (check' $ check' g2) `shouldBe` False
    isNextPhase g3 `shouldBe` True -- turn game
    isNextPhase (check' g3) `shouldBe` False
    isNextPhase (check' $ check' g3) `shouldBe` False
    isNextPhase g4 `shouldBe` True -- river game
    isNextPhase (check' g4) `shouldBe` False
    isNextPhase (check' $ check' g4) `shouldBe` False

testSetupSpec :: Spec
testSetupSpec = describe "test setup" $ do
  it "games should have the right community cards" $ do
    communityCards g0 `shouldBe` PreFlop
    communityCards g1 `shouldBe` PreFlop
    communityCards g2 `shouldSatisfy` \(Flop _) -> True
    communityCards g3 `shouldSatisfy` \(Turn _) -> True
    communityCards g4 `shouldSatisfy` \(River _) -> True
    g5 `shouldSatisfy` isOK

joinPartSpec :: Spec
joinPartSpec = describe "join and part" $ do

  it "should reset player pots and hand" $ do
    let p = (newPlayer' "p5") { playerPot = 1000 }
        g = join' p g0
        l = last $ players g
    l `shouldNotBe` p
    playerUsername l `shouldBe` playerUsername p
    playerPot l `shouldBe` 0
    playerHand l `shouldBe` Nothing

  it "should remove the correct player only" $ do
    players (part' p2 g0) `shouldBe` [p1, p3, p4]

checkSpec :: Spec
checkSpec = describe "check" $ do
  return ()

foldSpec :: Spec
foldSpec = describe "fold" $ do

  it "should keep pot size constant" $ do
    totalPotSize (fold' g0) `shouldBe` totalPotSize g0
    totalPotSize (fold' g1) `shouldBe` totalPotSize g1
    totalPotSize (fold' g2) `shouldBe` totalPotSize g2

  it "should end the game if the last player folds" $ do
    (fold . fold' . fold' $ g1) `shouldSatisfy` isOK

  it "should not end game when first player folds" $ do
    let g = cont dealCards
          . join' p3 . join' p2 . join' p1
          $ newGame (mkStdGen 42)
        p = players g
        g' = cont payBlinds g
        p' = players g'

    currentPlayer g' `shouldBe` p !! 2
    
    isNextPhase g' `shouldBe` False

    -- perform fold
    let f = fold g'
        f' = fold' g'
    f `shouldSatisfy` isOngoing
    communityCards f' `shouldBe` PreFlop


  it "should update last raise position correctly" $ do

    -- setup raise on 3rd position
    let g = raise' 500 . check' $ check' g2
    currentPosition g `shouldBe` 3
    communityCards g `shouldSatisfy` \(Flop _) -> True

    -- get list of players
    let [p0,p1,p2,p3] = players g

    -- raise was done in 3rd position (counting from 0)
    lastRaise g `shouldBe` Just ((2, playerUsername p2), 500)

    -- fold in last position
    let g'1 = fold' g
    currentPosition g'1 `shouldBe` 0
    lastRaise g'1 `shouldBe` Just ((2, playerUsername p2), 500)

    -- fold in first position
    let g'2 = fold' $ call' g
    currentPosition g'2 `shouldBe` 0
    lastRaise g'2 `shouldBe` Just ((1, playerUsername p2), 500)

    ---------------------------------------------------------
    -- perform fold in position of last raise (in next phase)
    --

    -- make all players call and go into turn
    let g'3 = call' . call' . call' $ g
    currentPosition g'3 `shouldBe` 2
    communityCards g'3 `shouldSatisfy` \(Turn _) -> True

    -- perform the fold
    let g'4 = fold' g'3
    currentPosition g'4 `shouldBe` 2
    lastRaise g'4 `shouldBe` Just ((2, playerUsername p3), 500)
    -- it should not switch phase yet
    communityCards g'4 `shouldSatisfy` \(Turn _) -> True

    -- make all remaining players check until next phase
    let g'5 = check' . check' . check' $ g'4
    currentPosition g'5 `shouldBe` 2
    communityCards g'5 `shouldSatisfy` \(River _) -> True

allInSpec :: Spec
allInSpec = describe "all in" $ do
  -- start with flop game
  let g = g2

  --it ""
  return ()