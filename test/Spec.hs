module Main where

import Test.Hspec

import Hirc.Modules.Poker.Game.Test

main :: IO ()
main = hspec $ do
    pokerGameSpec