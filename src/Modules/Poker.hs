{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Modules.Poker
  ( poker
  ) where

import Data.List

import Hirc

poker :: Module
poker = Module "Poker" (Just updatePlayers) $ do

  state <- load "state"

  onValidPrefix $ do

    userCommand $ \"poker" "start" -> do
      if (state == Nothing || state == Just "ended") then do
        answerTo $ \who ->
          case who of
               Left  _ -> answer "Sorry, but this game needs to be run in a public channel."
               Right _ -> startGame
       else do
        answer "Poker game already in progress. You can play only one game per channel."

    userCommand $ \"poker" "help" -> showHelp
    userCommand $ \"poker" "quit" -> quitGame
    userCommand $ \"poker" "join" -> acceptPlayers

  userCommand $ \"players" -> showPlayers

-- On nickchange: update player names
updatePlayers :: Username -> Nickname -> MessageM ()
updatePlayers un nn = do
  updateAll "players" $ \_chan m ->
    if memberMap un m
       then insertMap un nn m
       else m

--------------------------------------------------------------------------------
-- Information

-- Help
showHelp :: MessageM ()
showHelp = do
  whisper "Available commands:"
  whisper "  <bot>: poker start   --   start a new game"
  whisper "  <bot>: poker help    --   show this help"
  whisper "  <bot>: poker join    --   join a new game"
  whisper "  <bot>: poker quit    --   quit the current game"
  whisper "While playing:"
  whisper "  bet <num>            --   bet a new sum"
  whisper "  call/fold            --   call the current bet or fold your cards"
  whisper "  all-in               --   go all in"
  whisper "  players              --   show all information about current players"

showPlayers :: MessageM ()
showPlayers = do
  mps <- load "players"
  case mps of
       Just pls | not (nullMap pls) ->
            answer' $ "Current players: " ++ intercalate ", " (elemsMap pls)
       _ -> answer' "There is noone playing at the moment!"

--------------------------------------------------------------------------------
-- Setting up the game environment

startGame :: MessageM ()
startGame = do
  store "state" "new game"
  n <- getNickname
  answer' $ "Poker game started. Say \"" ++ n ++ ": poker join\" to join this game."

acceptPlayers :: MessageM ()
acceptPlayers = require "state" "new game" $ do

  withNickAndUser $ \n u -> do
    (mps :: Maybe Map) <- load "players"
    if maybe False (memberMap u) mps then
       answer "You're already in the game!"
     else do
       update "players" $ \ms ->
         case ms of
              Nothing -> singletonMap u n
              Just m  -> insertMap u n m
       answer' $ "Player \"" ++ n ++ "\" joins the game."

quitGame :: MessageM ()
quitGame = withNickAndUser $ \n u -> do
  update "players" $ \mm ->
    case mm of
         Just m  -> deleteMap u m
         Nothing -> emptyMap
  answer' $ "\"" ++ n ++ "\" left the game!"
