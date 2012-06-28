{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Poker
  ( pokerModule
  ) where

import Data.List

import Hirc

pokerModule :: Module
pokerModule = Module "Poker" (Just updatePlayers) $ do

  onValidPrefix $ do

    userCommand $ \"count" -> startCounting
    userCommand $ \"play" "poker" -> startGame
    userCommand $ \"help" "poker" -> showHelp
    userCommand $ \"quit" "poker" -> quitGame
    userCommand $ \"join" "poker" -> acceptPlayers

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
  whisper "  <bot>: play poker   --   start a new game"
  whisper "  <bot>: help poker   --   show this help"
  whisper "  <bot>: join poker   --   join a new game"
  whisper "  <bot>: quit poker   --   quit the current game"
  whisper "While playing:"
  whisper "  bet <num>           --   bet a new sum"
  whisper "  call/fold           --   call the current bet or fold your cards"
  whisper "  all-in              --   go all in"
  whisper "  players             --   show all information about current players"

showPlayers :: MessageM ()
showPlayers = do
  mps <- load "players"
  case mps of
       Just pls | not (nullMap pls) ->
            say $ "Currently playing poker: " ++ intercalate ", " (elemsMap pls)
       _ -> say "There is noone playing poker at the moment!"

--------------------------------------------------------------------------------
-- Setting up the game environment

startGame :: MessageM ()
startGame = do
  state <- load "state"
  if (state == Nothing || state == Just "ended") then do
    answerTo $ \who ->
      case who of
           Left  _ -> answer "Sorry, but this game needs to be run in a public channel."
           Right _ -> do
             store "state" "new game"
             n <- getNickname
             say $ "Poker game started. Say \"" ++ n ++ ": join poker\" to join this game."
   else do
    answer "Poker game already in progress. You can play only one game per channel."


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
       say $ "Player \"" ++ n ++ "\" joins the game."

quitGame :: MessageM ()
quitGame = withNickAndUser $ \n u -> do
  update "players" $ \mm ->
    case mm of
         Just m  -> deleteMap u m
         Nothing -> emptyMap
  say $ "\"" ++ n ++ "\" left the game!"
