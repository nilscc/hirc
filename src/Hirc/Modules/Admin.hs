{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Admin
  ( adminModule
  , AdminSettings (..)
  ) where

import Data.Maybe

import Hirc

adminModule :: AdminSettings -> Module
adminModule settings = Module "Admin" Nothing $ do
  currentChannel <- getCurrentChannel
  onValidPrefix $ do
    userCommand $ \"auth" pw         -> auth pw settings
    userCommand $ \"join" channel    -> requireAuth $ joinChannel channel
    userCommand $ \"part" channel    -> doneAfter $ requireAuth $ partChannel channel
    userCommand $ \"part"            -> doneAfter $ requireAuth $ maybe (return ()) partChannel currentChannel
    userCommand $ \"set" "nick" name -> requireAuth $ setNickname name
    userCommand $ \"help" "admin"    -> showHelp

showHelp :: MessageM ()
showHelp = do
  whisper "Admin module, available commands:"
  whisper "  help admin         --   show this help"
  whisper "  auth <password>    --   authenticate as admin"
  whisper "  join <channel>     --   join new channel"
  whisper "  part [<channel>]   --   part either the current or the specified channel"
  whisper "  set nick <name>    --   set a new nickname"

newtype AdminSettings = AdminSettings { adminPassword :: String }

auth :: String -> AdminSettings -> MessageM ()
auth pw settings
  | pw == adminPassword settings = withUsername $ \uname -> do
    updateGlobal "admins" $ \ml -> case ml of
      Nothing -> singletonList uname
      Just l  -> concatList uname l
    answer "You're successfully authenticated."
  | otherwise = do
    answer "Incorrect password."

requireAuth :: MessageM () -> MessageM ()
requireAuth m = do
  withUsername $ \uname -> do
    global  <- fmap (fromMaybe emptyList) (loadGlobal "admins")
    channel <- fmap (fromMaybe emptyList) (load "admins")
    let admins = appendList global channel
    if (uname `elemList` admins)
       then m
       else answer "You don't have permission to do that."
