{-# LANGUAGE PatternGuards #-}
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
    userCommand $ \"admin" "auth" pw         -> auth pw settings
    userCommand $ \"admin" "join" channel    -> requireAuth settings $ joinChannel channel
    userCommand $ \"admin" "part" channel    -> doneAfter $ requireAuth settings $ partChannel channel
    userCommand $ \"admin" "part"            -> doneAfter $ requireAuth settings $ maybe (return ()) partChannel currentChannel
    userCommand $ \"admin" "set" "nick" name -> requireAuth settings $ changeNickname name
    userCommand $ \"admin" "help" "admin"    -> showHelp

showHelp :: MessageM ()
showHelp = do
  whisper "Admin module, available commands:"
  whisper "  help admin         --   show this help"
  whisper "  auth <password>    --   authenticate as admin"
  whisper "  join <channel>     --   join new channel"
  whisper "  part [<channel>]   --   part either the current or the specified channel"
  whisper "  set nick <name>    --   set a new nickname"

data AdminSettings = AdminSettings
  { adminPassword :: Maybe String
  , adminUsers    :: [Username]
  }

auth :: String -> AdminSettings -> MessageM ()
auth pw settings
  | Nothing <- adminPassword settings =
    answer "Password authentication disabled."
  | Just settingsPw <- adminPassword settings
  , pw == settingsPw =
    withUsername $ \uname -> do
      updateGlobal "admins" $ \ml -> case ml of
        Nothing -> singletonList uname
        Just l  -> concatList uname l
      answer "You're successfully authenticated."
  | otherwise = do
    answer "Incorrect password."

requireAuth :: AdminSettings -> MessageM () -> MessageM ()
requireAuth settings m = do
  withUsername $ \uname -> do
    global   <- fmap (fromMaybe [] . join . fmap fromList) (loadGlobal "admins")
    channel  <- fmap (fromMaybe [] . join . fmap fromList) (load "admins")
    let admins = adminUsers settings ++ global ++ channel
    if (uname `elem` admins)
       then m
       else answer "You don't have permission to do that."
