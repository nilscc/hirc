{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Admin
  ( adminModule
  , AdminSettings (..), emptyAdminSettings
  ) where

import qualified Data.Map as M

import Hirc
import Hirc.Modules.Admin.Acid


--------------------------------------------------------------------------------
-- Main module

runAdminModule :: AdminM ()
runAdminModule = do
  currentChannel <- getCurrentChannel
  onValidPrefix $ do
    userCommand $ \"admin" "auth" pw         -> auth pw
    userCommand $ \"admin" "join" channel    -> requireAuth $ joinChannel channel
    userCommand $ \"admin" "part" channel    -> doneAfter $ requireAuth $ partChannel channel
    userCommand $ \"admin" "part"            -> doneAfter $ requireAuth $ maybe (return ()) partChannel currentChannel
    userCommand $ \"admin" "set" "nick" name -> requireAuth $ changeNickname name
    userCommand $ \"admin" "help" "admin"    -> showHelp

showHelp :: AdminM ()
showHelp = do
  whisper "Admin module, available commands:"
  whisper "  help admin         --   show this help"
  whisper "  auth <password>    --   authenticate as admin"
  whisper "  join <channel>     --   join new channel"
  whisper "  part [<channel>]   --   part either the current or the specified channel"
  whisper "  set nick <name>    --   set a new nickname"

auth :: String -> AdminM ()
auth pw = withUsername $ \un -> do
  response <- update $ Authenticate un pw
  createCheckpoint
  answer response

requireAuth :: AdminM () -> AdminM ()
requireAuth m = do
  withUsername $ \uname -> do
    ia <- query $ IsAdmin uname Nothing
    if ia
       then m
       else answer "You don't have permission to do that."


--------------------------------------------------------------------------------
-- Module instances

adminModule :: AdminSettings -> Module
adminModule = newModule . AdminModule

newtype AdminModule = AdminModule { unAM :: AdminSettings }

type AdminM a = ModuleM AdminModule a

instance IsModule AdminModule where
  type ModuleState AdminModule = AcidState AdminSettings
  moduleName     _ = "Admin"
  onNickChange   _ = Nothing
  initModule     a = openLocalState (unAM a)
  runModule      _ = runAdminModule
  shutdownModule _ = Just closeAcidState 

emptyAdminSettings :: AdminSettings
emptyAdminSettings = AdminSettings [] M.empty Nothing
