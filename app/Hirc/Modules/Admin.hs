{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Admin
  ( adminModule,
    AdminSettings (..),
    emptyAdminSettings,
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Acid as AS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Hirc
import Hirc.Modules.Admin.Acid

--------------------------------------------------------------------------------
-- Main module

runAdminModule :: AdminM ()
runAdminModule = do
  currentChannel <- getCurrentChannel
  onValidPrefix $ do
    userCommand' $ \text -> case words text of
      ["admin", "auth", pw] -> auth pw
      ["admin", "join", channel] -> requireAuth $ joinChannel channel
      ["admin", "part", channel] -> doneAfter $ requireAuth $ partChannel channel
      ["admin", "part"] -> doneAfter $ requireAuth $ maybe (return ()) partChannel currentChannel
      ["admin", "set", "nick", name] -> requireAuth $ changeNickname name
      ["admin", "help"] -> showHelp
      _ -> return ()

showHelp :: AdminM ()
showHelp = do
  whisper "Admin module, available commands:"
  whisper "  help               --   show this help"
  whisper "  auth <password>    --   authenticate as admin"
  whisper "  join <channel>     --   join new channel"
  whisper "  part [<channel>]   --   part either the current or the specified channel"
  whisper "  set nick <name>    --   set a new nickname"

auth :: String -> AdminM ()
auth pw = withUsername $ \un -> do
  response <- update' $ Authenticate un pw
  createCheckpoint'
  answer response
  where
    update' u = do
      as <- ask
      liftIO $ AS.update as u
    createCheckpoint' = do
      as <- ask
      liftIO $ AS.createCheckpoint as

requireAuth :: AdminM () -> AdminM ()
requireAuth m = do
  withUsername $ \uname -> do
    ia <- query' $ IsAdmin uname Nothing
    if ia
      then m
      else answer "You don't have permission to do that."
  where
    query' q = do
      as <- ask
      liftIO $ AS.query as q

--------------------------------------------------------------------------------
-- Module instances

adminModule :: AdminSettings -> Module
adminModule = newModule . AdminModule

newtype AdminModule = AdminModule {unAM :: AdminSettings}

type AdminM a = ModuleMessageM AdminModule a

instance IsModule AdminModule where
  type ModuleState AdminModule = AcidState AdminSettings
  moduleName _ = "Admin"
  initModule a = openLocalState (unAM a)
  shutdownModule _ = Just closeAcidState
  onStartup _ = Nothing
  onMessage _ = Just runAdminModule

emptyAdminSettings :: AdminSettings
emptyAdminSettings = AdminSettings [] M.empty Nothing
