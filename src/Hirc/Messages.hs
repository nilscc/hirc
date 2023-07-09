{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hirc.Messages where

import Control.Concurrent (readChan)
import Control.Concurrent.STM (TChan, atomically, readTChan, readTVarIO)
import Control.Monad (MonadPlus (mzero), when)
import Control.Monad.IO.Class
  ( MonadIO (liftIO),
  )
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), mapReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString.Char8 qualified as B8
import Hirc.Connection (Prefix (NickName, Server), awaitTVar)
import Hirc.Types (IrcInstance (msgBroadcast))
import Hirc.Types.Connection (NickName, UserName)
import Hirc.Types.Hirc
  ( CanSend,
    ContainsIrcInstance (askIrcInstance),
    ContainsMessage (getMessage),
    HircInstance (ircInstance),
    IrcInstance (currentNickname, msgBroadcast),
    MessageM (unMessageM),
    ModuleM,
    ModuleMessageM,
  )
import Hirc.Types.Instances ()
import Network.IRC qualified as IRC

handleIncomingMessage :: TChan IRC.Message -> ModuleMessageM s () -> ModuleM s ()
handleIncomingMessage msgChan m = do
  -- get next message from channel
  msg <- liftIO $ atomically $ readTChan msgChan

  -- run reader inside ModuleM
  _ <- mapReaderT (runMaybeT . (`runReaderT` msg) . unMessageM) m
  return ()

done :: MonadPlus m => m ()
done = mzero

doneAfter :: (MonadPlus m) => m () -> m ()
doneAfter m = m >> done

--------------------------------------------------------------------------------
-- Filter

getCurrentChannel ::
  (ContainsMessage m, MonadFail m, ContainsIrcInstance m) =>
  m (Maybe String)
getCurrentChannel = do
  msg <- getMessage
  let cmd = B8.unpack $ IRC.msg_command msg
      ((B8.unpack -> ch) : _) = IRC.msg_params msg

  -- get current nickname
  ircInst <- askIrcInstance
  nick <- liftIO $ readTVarIO (currentNickname ircInst)

  return $
    if cmd == "PRIVMSG" && ch /= nick
      then Just ch
      else Nothing

onCommand ::
  (ContainsMessage m) =>
  String ->
  m () ->
  m ()
onCommand c m = do
  c' <- B8.unpack . IRC.msg_command <$> getMessage
  when (c == c') m

-- | All current message parameters, see `Message'
withParams ::
  CanSend m =>
  ([String] -> m ()) ->
  m ()
withParams m = do
  ps <- map B8.unpack . IRC.msg_params <$> getMessage
  m ps

withNickname ::
  CanSend m =>
  (NickName -> m ()) ->
  m ()
withNickname m = do
  Just (NickName (B8.unpack -> n) _ _) <- IRC.msg_prefix <$> getMessage
  m n

-- | Get the username of the current message. If there is a leading '~' it is
-- discarded.
withUsername ::
  CanSend m =>
  (UserName -> m ()) ->
  m ()
withUsername m = do
  Just (NickName _ (Just (B8.unpack -> u)) _) <- IRC.msg_prefix <$> getMessage
  m $ dropWhile (== '~') u

withNickAndUser ::
  CanSend m =>
  (NickName -> UserName -> m a) ->
  m a
withNickAndUser m = do
  Just (NickName (B8.unpack -> n) (Just (B8.unpack -> u)) _) <- IRC.msg_prefix <$> getMessage
  m n (dropWhile (== '~') u)

withServer ::
  (ContainsMessage m, MonadFail m) =>
  (String -> m ()) ->
  m ()
withServer m = do
  Just p <- IRC.msg_prefix <$> getMessage
  case p of
    Server n -> m $ B8.unpack n
    NickName _ _ (Just n) -> m $ B8.unpack n
    _ -> return ()