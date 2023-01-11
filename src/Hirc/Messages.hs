{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
   MultiParamTypeClasses, FunctionalDependencies #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Hirc.Messages where

import Control.Monad ( when, MonadPlus(mzero) )
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import qualified Network.IRC as IRC

import Hirc.Connection ( Prefix(NickName, Server), awaitTVar )
import Hirc.Types.Connection ( UserName, NickName )
import Hirc.Types.Hirc
    ( ModuleMessageM,
      ModuleM,
      ContainsMessage(getMessage),
      IrcInstance (currentNickname, msgBroadcast), ContainsIrcInstance (askIrcInstance), CanSend, HircInstance (ircInstance), MessageM (unMessageM) )
import Hirc.Types.Instances ()
import Control.Monad.Reader (mapReaderT, MonadReader (ask), ReaderT (runReaderT))
import Control.Concurrent.STM (atomically, readTVarIO, TChan, readTChan)
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent (readChan)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Hirc.Types (IrcInstance(msgBroadcast))


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

getCurrentChannel :: (ContainsMessage m, MonadFail m, ContainsIrcInstance m)
  => m (Maybe String)
getCurrentChannel = do
  msg <- getMessage
  let cmd = B8.unpack $ IRC.msg_command msg
      ((B8.unpack -> ch):_) = IRC.msg_params msg

  -- get current nickname
  ircInst <- askIrcInstance
  nick <- liftIO $ readTVarIO (currentNickname ircInst)

  return $
    if cmd == "PRIVMSG" && ch /= nick then
       Just ch
     else
       Nothing

onCommand :: (ContainsMessage m)
          => String
          -> m ()
          -> m ()
onCommand c m = do
  c' <- B8.unpack . IRC.msg_command <$> getMessage
  when (c == c') m

-- | All current message parameters, see `Message'
withParams :: CanSend m
           => ([String] -> m ())
           -> m ()
withParams m = do
  ps <- map B8.unpack . IRC.msg_params <$> getMessage
  m ps

withNickname :: CanSend m
             => (NickName -> m ())
             -> m ()
withNickname m = do
  Just (NickName (B8.unpack -> n) _ _) <- IRC.msg_prefix <$> getMessage
  m n

-- | Get the username of the current message. If there is a leading '~' it is
-- discarded.
withUsername :: CanSend m
             => (UserName -> m ())
             -> m ()
withUsername m = do
  Just (NickName _ (Just (B8.unpack -> u)) _) <- IRC.msg_prefix <$> getMessage
  m $ dropWhile (== '~') u

withNickAndUser :: CanSend m
                => (NickName -> UserName -> m ())
                -> m ()
withNickAndUser m = do
  Just (NickName (B8.unpack -> n) (Just (B8.unpack -> u)) _) <- IRC.msg_prefix <$> getMessage
  m n (dropWhile (== '~') u)

withServer :: (ContainsMessage m, MonadFail m)
           => (String -> m ())
           -> m ()
withServer m = do
  Just p <- IRC.msg_prefix <$> getMessage
  case p of
       Server n              -> m $ B8.unpack n
       NickName _ _ (Just n) -> m $ B8.unpack n
       _                     -> return ()