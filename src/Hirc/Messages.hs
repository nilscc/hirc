{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Messages where

import Prelude hiding (catch)
import Control.Concurrent.MState
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.IO.Peel
import Control.Exception.Peel
import qualified Network.IRC as IRC

import Hirc.Connection
import Hirc.Types


handleIncomingMessage :: ModuleMessageM m () -> ModuleM m ()
handleIncomingMessage m = do
  msg <- lift getMsg
  mapMState (runReaderT `flip` msg)
    (m `catchError` noMsgErr)
 where
  noMsgErr e | e == noMsg = return ()
             | otherwise  = throwError e

done :: MonadPlus m => m ()
done = mzero

doneAfter :: (MonadPlus m) => m () -> m ()
doneAfter m = m >> done


--------------------------------------------------------------------------------
-- Filter

getCurrentChannel :: ContainsMessage m => m (Maybe String)
getCurrentChannel = do
  msg <- getMessage
  let cmd    = IRC.msg_command msg
      (ch:_) = IRC.msg_params msg
  me  <- ircNickname `fmap` getHircState
  return $
    if cmd == "PRIVMSG" && ch /= me then
       Just ch
     else
       Nothing

onCommand :: (ContainsMessage m)
          => String
          -> m ()
          -> m ()
onCommand c m = do
  c' <- IRC.msg_command `fmap` getMessage
  when (c == c') (m)

-- | All current message parameters, see `Message'
withParams :: (ContainsMessage m)
           => ([String] -> m ())
           -> m ()
withParams m = do
  ps <- fmap IRC.msg_params getMessage
  catchPatternException $ (m ps)

withNickname :: (ContainsMessage m)
             => (Nickname -> m ())
             -> m ()
withNickname m = catchPatternException $ do
  Just (NickName n _ _) <- fmap IRC.msg_prefix getMessage
  (m n)

-- | Get the username of the current message. If there is a leading '~' it is
-- discarded.
withUsername :: (ContainsMessage m)
             => (Username -> m ())
             -> m ()
withUsername m = catchPatternException $ do
  Just (NickName _ (Just u) _) <- fmap IRC.msg_prefix getMessage
  (m $ dropWhile (== '~') u)

withNickAndUser :: (ContainsMessage m)
                => (Nickname -> Username -> m ())
                -> m ()
withNickAndUser m = do
  Just (NickName n (Just u) _) <- fmap IRC.msg_prefix getMessage
  m n (dropWhile (== '~') u)

withServer :: (ContainsMessage m)
           => (String -> m ())
           -> m ()
withServer m = catchPatternException $ do
  Just p <- fmap IRC.msg_prefix getMessage
  case p of
       Server n              -> (m n)
       NickName _ _ (Just n) -> (m n)
       _                     -> return ()

--------------------------------------------------------------------------------
-- Exceptions

catchPatternException :: MonadPeelIO m => m () -> m ()
catchPatternException =
  handle $ \(PatternMatchFail _) -> return ()
