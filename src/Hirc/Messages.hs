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


handleIncomingMessage :: MessageM () -> HircM ()
handleIncomingMessage m = do
  msg <- getMsg
  runReaderT (m `catchError` noMsgErr) (msg, nullCtxt)
 where
  noMsgErr e | e == noMsg = return ()
             | otherwise  = throwError e
  nullCtxt = Context { ctxtModule = Nothing }

done :: MonadPlus m => m ()
done = mzero

doneAfter :: (MonadPlus m, Filtered m) => m () -> MessageM ()
doneAfter m = runFiltered m >> done


--------------------------------------------------------------------------------
-- Filter

getMessage :: MessageM Message
getMessage = fmap fst ask

getCurrentChannel :: MessageM (Maybe String)
getCurrentChannel = do
  msg <- getMessage
  let cmd    = IRC.msg_command msg
      (ch:_) = IRC.msg_params msg
  me  <- gets ircNickname
  return $
    if cmd == "PRIVMSG" && ch /= me then
       Just ch
     else
       Nothing

onCommand :: Filtered m
          => String
          -> m ()
          -> MessageM ()
onCommand c m = do
  c' <- fmap IRC.msg_command getMessage
  when (c == c') (runFiltered m)

-- | All current message parameters, see `Message'
withParams :: Filtered m
           => ([String] -> m ())
           -> MessageM ()
withParams m = do
  ps <- fmap IRC.msg_params getMessage
  catchPatternException $ runFiltered (m ps)

withNickname :: Filtered m
             => (Nickname -> m ())
             -> MessageM ()
withNickname m = catchPatternException $ do
  Just (NickName n _ _) <- fmap IRC.msg_prefix getMessage
  runFiltered (m n)

withUsername :: Filtered m
             => (Username -> m ())
             -> MessageM ()
withUsername m = catchPatternException $ do
  Just (NickName _ (Just u) _) <- fmap IRC.msg_prefix getMessage
  runFiltered (m u)

withNickAndUser :: Filtered m
                => (Nickname -> Username -> m ())
                -> MessageM ()
withNickAndUser m =
  withNickname $ \n ->
  withUsername $ \u ->
    m n u

withServer :: Filtered m
           => (String -> m ())
           -> MessageM ()
withServer m = catchPatternException $ do
  Just p <- fmap IRC.msg_prefix getMessage
  case p of
       Server n              -> runFiltered (m n)
       NickName _ _ (Just n) -> runFiltered (m n)
       _                     -> return ()

--------------------------------------------------------------------------------
-- Exceptions

catchPatternException :: MonadPeelIO m => m () -> m ()
catchPatternException =
  handle $ \(PatternMatchFail _) -> return ()
