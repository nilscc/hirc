{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Messages where

import Prelude hiding (catch)
import Control.Monad.Reader
import qualified Network.IRC as IRC

import Hirc
import Connection
import Types


handleIncomingMessage :: WithMessage () -> Hirc ()
handleIncomingMessage m = do
  msg <- getMsg
  runReaderT m msg


--------------------------------------------------------------------------------
-- Filter

onCommand :: Filtered m
          => String
          -> m ()
          -> WithMessage ()
onCommand c m = do
  c' <- asks IRC.msg_command
  when (c == c') (runFiltered m)

withParams :: Filtered m
           => ([String] -> m ())
           -> WithMessage ()
withParams m = do
  ps <- asks IRC.msg_params
  catchPatternException $ runFiltered (m ps)

withNickname :: Filtered m
             => (String -> m ())
             -> WithMessage ()
withNickname m = catchPatternException $ do
  Just (NickName n _ _) <- asks IRC.msg_prefix
  runFiltered (m n)

withUsername :: Filtered m
             => (String -> m ())
             -> WithMessage ()
withUsername m = catchPatternException $ do
  Just (NickName _ (Just u) _) <- asks IRC.msg_prefix
  runFiltered (m u)

withNickAndUser :: Filtered m
                => (String -> String -> m ())
                -> WithMessage ()
withNickAndUser m =
  withNickname $ \n ->
  withUsername $ \u ->
    m n u


withServer :: Filtered m
           => (String -> m ())
           -> WithMessage ()
withServer m = catchPatternException $ do
  Just p <- asks IRC.msg_prefix
  case p of
       Server n              -> runFiltered (m n)
       NickName _ _ (Just n) -> runFiltered (m n)
       _                     -> return ()


--------------------------------------------------------------------------------
-- Exceptions

catchPatternException :: WithMessage () -> WithMessage ()
catchPatternException =
  handle $ \(PatternMatchFail _) -> return ()
