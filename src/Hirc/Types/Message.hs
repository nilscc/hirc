{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Hirc.Types.Message where

import Control.Monad.Reader
import Control.Monad.IO.Peel
import Network.IRC

import Hirc.Types.Hirc


--------------------------------------------------------------------------------
-- Message filters

type WithMessage = ReaderT Message Hirc

class MonadPeelIO m => Filtered m where
  runFiltered :: m () -> WithMessage ()

instance Filtered WithMessage where
  runFiltered = id

instance Filtered Hirc where
  runFiltered = lift

instance LogM WithMessage where
  logChan     = lift logChan
  logSettings = lift logSettings
