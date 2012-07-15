{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Hirc.Types.Instances where

import Control.Monad.Error
import Control.Monad.Reader

import Hirc.Types.Commands
import Hirc.Types.Hirc


instance Error HircError where
  strMsg = H_Other


--------------------------------------------------------------------------------
-- Filter monad instances

instance Filtered HircM where
  runFiltered = lift

instance Filtered MessageM where
  runFiltered = id


--------------------------------------------------------------------------------
-- HircCommand instances

-- Basic type instances

instance IsHircCommand () where
  toCmd _ = HC_Nothing

-- functions

instance IsHircCommand a => IsHircCommand (String -> a) where
  toCmd f    = HC_Lam     (\s  -> toCmd (f s))

instance IsHircCommand a => IsHircCommand ([String] -> a) where
  toCmd f    = HC_Lams    (\ws -> toCmd (f ws))

-- monads

instance IsHircCommand a => IsHircCommand (MessageM a) where
  toCmd wm   = HC_Run     (fmap toCmd wm)

instance IsHircCommand a => IsHircCommand (HircM a) where
  toCmd hirc = HC_Run     (fmap toCmd $ lift $ hirc)

instance IsHircCommand a => IsHircCommand (IO a) where
  toCmd io   = HC_Run     (fmap toCmd $ liftIO io)


--------------------------------------------------------------------------------
-- Log instances

instance LogM MessageM where
  logChan     = lift logChan
  logSettings = lift logSettings

instance LogM HircM where
  logChan     = asks logChanH
  logSettings = asks logSettingsH

instance LogM Managed where
  logChan     = asks logChanM
  logSettings = asks logSettingsM
