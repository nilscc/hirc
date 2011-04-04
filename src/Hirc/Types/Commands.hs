{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}

module Hirc.Types.Commands where

import Control.Monad.Reader

import Hirc.Types.Hirc
import Hirc.Types.Message


data HircCommand
  = HC_Nothing
  | HC_WithMsg      (WithMessage HircCommand)
  | HC_Lam          (String   -> HircCommand)
  | HC_Lams         ([String] -> HircCommand)

class IsHircCommand a where
  toCmd :: a -> HircCommand

--------------------------------------------------------------------------------
-- Function instances

instance IsHircCommand a => IsHircCommand (String -> a) where
  toCmd f    = HC_Lam     (\s  -> toCmd (f s))

instance IsHircCommand a => IsHircCommand ([String] -> a) where
  toCmd f    = HC_Lams    (\ws -> toCmd (f ws))

instance IsHircCommand a => IsHircCommand (WithMessage a) where
  toCmd wm   = HC_WithMsg (fmap toCmd wm)

instance IsHircCommand a => IsHircCommand (Hirc a) where
  toCmd hirc = HC_WithMsg (fmap toCmd $ lift hirc)

instance IsHircCommand a => IsHircCommand (IO a) where
  toCmd io   = HC_WithMsg (fmap toCmd $ liftIO io)

--------------------------------------------------------------------------------
-- Basic type instances

instance IsHircCommand () where
  toCmd _ = HC_Nothing
