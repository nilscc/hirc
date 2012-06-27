{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}

module Hirc.Types.Commands where

import Control.Monad.Reader

import Hirc.Types.Hirc


data HircCommand
  = HC_Nothing
  | HC_WithMsg      (WithMessage HircCommand)
  | HC_Lam          (String   -> HircCommand)
  | HC_Lams         ([String] -> HircCommand)
  | HC_Pred         (String -> Bool)

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

instance IsHircCommand a => IsHircCommand (HircM a) where
  toCmd hirc = HC_WithMsg (fmap toCmd $ lift hirc)

instance IsHircCommand a => IsHircCommand (IO a) where
  toCmd io   = HC_WithMsg (fmap toCmd $ liftIO io)

instance IsHircCommand (String -> Bool) where
  toCmd p    = HC_Pred    p

--------------------------------------------------------------------------------
-- Basic type instances

instance IsHircCommand () where
  toCmd _ = HC_Nothing
