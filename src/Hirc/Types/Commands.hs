{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}

module Hirc.Types.Commands where

import Control.Monad.Reader

import Hirc.Types.Hirc
import Hirc.Types.Message


data HircCommand
  = HC_Str String
  | HC_Nothing
  | HC_WithMsg      (WithMessage HircCommand)
  | HC_Lam          (Maybe String -> HircCommand)
  | HC_Lams         ([String]     -> HircCommand)

class FromHaskell a where
  fromHaskell :: a -> HircCommand

--------------------------------------------------------------------------------
-- Function instances

instance FromHaskell a => FromHaskell (Maybe String -> a) where
  fromHaskell f    = HC_Lam     (\s  -> fromHaskell (f s))

instance FromHaskell a => FromHaskell ([String] -> a) where
  fromHaskell f    = HC_Lams    (\ws -> fromHaskell (f ws))

instance FromHaskell a => FromHaskell (WithMessage a) where
  fromHaskell wm   = HC_WithMsg (fmap fromHaskell wm)

instance FromHaskell a => FromHaskell (Hirc a) where
  fromHaskell hirc = HC_WithMsg (fmap fromHaskell $ lift hirc)

instance FromHaskell a => FromHaskell (IO a) where
  fromHaskell io   = HC_WithMsg (fmap fromHaskell $ liftIO io)

--------------------------------------------------------------------------------
-- Basic type instances

instance FromHaskell String where
  fromHaskell s = HC_Str s

instance FromHaskell (Maybe String) where
  fromHaskell (Just s) = HC_Str s
  fromHaskell _        = HC_Nothing
