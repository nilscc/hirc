{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}

module Hirc.Types.Commands where

import Hirc.Types.Hirc

data HircCommand
  = HC_Nothing
  | HC_Run          (MessageM    HircCommand)
  | HC_Lam          (String   -> HircCommand)
  | HC_Lams         ([String] -> HircCommand)

class IsHircCommand a where
  toCmd :: a -> HircCommand
