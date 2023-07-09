{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Hirc.Types.Commands where

data HircCommand m
  = HC_Nothing
  | HC_Run (m (HircCommand m))
  | HC_Lam (String -> HircCommand m)
  | HC_Lams ([String] -> HircCommand m)

class IsHircCommand m a where
  toCmd :: a -> HircCommand m
