{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Commands
  ( userCommand
  ) where

import Control.Monad.Reader
import Network.IRC

import Hirc.Types
import Hirc.Messages


--------------------------------------------------------------------------------
-- Running

userCommand :: IsHircCommand cmd
            => cmd
            -> WithMessage ()
userCommand cmd = withParams $ \[_,text] ->
  runC (words text) (toCmd cmd)

runC :: [String] -> HircCommand -> WithMessage ()
runC wrds cmd = case cmd of

  HC_Nothing   -> return ()
  HC_WithMsg h -> h >>= runC wrds
  HC_Lam f     -> 
    case wrds of
         (w:ws) -> local dropWord $ runC ws (f w)
         []     -> return ()
  HC_Lams f    -> runC [] $ f wrds


--------------------------------------------------------------------------------
-- ReaderT modification

dropWord :: Message -> Message
dropWord msg@Message { msg_params = ps } =
  case ps of
       [c,t] -> msg { msg_params = [c, (unwords . drop 1 . words) t] }
       _     -> msg
