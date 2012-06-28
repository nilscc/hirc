{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Commands
  ( userCommand
  ) where

import Control.Arrow
import Control.Monad.Reader
import Network.IRC

import Hirc.Types
import Hirc.Messages


--------------------------------------------------------------------------------
-- Running

userCommand :: IsHircCommand cmd
            => cmd
            -> MessageM ()
userCommand cmd = withParams $ \[_,text] ->
  runC (words text) (toCmd cmd)

runC :: [String] -> HircCommand -> MessageM ()
runC wrds cmd = case cmd of

  HC_Nothing   -> return ()
  HC_Run h     -> h >>= runC wrds
  HC_Lam f     -> 
    case wrds of
         (w:ws) -> local (first dropWord) $ catchPatternException $
                     runC ws (f w)
         []     -> return ()
  HC_Lams f    -> catchPatternException $
                    runC [] $ f wrds


--------------------------------------------------------------------------------
-- ReaderT modification

dropWord :: Message -> Message
dropWord msg@Message { msg_params = ps } =
  case ps of
       [c,t] -> msg { msg_params = [c, (unwords . drop 1 . words) t] }
       _     -> msg
