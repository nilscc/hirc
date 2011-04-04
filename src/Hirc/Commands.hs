module Hirc.Commands where

-- import Control.Exception.Peel

import Hirc.Types

--------------------------------------------------------------------------------
-- Running

runC :: [String] -> HircCommand -> WithMessage (Maybe String)
runC wrds cmd = case cmd of

  HC_Str s     -> return $ Just s
  HC_Nothing   -> return Nothing
  HC_WithMsg h -> h >>= runC wrds
  HC_Lam f     -> 
    case wrds of
         (w:ws) -> runC ws (f $ Just w)
         []     -> runC [] (f Nothing)
  HC_Lams f    -> runC [] $ f wrds

-- runCommand ::
