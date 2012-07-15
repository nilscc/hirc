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

-- | Handle user commands. This can be done by pattern matching (pattern
-- matching exceptions are caught such that this can be done safely), e.g.:
--
-- > userCommand $ \"translate" lang1 lang2 (unwords -> text) -> do
-- >   ...
--
-- Here @\"translate\"@ is matched with the actual word \"translate\", @lang1@
-- and @lang2@ are the two words following after @\"translate\"@ and the
-- @-XViewPattern@ extension @(unwords -> text)@ applies the function @unwords@
-- to all remaining words of this user command. For example:
--
-- > translate en de this bot is so cool
--
-- turns into
--
-- > lang1 = "en"
-- > lang2 = "de"
-- > text  = "this bot is so cool"
--
-- If \"translate\" doesn't match the first word (or the rest of the pattern
-- doesn't match), the whole user command is ignored.
userCommand :: IsHircCommand cmd
            => cmd
            -> MessageM ()
userCommand cmd = onCommand "PRIVMSG" $ withParams $ \[_,text] ->
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
