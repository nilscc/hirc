{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hirc.Commands
  ( userCommand
  , userCommand'
  ) where

import qualified Data.ByteString.Char8 as B8
import Network.IRC ( Message(Message, msg_params) )

import Hirc.Types.Commands ( IsHircCommand(..), HircCommand(..) )
import Hirc.Types.Hirc ( ContainsMessage(localMessage), CanSend )
import Hirc.Messages
    ( onCommand, withParams )
import Control.Exception (PatternMatchFail)
import Control.Exception.Peel as CEP
import Data.List.Extra (trim)


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
--
-- To avoid @incomplete pattern@ warnings in GHC, add the following line to
-- the top of your module files:
--
-- > {-# OPTIONS -fno-warn-incomplete-patterns #-}
userCommand :: (CanSend m, IsHircCommand m cmd) => cmd -> m ()
userCommand cmd = onCommand "PRIVMSG" $ withParams $ \[_,text] ->
  runC (words text) (toCmd cmd) `CEP.catch` \PatternMatchFail{} -> return ()

runC :: (ContainsMessage m, MonadFail m) => [String] -> HircCommand m -> m ()
runC wrds cmd = case cmd of
  HC_Nothing -> return ()
  HC_Run h -> h >>= runC wrds
  HC_Lam f -> 
    case wrds of
         (w:ws) -> localMessage dropWord $
                     runC ws (f w)
         []     -> return ()
  HC_Lams f -> runC [] $ f wrds

userCommand' :: CanSend m => (String -> m ()) -> m ()
userCommand' cmd = onCommand "PRIVMSG" $ withParams $ \[_,text] -> cmd (trim text)

--------------------------------------------------------------------------------
-- ReaderT modification

dropWord :: Message -> Message
dropWord msg@Message { msg_params = ps } =
  case ps of
       [c,t] -> msg { msg_params = [c, B8.pack $ (unwords . drop 1 . words) (B8.unpack t)] }
       _     -> msg
