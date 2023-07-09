{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hirc.Commands
  ( userCommand
  ) where

import qualified Data.ByteString.Char8 as B8
import Network.IRC ( Message(Message, msg_params) )

import Hirc.Types.Hirc ( ContainsMessage(localMessage), CanSend )
import Hirc.Messages
    ( onCommand, withParams )
import Control.Exception (PatternMatchFail)
import Control.Exception.Peel as CEP
import Data.List.Extra (trim)


--------------------------------------------------------------------------------
-- Running

userCommand :: CanSend m => (String -> m ()) -> m ()
userCommand cmd = onCommand "PRIVMSG" $ withParams $ \[_,text] -> cmd (trim text)

--------------------------------------------------------------------------------
-- ReaderT modification

dropWord :: Message -> Message
dropWord msg@Message { msg_params = ps } =
  case ps of
       [c,t] -> msg { msg_params = [c, B8.pack $ (unwords . drop 1 . words) (B8.unpack t)] }
       _     -> msg
