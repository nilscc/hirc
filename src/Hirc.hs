module Hirc
  ( -- requireHandle

    -- * Types
    -- ** Hirc types & functions
    Hirc
  , HircError (..)
  , HircState (..)
  , HircSettings (..)

  , getNickname
  , getUsername
  , getRealname
  , setNickname

    -- ** Managed types
  , Managed
  , ManagedState (..)
  , ManagedSettings (..)

    -- ** Connection types
  , IrcServer (..)
  , Reconnect (..)
  , ConnectionCommand (..)
  , Nickname
  , Username
  , Realname
  , To

    -- ** Logging
  , LogM (..)
  , LogSettings (..)

    -- * Reexports
  , module Control.Concurrent.MState
  , module Control.Monad.Reader
  , module Control.Exception.Peel
  ) where

import Control.Concurrent.MState
import Control.Monad.Reader
import Control.Exception.Peel

import Types

setNickname :: String -> Hirc ()
setNickname n = modifyM_ $ \s -> s { ircNickname = n }

getNickname :: Hirc String
getNickname = gets ircNickname

getUsername :: Hirc String
getUsername = gets ircUsername

getRealname :: Hirc String
getRealname = gets ircRealname
