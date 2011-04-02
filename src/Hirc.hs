module Hirc
  ( -- requireHandle

    -- * Types
    -- ** Hirc types
    Hirc
  , HircError (..)
  , HircState (..)
  , HircSettings (..)
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
