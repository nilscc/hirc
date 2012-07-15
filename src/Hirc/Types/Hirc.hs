{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, RankNTypes,
             TypeFamilies, GADTs #-}

module Hirc.Types.Hirc where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.MState
import Control.Monad.IO.Peel
import Control.Monad.Reader
import Control.Monad.Error
import Network.IRC
import System.IO

import Hirc.Types.Connection


type NickName = String


--------------------------------------------------------------------------------
-- The Hirc monad

data Hirc = Hirc
  { server        :: IrcServer
  , channels      :: [Channel]
  , nickname      :: String
  , username      :: String
  , realname      :: String
  , modules       :: [Module]
  }

type HircM = MState HircState (ReaderT HircSettings (ErrorT HircError IO))

class (LogM m, MonadPeelIO m, Functor m) => ContainsHirc m where
  askHircSettings :: m HircSettings
  getHircState    :: m HircState
  modifyHircState :: (HircState -> HircState) -> m ()

type EventLoop = HircM ()

data HircError
  = H_NotConnected
  | H_ConnectionLost
  | H_ConnectionFailed
  | H_NicknameAlreadyInUse NickName
  | H_UsernameAlreadyInUse Username
  | H_Other String
  deriving (Show, Eq)

data HircSettings = HircSettings
  { runningHirc     :: Hirc
  , cmdChan         :: Chan ConnectionCommand
  , msgChan         :: Chan Message
  , errMVar         :: MVar HircError
  , logChanH        :: Chan (Int,String)
  , logSettingsH    :: LogSettings
  }

data HircState = HircState
  { connectedHandle :: Maybe Handle
  , ircNickname     :: String
  , ircUsername     :: String
  , ircRealname     :: String
  , runningModules  :: [Module]
  }


--------------------------------------------------------------------------------
-- Message filter monad

type MessageM = ReaderT Message HircM

class ContainsHirc m => ContainsMessage m where
  getMessage :: m Message
  localMessage :: (Message -> Message) -> m a -> m a


--------------------------------------------------------------------------------
-- Modules

data Module where
  Module :: IsModule m => m -> ModuleState m -> Module

type ModuleM m a = MState (ModuleState m) MessageM a

class IsModule m where
  type ModuleState m :: *

  moduleName         :: m -> String
    -- ^ (Unique) module name. This should be the same as the Haskell filename of the module.

  onNickChange       :: m -> Maybe (Username -> Nickname -> ModuleM m ())
    -- ^ Optional function to be run whenever a user changes his nickname.
    -- @Nickname@ is the new nickname whereas @Username@ should stay the same.

  initModule         :: m -> HircM (ModuleState m)
    -- ^ initiate the state

  runModule          :: m -> ModuleM m ()


--------------------------------------------------------------------------------
-- The Managed monad

type Managed = MState ManagedState (ReaderT ManagedSettings IO)

data ManagedState = ManagedState

data ManagedSettings = ManagedSettings
  { logChanM      :: Chan (Int,String)
  , logSettingsM  :: LogSettings
  }


--------------------------------------------------------------------------------
-- Logging

class MonadPeelIO m => LogM m where
  logChan     :: m (Chan (Int,String))
  logSettings :: m LogSettings

data LogSettings = LogSettings
  { logLevel      :: Int
  , logPrintLevel :: Int
  , logFile       :: FilePath
  }


--------------------------------------------------------------------------------
-- X runs in Y

class (Monad m, Monad f) => CanRun m f where
  runInside :: f a -> m a
