{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, RankNTypes,
             TypeFamilies, GADTs #-}

module Hirc.Types.Hirc where

import Control.Concurrent
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
  { runningHirc        :: Hirc
  , cmdChan            :: Chan ConnectionCommand
  , msgChan            :: Chan Message
  , errMVar            :: MVar HircError
  , logChanH           :: Chan (Int,String)
  , logSettingsH       :: LogSettings
  , managedThreadsChan :: Chan ThreadId
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

-- | The (abstract) `Module' type. To create a module you first have to define
-- the module type and its state type:
--
-- > data MyModule = MyModule { initialState :: MyState }
-- > data MyState  = MyState  { unMyState :: Int }
-- 
-- Then you'll need the `IsModule' instance definition, here we're using the
-- `AcidState' type to make our state persistent between sessions:
--
-- > instance IsModule MyModule where
-- >   type ModuleState MyModule = AcidState MyState
-- >   moduleName     _ = "My Module"
-- >   initModule     m = openLocalState (initialState m)
-- >   shutdownModule _ = Just closeAcidState
-- >   onStartup      _ = Nothing
-- >   onMessage      _ = Just myOnMessage
-- 
-- Make sure to define the necessary Acid types and instances if you want to use
-- this persistent data storage system:
--
-- > makeAcidic ''MyState ['myStateFunction1, 'myStateFunction2]
-- > deriveSafeCopy 0 'base ''MyState
--
-- For convenience you can define a type for our new module monad:
--
-- > type MyModuleM a = ModuleMessageM MyModule a
--
-- So your module functions look like:
--
-- > myOnMessage :: MyModuleM ()
-- > myOnMessage = do ...
--
-- When you're done you'll have to create the abstract `Module' type and export
-- it to be used in your main hirc program:
--
-- > myModule :: MyState    -- ^ The initial state
-- >          -> Module
-- > myModule initialState' = newModule (MyModule initialState')
data Module where
  Module :: IsModule m => m -> ModuleState m -> Module

type ModuleM        m a = MState (ModuleState m) HircM    a
type ModuleMessageM m a = MState (ModuleState m) MessageM a

-- | The main module class. The module runs in a `MState' environment with the
-- `ModuleState m' type as state. For a persistent state see the section about
-- the Acid state system.
class IsModule m where
  type ModuleState m :: *

  moduleName         :: m -> String
    -- ^ (Unique) module name.

  initModule         :: m -> HircM (ModuleState m)
    -- ^ Since the initiate state is undefined this function will have to
    -- initiate the state and make sure everything is loaded up correctly.

  shutdownModule     :: m -> Maybe (ModuleState m -> HircM ())
    -- ^ Shutdown the module and optionally free/store the state system.

  onStartup          :: m -> Maybe (ModuleM m ())
    -- ^ Run this once after the module has been initialized, for example for
    -- time scheduled jobs. Shares the state with `onMessage'.

  onMessage          :: m -> Maybe (ModuleMessageM m ())
    -- ^ Everytime an IRC message is received this function will be run.

--------------------------------------------------------------------------------
-- The Managed monad

type ManagedM = MState ManagedState (ReaderT ManagedSettings IO)

data ManagedState = ManagedState

data ManagedSettings = ManagedSettings
  { logChanM       :: Chan (Int,String)
  , logSettingsM   :: LogSettings
  , managedThreads :: Chan ThreadId
  }

class ContainsManaged m where
  getManagedState    :: m ManagedState
  askManagedSettings :: m ManagedSettings

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
