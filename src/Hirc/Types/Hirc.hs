{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, RankNTypes,
             TypeFamilies, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hirc.Types.Hirc where

import Control.Applicative ( Alternative )
import Control.Concurrent ( ThreadId, Chan )
import Control.Concurrent.STM ( TVar )
import Control.Monad.Reader
    ( MonadIO, MonadPlus, ReaderT(ReaderT), MonadReader )
import Control.Monad.Except ( ExceptT, MonadError )
import Network.Connection (Connection)
import Network.IRC ( Message, Channel )

import Hirc.Types.Connection
    ( ConnectionCommand, Username, IrcServer )

import Data.Kind (Type)

type NickName = String


--------------------------------------------------------------------------------
-- The Hirc monad

{-
-- we need the error instance here!
instance Error HircError where
  strMsg = H_Other
-}

newtype HircM a = HircM
  { unHircM :: ReaderT HircInstance (ExceptT HircError IO) a }
  deriving ( Monad, MonadIO
           , Functor, Applicative
           , MonadReader HircInstance
           , MonadError HircError
           )

type EventLoop = HircM ()

data HircError
  = H_NotConnected
  | H_ConnectionLost
  | H_ConnectionFailed
  | H_NicknameAlreadyInUse NickName
  | H_UsernameAlreadyInUse Username
  | H_Other String
  deriving (Show, Eq)

--instance Monoid HircError

data LogSettings = LogSettings
  { logLevel      :: Int
  , logPrintLevel :: Int
  , logFile       :: FilePath
  }

data HircDefinition = HircDefinition
  { server        :: IrcServer
  , modules       :: [Module]
  }

data IrcDefinition = IrcDefinition
  { ircServer           :: IrcServer
  , ircChannels         :: [Channel]
  , ircNickname         :: String
  , ircUsername         :: String
  , ircRealname         :: String
  }

data IrcInstance = IrcInstance
  { ircDefinition       :: IrcDefinition

  -- IO & thread communication
  , networkConnection   :: TVar (Maybe Connection)
  , listenThreadId      :: TVar (Maybe ThreadId)
  , cmdThreadId         :: TVar (Maybe ThreadId)

  , cmdChan             :: Chan ConnectionCommand
  , msgChan             :: Chan Message

  -- IRC properties
  , currentChannels     :: TVar [Channel]
  , currentNickname     :: TVar String
  , currentUsername     :: TVar String
  , currentRealname     :: TVar String
  }

data LogInstance = LogInstance
  { logSettings         :: LogSettings

  -- IO & thread communication
  , logThreadId         :: TVar (Maybe ThreadId)
  , logChan             :: Chan (Int,String)
  }

data HircInstance = HircInstance
  { hircDefinition      :: HircDefinition

  , ircInstance         :: TVar (Maybe IrcInstance)
  , logInstance         :: TVar (Maybe LogInstance)

  -- IO & thread communication
  --, errMVar             :: MVar HircError
  --, managedThreadsChan  :: Chan ThreadId
  }

{-
data HircState = HircState
  { connectedHandle :: Maybe Handle
  , ircNickname     :: String
  , ircUsername     :: String
  , ircRealname     :: String
  , runningModules  :: [Module]
  }
-}


--------------------------------------------------------------------------------
-- Message filter monad

type MessageM = ReaderT Message HircM

class MonadReader HircInstance m => ContainsMessage m where
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

type ModuleM        m a = ReaderT (TVar (ModuleState m)) HircM    a
type ModuleMessageM m a = ReaderT (TVar (ModuleState m)) MessageM a

-- | The main module class. The module runs in a `MState' environment with the
-- `ModuleState m' type as state. For a persistent state see the section about
-- the Acid state system.
class IsModule m where
  type ModuleState m :: Type

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

newtype ManagedM a = ManagedM
  { unManagedM :: ReaderT ManagedSettings IO a }
  deriving ( Monad, MonadIO, Functor, Applicative, Alternative
           , MonadReader ManagedSettings
           , MonadPlus
           )

data ManagedSettings = ManagedSettings
  { logChanM       :: Chan (Int,String)
  , logSettingsM   :: LogSettings
  , managedThreads :: Chan ThreadId
  }

--------------------------------------------------------------------------------
-- X runs in Y

class (Monad m, Monad f) => CanRun m f where
  runInside :: f a -> m a
