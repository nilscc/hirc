{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, RankNTypes,
             TypeFamilies, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hirc.Types.Hirc where

import Control.Applicative ( Alternative )
import Control.Concurrent ( ThreadId, Chan )
import Control.Concurrent.STM ( TVar )
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
    ( ReaderT(ReaderT), MonadReader )
import Control.Monad.Except ( ExceptT )
import Control.Monad.Error.Class (MonadError)
import Control.Exception (PatternMatchFail, IOException)
import Data.Kind (Type)
import Network.Connection (Connection, ConnectionContext)
import Network.IRC ( Message, Channel )

import Hirc.Types.Connection
    ( ConnectionCommand, UserName, NickName, RealName, ChannelName, IrcServer )
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.RWS (MonadState)

--------------------------------------------------------------------------------
-- The Hirc monad

newtype HircM a = HircM
  { unHircM :: ReaderT HircInstance (ExceptT HircError IO) a }
  deriving ( Monad, MonadIO, MonadFail
           , Functor, Applicative
           , MonadReader HircInstance
           , MonadError HircError
           )

type EventLoop = HircM ()

data HircError
  = HircNotConnected
  | HircConnectionLost
  | HircConnectionFailed
  | HircNicknameAlreadyInUse NickName
  | HircUsernameAlreadyInUse UserName
  | HircOther String
  deriving (Show, Eq)

--instance Monoid HircError

data LogSettings = LogSettings
  { logLevel      :: Int
  , logPrintLevel :: Int
  , logFile       :: FilePath
  }

data IrcDefinition = IrcDefinition
  { ircServer           :: IrcServer
  , ircChannels         :: [ChannelName]
  , ircNickname         :: String
  , ircUsername         :: String
  , ircRealname         :: String
  }

data IrcInstance = IrcInstance
  { ircInstanceServer   :: IrcServer

  -- IO & thread communication
  , connectionContext   :: TVar (Maybe ConnectionContext)
  , networkConnection   :: TVar (Maybe Connection)
  , listenThreadId      :: TVar (Maybe ThreadId)
  , cmdThreadId         :: TVar (Maybe ThreadId)

  , cmdChan             :: Chan ConnectionCommand
  , msgChan             :: Chan Message

  -- IRC properties
  , currentChannels     :: TVar [ChannelName]
  , currentNickname     :: TVar NickName
  , currentUsername     :: TVar UserName
  , currentRealname     :: TVar RealName
  }

type LogDefinition = LogSettings

data LogInstance = LogInstance
  { logSettings         :: LogSettings

  -- IO & thread communication
  , logThreadId         :: TVar (Maybe ThreadId)
  , logChan             :: Chan (Int,String)
  }

data HircDefinition = HircDefinition
  { modulesDefinition   :: [Module]
  , ircDefinition       :: IrcDefinition
  , logDefinition       :: LogDefinition
  }
data HircInstance = HircInstance
  { modules             :: TVar [Module]
  , ircInstance         :: TVar (Maybe IrcInstance)
  , logInstance         :: TVar (Maybe LogInstance)

  -- IO & thread communication
  --, errMVar             :: MVar HircError
  --, managedThreadsChan  :: Chan ThreadId
  }

--------------------------------------------------------------------------------
-- Message filter monad

class Monad m => ContainsMessage m where
  getMessage :: m Message
  localMessage :: (Message -> Message) -> m a -> m a

class MonadIO m => ContainsIrcInstance m where
  askIrcInstance :: m IrcInstance

class MonadIO m => ContainsLogInstance m where
  askLogInstance :: m (Maybe LogInstance)

-- | Type alias for monads which can send IRC messages
class (ContainsIrcInstance m, ContainsMessage m, MonadFail m) => CanSend m where

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

type ModuleM m a = ReaderT (TVar (ModuleState m)) HircM a

newtype MessageM a = MessageM { unMessageM :: ReaderT Message (MaybeT HircM) a }
  deriving ( Monad, MonadIO, MonadFail
           , MonadPlus, Alternative
           , Functor, Applicative
           , MonadReader Message
           , MonadError HircError
           )

type ModuleMessageM m a = ReaderT (TVar (ModuleState m)) MessageM a

-- | The main module class. The module runs in a `MState' environment with the
-- `ModuleState m' type as state. For a persistent state see the section about
-- the Acid state system.
class IsModule m where
  type ModuleState m = (s :: Type) | s -> m

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
