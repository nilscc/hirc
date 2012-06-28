{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hirc.Types.Hirc where

import Data.Time
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.MState
import Control.Monad.IO.Peel
import Control.Monad.Reader
import Control.Monad.Error
import Network.IRC
import System.IO

import qualified Data.Map as M
import qualified Data.Set as S

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
  , eventQueue    :: HircM ()
  , modules       :: [Module]
  }

type HircM = MState HircState (ReaderT HircSettings (ErrorT HircError IO))

data HircError
  = H_NotConnected
  | H_ConnectionLost
  | H_ConnectionFailed
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
  , moduleState     :: M.Map ModuleName ModuleState
  }


--------------------------------------------------------------------------------
-- Message filter monad

type MessageM = ReaderT (Message, Context) HircM

data Context = Context
  { ctxtModule :: Maybe Module
  }

class MonadPeelIO m => Filtered m where
  runFiltered :: m a -> MessageM a


--------------------------------------------------------------------------------
-- Modules

type ModuleName = String

-- | Modules use the `MessageM' monad and can `store' and `load' values from the
-- module state. Each module has its own state that cannot be accessed by other
-- modules.
data Module = Module
  { moduleName    :: ModuleName                                   -- ^ (Unique) module name. This should be the same as the Haskell filename of the module.
  , onNickChange  :: Maybe (Username -> Nickname -> MessageM ())  -- ^ Optional function to be run whenever a user changes his nickname.
                                                                  -- @Nickname@ is the new nickname whereas @Username@ should stay the same.
  , runModule     :: MessageM ()
  }

-- | Any value stored
data ModuleStateValue
  = MSV_String  String
  | MSV_Int     Integer
  | MSV_Bool    Bool
  | MSV_Time    UTCTime
  | MSV_List    List
  | MSV_Map     Map
  | MSV_Maybe   (Maybe ModuleStateValue)
  | MSV_Tup2    (ModuleStateValue, ModuleStateValue)
  | MSV_Tup3    (ModuleStateValue, ModuleStateValue, ModuleStateValue)
  | MSV_Tup4    (ModuleStateValue, ModuleStateValue, ModuleStateValue, ModuleStateValue)
  | MSV_Tup5    (ModuleStateValue, ModuleStateValue, ModuleStateValue, ModuleStateValue, ModuleStateValue)
  | MSV_Set     (S.Set ModuleStateValue)
  deriving (Eq, Ord, Show)

-- | The `Map' type is a weakly typed @String => ModuleStateValue@ map. Type
-- interference will convert `ModuleStateValue's to the corresponding basic
-- Haskell type (if possible) when using the `Map'-functions listed below.
newtype Map = Map { unMSV_Map :: M.Map String ModuleStateValue }
  deriving (Eq, Ord, Show)

-- | Weakly typed @ModuleStateValue@ list. Similar behaviour to `Map'.
newtype List = List { unLSV_List :: [ModuleStateValue] }
  deriving (Eq, Ord, Show)

type ModuleState = M.Map String ModuleStateValue

class Eq a => IsModuleStateValue a where
  toMSV   :: a -> ModuleStateValue
  fromMSV :: ModuleStateValue -> Maybe a


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
