{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, TypeFamilies, ConstraintKinds,
             FlexibleContexts #-}
{-# OPTIONS -fno-warn-orphans #-}

module Hirc.Types.Instances where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Reader ( ReaderT (runReaderT), MonadReader (ask, local), mapReaderT )

import Hirc.Types.Commands ( IsHircCommand(..), HircCommand(..) )
import Hirc.Types.Hirc ( CanRun(..), MessageM (unMessageM, MessageM), HircM (unHircM), ContainsMessage (getMessage, localMessage), ContainsIrcInstance (askIrcInstance), HircInstance (ircInstance, logInstance), ContainsLogInstance (askLogInstance), CanSend )
import Control.Concurrent.STM (atomically, readTVarIO, TVar)
import Hirc.Connection (awaitTVar)
import Control.Monad.RWS (MonadState)

--------------------------------------------------------------------------------
-- Filter monad instances

instance ContainsIrcInstance HircM where
  askIrcInstance = do
    hinst <- ask
    liftIO $ atomically $ awaitTVar (ircInstance hinst)

instance ContainsIrcInstance MessageM where
  askIrcInstance = MessageM . lift . lift $ askIrcInstance

instance ContainsIrcInstance m => ContainsIrcInstance (ReaderT r m) where
  askIrcInstance = lift askIrcInstance



instance ContainsLogInstance HircM where
  askLogInstance = do
    hinst <- ask
    liftIO $ readTVarIO (logInstance hinst)

instance ContainsLogInstance m => ContainsLogInstance (ReaderT r m) where
  askLogInstance = lift askLogInstance

instance ContainsLogInstance MessageM where
  askLogInstance = MessageM . lift . lift $ askLogInstance



instance ContainsMessage MessageM where
  getMessage = ask
  localMessage = local

instance (ContainsMessage m) => ContainsMessage (ReaderT r m) where
  getMessage = lift getMessage
  localMessage f = mapReaderT (localMessage f)


instance CanSend MessageM where
instance CanSend m => CanSend (ReaderT r m) where

{-
instance ContainsMessage (MState s MessageM) where
  getMessage = lift ask
  localMessage f s = do
    k <- peel
    join $ lift $ local f (k s)
-}

--------------------------------------------------------------------------------
-- X runs in Y

instance CanRun HircM HircM where
  runInside = id
instance CanRun HircM IO where
  runInside = liftIO

instance CanRun MessageM MessageM where
  runInside = id
instance CanRun MessageM HircM where
  runInside = MessageM . lift . lift
instance CanRun MessageM IO where
  runInside = MessageM . lift . lift . runInside

{-
instance CanRun (MState s MessageM) (MState s MessageM) where
  runInside = id
instance CanRun (MState s MessageM) MessageM where
  runInside = lift
instance CanRun (MState s MessageM) HircM where
  runInside = lift . runInside
instance CanRun (MState s MessageM) IO where
  runInside = lift . runInside
-}


--------------------------------------------------------------------------------
-- HircCommand instances

-- Basic type instances

instance IsHircCommand m () where
  toCmd _    = HC_Nothing

-- functions

instance IsHircCommand m a => IsHircCommand m (String -> a) where
  toCmd f    = HC_Lam  (toCmd . f)
instance IsHircCommand m a => IsHircCommand m ([String] -> a) where
  toCmd f    = HC_Lams (toCmd . f)

-- monads

type ModM s = ReaderT s MessageM

--instance (CanRun m m, IsHircCommand m a) => IsHircCommand m (m a) where
  --toCmd m    = HC_Run $ m >>= return . toCmd

instance IsHircCommand (ModM s) a => IsHircCommand (ModM s) (ModM s a) where
  toCmd modm = mkHcRun modm
instance (CanRun (ModM s) MessageM, IsHircCommand (ModM s) a) => IsHircCommand (ModM s) (MessageM a) where
  toCmd = mkHcRunInside

instance IsHircCommand MessageM a => IsHircCommand MessageM (MessageM a) where
  toCmd msgm = mkHcRun msgm
instance (CanRun MessageM HircM, IsHircCommand MessageM a) => IsHircCommand MessageM (HircM a) where
  toCmd = mkHcRunInside

instance IsHircCommand HircM a => IsHircCommand HircM (HircM a) where
  toCmd = mkHcRun
instance (CanRun HircM IO, IsHircCommand HircM a) => IsHircCommand HircM (IO a) where
  toCmd = mkHcRunInside

mkHcRun :: (Monad m, IsHircCommand m a) => m a -> HircCommand m
mkHcRun f = HC_Run $ toCmd <$> f

mkHcRunInside :: (IsHircCommand m a, CanRun m f) => f a -> HircCommand m
mkHcRunInside f = HC_Run $ runInside $ toCmd <$> f


--------------------------------------------------------------------------------
-- Log instances

{-
instance LogM HircM where
  logChan     = asks logChanH
  logSettings = asks logSettingsH

instance LogM ManagedM where
  logChan     = asks logChanM
  logSettings = asks logSettingsM

instance (LogM m) => LogM (MState s m) where
  logChan     = lift logChan
  logSettings = lift logSettings

instance (LogM m) => LogM (ReaderT t m) where
  logChan     = lift logChan
  logSettings = lift logSettings
-}


--------------------------------------------------------------------------------
-- Managed monad instances

{-
instance ContainsManaged ManagedM where
  getManagedState = get
  askManagedSettings = ask

--instance ContainsManaged HircM where
  --getManagedState = lift get
  --askManagedSettings = lift ask
-}