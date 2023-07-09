{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-orphans #-}

module Hirc.Types.Instances where

import Control.Concurrent.STM (TVar, atomically, readTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Peel (MonadPeelIO (peelIO))
import Control.Monad.RWS (MonadState)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), mapReaderT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Hirc.Connection (awaitTVar)
import Hirc.Types.Hirc (CanRun (..), CanSend, ContainsIrcInstance (askIrcInstance), ContainsLogInstance (askLogInstance), ContainsMessage (getMessage, localMessage), HircInstance (ircInstance, logInstance), HircM (..), MessageM (MessageM, unMessageM))

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

instance MonadPeelIO HircM where
  peelIO = HircM $ do
    k <- peelIO
    return $ \(HircM m) -> HircM <$> k m

instance MonadPeelIO MessageM where
  peelIO = MessageM $ do
    k <- peelIO
    return $ \(MessageM m) -> MessageM <$> k m

instance CanSend MessageM

instance CanSend m => CanSend (ReaderT r m)

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