{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, TypeFamilies #-}
{-# OPTIONS -fno-warn-orphans #-}

module Hirc.Types.Instances where

import Control.Concurrent.MState
import Control.Monad.Error
import Control.Monad.IO.Peel
import Control.Monad.Reader
import Control.Monad.Trans.Peel

import Hirc.Types.Commands
import Hirc.Types.Hirc




--------------------------------------------------------------------------------
-- Filter monad instances

instance ContainsHirc HircM where
  askHircSettings = ask
  getHircState    = get
  modifyHircState f = HircM $ modifyM_ f

instance ContainsHirc m => ContainsHirc (ReaderT r m) where
  askHircSettings = lift askHircSettings
  getHircState    = lift getHircState
  modifyHircState = lift . modifyHircState

--instance ContainsHirc (MState s MessageM) where
  --askHircSettings = lift askHircSettings
  --getHircState    = lift getHircState
  --modifyHircState = lift . modifyHircState

instance ContainsHirc m => ContainsHirc (MState t m) where
  askHircSettings = lift askHircSettings
  getHircState    = lift getHircState
  modifyHircState = lift . modifyHircState

instance ContainsMessage MessageM where
  getMessage = ask
  localMessage = local

instance ContainsMessage (MState s MessageM) where
  getMessage = lift ask
  localMessage f s = do
    k <- peel
    join $ lift $ local f (k s)

--------------------------------------------------------------------------------
-- X runs in Y

instance CanRun HircM HircM where
  runInside = id
instance CanRun HircM IO where
  runInside = liftIO

instance CanRun MessageM MessageM where
  runInside = id
instance CanRun MessageM HircM where
  runInside = lift
instance CanRun MessageM IO where
  runInside = lift . runInside

instance CanRun (MState s MessageM) (MState s MessageM) where
  runInside = id
instance CanRun (MState s MessageM) MessageM where
  runInside = lift
instance CanRun (MState s MessageM) HircM where
  runInside = lift . runInside
instance CanRun (MState s MessageM) IO where
  runInside = lift . runInside


--------------------------------------------------------------------------------
-- HircCommand instances

-- Basic type instances

instance IsHircCommand m () where
  toCmd _    = HC_Nothing

-- functions

instance IsHircCommand m a => IsHircCommand m (String -> a) where
  toCmd f    = HC_Lam  (\s  -> toCmd (f s))
instance IsHircCommand m a => IsHircCommand m ([String] -> a) where
  toCmd f    = HC_Lams (\ws -> toCmd (f ws))

-- monads

type ModM s = MState s MessageM

--instance (CanRun m m, IsHircCommand m a) => IsHircCommand m (m a) where
  --toCmd m    = HC_Run $ m >>= return . toCmd

instance IsHircCommand (ModM s) a => IsHircCommand (ModM s) (ModM s a) where
  toCmd modm = mkHcRun modm
instance (CanRun (ModM s) MessageM, IsHircCommand (ModM s) a) => IsHircCommand (ModM s) (MessageM a) where
  toCmd msgm = mkHcRunInside msgm

instance IsHircCommand MessageM a => IsHircCommand MessageM (MessageM a) where
  toCmd msgm = mkHcRun msgm
instance (CanRun MessageM HircM, IsHircCommand MessageM a) => IsHircCommand MessageM (HircM a) where
  toCmd hrcm = mkHcRunInside hrcm

instance IsHircCommand HircM a => IsHircCommand HircM (HircM a) where
  toCmd hrcm = mkHcRun hrcm
instance (CanRun HircM IO, IsHircCommand HircM a) => IsHircCommand HircM (IO a) where
  toCmd iom  = mkHcRunInside iom

mkHcRun :: (Monad m, IsHircCommand m a) => m a -> HircCommand m
mkHcRun f = HC_Run $ f >>= return . toCmd

mkHcRunInside :: (IsHircCommand m a, CanRun m f) => f a -> HircCommand m
mkHcRunInside f = HC_Run $ runInside $ f >>= return . toCmd


--------------------------------------------------------------------------------
-- Log instances

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


--------------------------------------------------------------------------------
-- Managed monad instances

instance ContainsManaged ManagedM where
  getManagedState = get
  askManagedSettings = ask

--instance ContainsManaged HircM where
  --getManagedState = lift get
  --askManagedSettings = lift ask

--------------------------------------------------------------------------------
-- Forkable instances

instance MonadPeelIO m => Forkable (MState t m) where
  forkM'  = forkM
  forkM_' = forkM_
  waitM'  = waitM

instance Forkable HircM where
  forkM'  (HircM t) = HircM $ forkM t
  forkM_' (HircM t) = HircM $ forkM_ t
  waitM'  t         = HircM $ waitM t

instance Forkable ManagedM where
  forkM'  (ManagedM t) = ManagedM $ forkM t
  forkM_' (ManagedM t) = ManagedM $ forkM_ t
  waitM'  t            = ManagedM $ waitM t
