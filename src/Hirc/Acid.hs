{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Hirc.Acid
  ( update, query
  , runQuery
  , openLocalState, openLocalStateFrom, closeAcidState, createCheckpoint
    -- *** Re-exports
  , makeAcidic
  , Update, Query, AcidState, EventResult, EventState
  , Typeable
  , module Data.SafeCopy
  ) where

import Data.Acid hiding (update, query, openLocalState, openLocalStateFrom,
                         closeAcidState, createCheckpoint)
import qualified Data.Acid as A

import Data.SafeCopy
import Data.Typeable
import Control.Monad.State


--------------------------------------------------------------------------------
-- Update & queries

-- | If your module has `AcidState' as its `ModuleState' type you can use
-- `update' and `query' without any further arguments to access your acid module
-- state. See `openLocalState' for an example on how to set it up.
update :: (UpdateEvent e, MonadState (AcidState (EventState e)) m, MonadIO m)
       => e -> m (EventResult e)
update e = do
  a <- get
  liftIO $ A.update a e

query :: (QueryEvent e, MonadState (AcidState (EventState e)) m, MonadIO m)
      => e -> m (EventResult e)
query e = do
  a <- get
  liftIO $ A.query a e


--------------------------------------------------------------------------------
-- Setting it up

-- | These functions are designed to be used with the `Module' system. To
-- make the Acid state available in your module simply put
--
-- > instance IsModule MyModule where
-- >    type ModuleState MyModule = AcidState MyState
-- >    [..]
-- >    initModule (MyModule initialState) = openLocalState initialState
-- >    shutdownModule _ = Just closeAcidState
-- >    [..]
--
-- in your `IsModule' instance.
openLocalState :: (Typeable st, IsAcidic st, MonadIO m)
               => st -> m (AcidState st)
openLocalState i =
  liftIO $ A.openLocalState i

openLocalStateFrom :: (Typeable st, IsAcidic st, MonadIO m)
                   => FilePath -> st -> m (AcidState st)
openLocalStateFrom fp i =
  liftIO $ A.openLocalStateFrom fp i

closeAcidState :: MonadIO m
               => AcidState st -> m ()
closeAcidState a =
  liftIO $ A.closeAcidState a

createCheckpoint :: (MonadState (AcidState st) m, MonadIO m)
                 => m ()
createCheckpoint =
  get >>= liftIO . A.createCheckpoint
