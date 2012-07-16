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
