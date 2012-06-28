{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Hirc.Types.Instances where

import Data.Maybe
import Control.Monad.Error
import Control.Monad.Reader
import Data.Time

import qualified Data.Set as S

import Hirc.Types.Commands
import Hirc.Types.Hirc


instance Error HircError where
  strMsg = H_Other


--------------------------------------------------------------------------------
-- Filter monad instances

instance Filtered HircM where
  runFiltered = lift

instance Filtered MessageM where
  runFiltered = id


--------------------------------------------------------------------------------
-- ModuleStateValue instances

instance IsModuleStateValue ModuleStateValue where
  toMSV = id
  fromMSV = Just

instance IsModuleStateValue [Char] where
  toMSV = MSV_String
  fromMSV (MSV_String s) = Just s
  fromMSV _ = Nothing

instance IsModuleStateValue Integer where
  toMSV = MSV_Int
  fromMSV (MSV_Int i) = Just i
  fromMSV _ = Nothing

instance IsModuleStateValue Bool where
  toMSV = MSV_Bool
  fromMSV (MSV_Bool b) = Just b
  fromMSV _ = Nothing

instance IsModuleStateValue UTCTime where
  toMSV = MSV_Time
  fromMSV (MSV_Time t) = Just t
  fromMSV _ = Nothing

instance (IsModuleStateValue v1, IsModuleStateValue v2) => IsModuleStateValue (v1, v2) where
  toMSV (v1, v2) = MSV_Tup2 (toMSV v1, toMSV v2)
  fromMSV (MSV_Tup2 (m1,m2)) = do v1 <- fromMSV m1
                                  v2 <- fromMSV m2
                                  Just (v1,v2)
  fromMSV _ = Nothing

instance (IsModuleStateValue v1, IsModuleStateValue v2, IsModuleStateValue v3) => IsModuleStateValue (v1, v2, v3) where
  toMSV (v1, v2, v3) = MSV_Tup3 (toMSV v1, toMSV v2, toMSV v3)
  fromMSV (MSV_Tup3 (m1,m2,m3)) = do
    v1 <- fromMSV m1
    v2 <- fromMSV m2
    v3 <- fromMSV m3
    Just (v1,v2,v3)
  fromMSV _ = Nothing

instance (IsModuleStateValue v1, IsModuleStateValue v2, IsModuleStateValue v3, IsModuleStateValue v4) => IsModuleStateValue (v1, v2, v3, v4) where
  toMSV (v1, v2, v3, v4) = MSV_Tup4 (toMSV v1, toMSV v2, toMSV v3, toMSV v4)
  fromMSV (MSV_Tup4 (m1,m2,m3,m4)) = do
    v1 <- fromMSV m1
    v2 <- fromMSV m2
    v3 <- fromMSV m3
    v4 <- fromMSV m4
    Just (v1,v2,v3,v4)
  fromMSV _ = Nothing

instance (IsModuleStateValue v1, IsModuleStateValue v2, IsModuleStateValue v3, IsModuleStateValue v4, IsModuleStateValue v5) => IsModuleStateValue (v1, v2, v3, v4, v5) where
  toMSV (v1, v2, v3, v4, v5) = MSV_Tup5 (toMSV v1, toMSV v2, toMSV v3, toMSV v4, toMSV v5)
  fromMSV (MSV_Tup5 (m1,m2,m3,m4,m5)) = do
    v1 <- fromMSV m1
    v2 <- fromMSV m2
    v3 <- fromMSV m3
    v4 <- fromMSV m4
    v5 <- fromMSV m5
    Just (v1,v2,v3,v4,v5)
  fromMSV _ = Nothing

instance IsModuleStateValue Map where
  toMSV = MSV_Map
  fromMSV (MSV_Map m) = Just m
  fromMSV _ = Nothing

-- instance (IsModuleStateValue v) => IsModuleStateValue (M.Map String v) where
  -- toMSV = MSV_Map . M.map toMSV
  -- fromMSV (MSV_Map m) = Just $ M.mapMaybe fromMSV m
  -- fromMSV _ = Nothing

instance (IsModuleStateValue v, Ord v) => IsModuleStateValue (S.Set v) where
  toMSV = MSV_Set . S.map toMSV
  fromMSV (MSV_Set s) = Just $ S.map fromJust $ S.delete Nothing $ S.map fromMSV s
  fromMSV _ = Nothing


--------------------------------------------------------------------------------
-- HircCommand instances

-- Basic type instances

instance IsHircCommand () where
  toCmd _ = HC_Nothing

-- functions

instance IsHircCommand a => IsHircCommand (String -> a) where
  toCmd f    = HC_Lam     (\s  -> toCmd (f s))

instance IsHircCommand a => IsHircCommand ([String] -> a) where
  toCmd f    = HC_Lams    (\ws -> toCmd (f ws))

-- monads

instance IsHircCommand a => IsHircCommand (MessageM a) where
  toCmd wm   = HC_Run     (fmap toCmd wm)

instance IsHircCommand a => IsHircCommand (HircM a) where
  toCmd hirc = HC_Run     (fmap toCmd $ lift $ hirc)

instance IsHircCommand a => IsHircCommand (IO a) where
  toCmd io   = HC_Run     (fmap toCmd $ liftIO io)


--------------------------------------------------------------------------------
-- Log instances

instance LogM MessageM where
  logChan     = lift logChan
  logSettings = lift logSettings

instance LogM HircM where
  logChan     = asks logChanH
  logSettings = asks logSettingsH

instance LogM Managed where
  logChan     = asks logChanM
  logSettings = asks logSettingsM
