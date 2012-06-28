module Hirc.ModuleState
  ( store, load, update, require
  , loadGlobal, storeGlobal, updateGlobal, requireGlobal
  , updateAll
  , (~>)

    -- ** List functions
  , emptyList, concatList, singletonList, appendList, nullList
  , headList, tailList, initList, lengthList, takeList, dropList
  , reverseList, mapList, elemList

    -- ** Map functions
  , emptyMap, insertMap, alterMap, lookupMap, singletonMap, memberMap
  , mapWithKeyMap, adjustMap, deleteMap, elemsMap, keysMap, nullMap
  ) where

import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.MState
import qualified Data.Map as M

import Hirc.Messages
import Hirc.Types
import Hirc.Logging

--------------------------------------------------------------------------------
-- Global state functions

-- | Store a module state value in the global context (i.e. all channels share
-- the same state)
storeGlobal :: IsModuleStateValue a => String -> a -> MessageM ()
storeGlobal k v = do
  mn <- requireModuleName
  modify $ \hs ->
    hs { moduleState = M.alter (Just . M.insert k (toMSV v) . fromMaybe M.empty)
                               mn
                               (moduleState hs) }

-- | Load a module state value from the global context
loadGlobal :: IsModuleStateValue a => String -> MessageM (Maybe a)
loadGlobal k = do
  mn <- requireModuleName
  gets $ onJust (onJust fromMSV . M.lookup k) . M.lookup mn . moduleState
 where
  onJust = (=<<)

-- | Update a module state value in the global context
updateGlobal :: IsModuleStateValue v => String -> (Maybe v -> v) -> MessageM ()
updateGlobal k f = do
  v <- loadGlobal k
  storeGlobal k (f v)

-- | Require a module state value. The action @m ()@ is only run when the value
-- @v@ matches the actual value in the state at key @k@.
requireGlobal :: (IsModuleStateValue a, Filtered m) => String -> a -> m () -> MessageM ()
requireGlobal k v m = do
  realv <- loadGlobal k
  when (Just v == realv) (runFiltered m)


--------------------------------------------------------------------------------
-- Channel specific state functions

-- | Store a module state value in the current channel context (i.e. each
-- channel has it's own state)
store :: IsModuleStateValue v => String -> v -> MessageM ()
store k v = do
  chan <- fmap (fromMaybe "__private__") getCurrentChannel
  updateGlobal "__channels__" $ \mm -> case mm of
    Just m  -> alterMap storeChKV chan m
    Nothing -> singletonMap chan (singletonMap k v)
 where
  storeChKV (Just m) = Just $ insertMap k v m
  storeChKV Nothing  = Just $ singletonMap k v

-- | Load a module state value from the current channel context
load :: IsModuleStateValue v => String -> MessageM (Maybe v)
load k = do
  mchan <- getCurrentChannel
  loadGlobal "__channels__" ~> fromMaybe "__private__" mchan ~> k

-- | Update a module state value in the current channel context
update :: IsModuleStateValue v => String -> (Maybe v -> v) -> MessageM ()
update k f = do
  v <- load k
  store k (f v)

-- | Require a module state value in the current channel context. The action @m
-- ()@ is only run when the value @v@ matches the actual value in the state at
-- key @k@.
require :: (IsModuleStateValue v, Filtered m) => String -> v -> m () -> MessageM ()
require k v m = do
  realv <- load k
  when (Just v == realv) (runFiltered m)


--------------------------------------------------------------------------------
-- Functions to map over all channels

-- | Map over all channels and update all states
updateAll :: IsModuleStateValue v => String -> (String -> v -> v) -> MessageM ()
updateAll k f = do
  updateGlobal "__channels__" $ \mm -> case mm of
    Just m  -> mapWithKeyMap (\c -> adjustMap (f c) k) m
    Nothing -> emptyMap


--------------------------------------------------------------------------------
-- other

getModuleName :: MessageM (Maybe String)
getModuleName = fmap (fmap moduleName . ctxtModule . snd) ask

requireModuleName :: MessageM String
requireModuleName = do
  mn <- getModuleName
  maybe (logM 2 "`requireModuleName': No module name!" >> mzero) return mn

-- | Combinator to easily access nested `Map's in a module state, e.g.
--
-- > playername <- loadGloabl "__channels__" ~> channelname ~> "players" ~> "name"
(~>) :: IsModuleStateValue a => MessageM (Maybe Map) -> String -> MessageM (Maybe a)
loadCmd ~> k = do
  mres <- loadCmd
  return $ do
    res <- mres
    msv <- lookupMap k res
    fromMSV msv

--------------------------------------------------------------------------------
-- `List' functions

emptyList :: List
emptyList = List []

concatList :: IsModuleStateValue v => v -> List -> List
concatList v (List l) = List ((toMSV v) : l)

singletonList :: IsModuleStateValue v => v -> List
singletonList v = concatList v emptyList

appendList :: List -> List -> List
appendList (List l1) (List l2) = List (l1 ++ l2)

nullList :: List -> Bool
nullList (List l) = null l

headList :: IsModuleStateValue v => List -> Maybe v
headList (List l)
  | null l    = Nothing
  | otherwise = fromMSV (head l)

tailList :: List -> List
tailList (List l) = List (tail l)

initList :: List -> List
initList (List l) = List (init l)

lengthList :: List -> Int
lengthList (List l) = length l

takeList :: Int -> List -> List
takeList i (List l) = List (take i l)

dropList :: Int -> List -> List
dropList i (List l) = List (drop i l)

reverseList :: List -> List
reverseList (List l) = List (reverse l)

-- | Map over the list, the mapping function is only applied if the conversion
-- from the @ModuleStateValue@ to @v@ is not @Nothing@.
mapList :: (IsModuleStateValue v, IsModuleStateValue w) => (v -> w) -> List -> List
mapList f (List l) = List (map (\v -> fromMaybe v $ fmap (toMSV . f) $ fromMSV v) l)

elemList :: IsModuleStateValue v => v -> List -> Bool
elemList v (List l) = toMSV v `elem` l

--------------------------------------------------------------------------------
-- `Map' functions

emptyMap :: Map
emptyMap = Map M.empty

insertMap :: IsModuleStateValue v => String -> v -> Map -> Map
insertMap k v (Map m) = Map (M.insert k (toMSV v) m)

alterMap :: IsModuleStateValue v => (Maybe v -> Maybe v) -> String -> Map -> Map
alterMap f k (Map m) = Map (M.alter (fmap toMSV . f . join . fmap fromMSV) k m)

lookupMap :: IsModuleStateValue v => String -> Map -> Maybe v
lookupMap k (Map m) = join . fmap fromMSV $ M.lookup k m

singletonMap :: IsModuleStateValue v => String -> v -> Map
singletonMap k v = Map (M.singleton k (toMSV v))

memberMap :: String -> Map -> Bool
memberMap k (Map m) = M.member k m

mapWithKeyMap :: (IsModuleStateValue v, IsModuleStateValue w) => (String -> v -> w) -> Map -> Map
mapWithKeyMap f (Map m) = Map (M.mapWithKey (\k v -> fromMaybe v $ fmap (toMSV . f k) $ fromMSV v) m)

adjustMap :: IsModuleStateValue v => (v -> v) -> String -> Map -> Map
adjustMap f k (Map m) = Map (M.adjust (\v -> fromMaybe v $ fmap (toMSV . f) $ fromMSV v) k m)

deleteMap :: String -> Map -> Map
deleteMap k (Map m) = Map (M.delete k m)

elemsMap :: IsModuleStateValue v => Map -> [v]
elemsMap (Map m) = catMaybes . map fromMSV $ M.elems m

keysMap :: Map -> [String]
keysMap (Map m) = M.keys m

nullMap :: Map -> Bool
nullMap (Map m) = M.null m
