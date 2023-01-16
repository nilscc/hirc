{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.H3st
  ( h3stModule
  ) where

import Data.Maybe

import Hirc

h3stUser :: Username
h3stUser = "bokunenji"

h3stModule :: Module
h3stModule = Module "h3st" Nothing $ do

  onCommand "QUIT" $ do
    withNickAndUser $ \n u -> do
      when (u == h3stUser) $ do
        msc <- addKill
        logM 1 $ "`addKill' result: " ++ show msc
        case msc of
             Nothing -> do
               incKills n
               c <- fmap (fromMaybe (0 :: Integer)) $ loadGlobal "kills" ~> n
               sayIn "##norsk" $ n ++ " denies a kill! (" ++ show c ++ " kills denied)"
             Just (n',c) ->
               sayIn "##norsk" $ n' ++ " scores a kill! (" ++ show c ++ " total kills)"
        done

  onCommand "PRIVMSG" $ do
    withNickAndUser $ \n u ->
      if u == h3stUser
         then deleteGlobal "last message"
         else storeGlobal  "last message" n

  onValidPrefix $ userCommand $ \"h3st" "score" ->
    withNickname $ \n -> do
      mc <- loadGlobal "kills" ~> n :: MessageM (Maybe Integer)
      case mc of
           Just c  -> answer $ "Your score is " ++ show c ++ "."
           Nothing -> answer "You haven't scored any kills yet."

addKill :: MessageM (Maybe (Nickname, Integer))
addKill = do
  lm <- loadGlobal "last message"
  logM 1 $ "`addKill', current last message: " ++ show lm
  case lm of
       Nothing -> return Nothing
       Just n  -> do
         incKills n
         fmap (fmap (n,)) $ loadGlobal "kills" ~> n

incKills :: Nickname -> MessageM ()
incKills n =
  updateGlobal "kills" $ \mk -> case mk of
    Nothing -> singletonMap n (1 :: Integer)
    Just m  -> alterMap add' n m
 where
  add' Nothing  = Just (1 :: Integer)
  add' (Just c) = Just (c + 1)
