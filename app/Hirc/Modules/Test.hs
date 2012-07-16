{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-unused-do-bind
            -fno-warn-incomplete-patterns
            #-}

module Hirc.Modules.Test
  ( testModule
  ) where

import Hirc

data TestModule = TestModule

newtype TestState = TestState { unTS :: Int }
  deriving Typeable

deriveSafeCopy 0 'base ''TestState

incState :: Update TestState ()
incState = modify (TestState . (+1) . unTS)

getState :: Query TestState Int
getState = asks unTS

makeAcidic ''TestState ['incState, 'getState]

instance IsModule TestModule where
  type ModuleState TestModule = AcidState TestState
  moduleName     _ = "Test Module"
  onNickChange   _ = Nothing
  initModule     _ = openLocalState $ TestState 0
  runModule      _ = runTestModule
  shutdownModule _ = Just closeAcidState

type TestM a = ModuleM TestModule a

runTestModule :: TestM ()
runTestModule = do

  onValidPrefix $ do
    userCommand $ \"test" "++"   -> incTM
    userCommand $ \"test" "show" -> showTM

 where
  incTM, showTM :: TestM ()
  incTM = do
    update IncState
    i <- query GetState
    answer $ "Test state is now at " ++ show i ++ "."
  showTM = do
    i <- query GetState
    answer $ "Test state is currently at " ++ show i ++ "."


testModule :: Module
testModule = newModule TestModule
