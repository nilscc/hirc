{-# LANGUAGE PatternGuards, TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
module Hirc.Modules.Admin.Acid where

import qualified Data.Map as M

import Hirc

type Password = String

data AdminSettings = AdminSettings
  { globalAdmins  :: [Username]
  , localAdmins   :: M.Map Channel [Username]
  , adminPassword :: Maybe String
  }
  deriving (Typeable)


--------------------------------------------------------------------------------
-- ACID functions

isAdmin :: Username -> Maybe Channel -> Query AdminSettings Bool
isAdmin un mc = do
  as <- ask
  let ga = globalAdmins as
      la = localAdmins  as
  return $ un `elem` ga || un `elem` maybe [] (la M.!) mc

authenticate :: Username -> Password -> Update AdminSettings String
authenticate un pw = do
  as <- get -- adminPassword
  case adminPassword as of
       Nothing -> return "Password authentication disabled."
       Just apw
         | pw == apw -> do
           modify $ \as' -> as' { globalAdmins = un : globalAdmins as' }
           return "You're successfully authenticated."
         | otherwise ->
           return "Incorrect password."

{-
authenticateLocal :: Username -> Channel -> Password -> Update AdminSettings (MessageM ())
authenticateLocal un ch pw = do
  mapw <- asks adminPassword
  case mapw of
       Nothing -> return $ answer "Password authentication disabled."
       Just apw
         | pw == apw ->
           modify_ $ \as -> as { localAdmins = ... }
           return $ answer "You're successfully authenticated."
         | otherwise ->
           return $ answer "Incorrect password."
-}

deriveSafeCopy 0 'base ''AdminSettings

makeAcidic ''AdminSettings
           [ 'isAdmin
           , 'authenticate
           ]

