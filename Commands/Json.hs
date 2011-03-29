{-# LANGUAGE ViewPatterns #-}

module Commands.Json
    (
      readCommands
    , (~>)
    ) where

import Data.List (find, isPrefixOf)
import Data.Char
import Text.JSON
import System.Directory
import System.Random

-- | Get a value from a JSValue by its key
(~>) :: JSValue -> JSValue -> Maybe JSValue
JSObject o ~> JSString a = snd `fmap` find (matches . fst) (fromJSObject o)
  where matches (map toLower -> s) = s `isPrefixOf` map toLower (fromJSString a) || map toLower (fromJSString a) `isPrefixOf` s

JSArray  a ~> b
    | b `elem` a = Just b
    | otherwise  = Nothing
a ~> b
    | a == b     = Just b
    | otherwise  = Nothing

jStr :: String -> JSValue
jStr = JSString . toJSString

jObj :: [(String, JSValue)] -> JSValue
jObj = JSObject . toJSObject

readCommands :: FilePath -> String -> IO (Maybe String)
readCommands fp key = do
    e <- doesFileExist fp
    if not e then
      return Nothing
     else do
      f <- readFile fp
      case decodeStrict f of
           Ok json -> case json ~> jStr key of
                           Just (JSString js) -> return . Just $ fromJSString js
                           Just (JSArray a@(x:xs)) -> do
                             n <- randomRIO (0, length a - 1)
                             case a !! n of
                                  JSString js -> return . Just $ fromJSString js
                                  _ -> return Nothing
                           _ -> return Nothing
           _       -> return $ Nothing


