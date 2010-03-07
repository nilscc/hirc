{-# LANGUAGE ViewPatterns #-}

module Commands.Json
    (
      readCommands
    , (~>)
    ) where

import Data.List (find, isPrefixOf)
import Data.Char
import Text.JSON

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
    f <- readFile fp
    return $ case decodeStrict f of
                  Ok json -> do -- Maybe monad
                      JSString js <- json ~> jStr key
                      return $ fromJSString js
                  _       -> Nothing


