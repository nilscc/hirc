module Commands.GoogleSearch
    (
      getGoogleSearch
    ) where

import Control.Arrow
import Control.Applicative
import Data.List (foldl')
import Text.JSONb
import Network.Curl

import Utils

import qualified Data.Map                   as M
import qualified Data.Trie                  as T
import qualified Data.ByteString.UTF8       as B8
import qualified Data.ByteString.Lazy.UTF8  as BU

googleSearchUrl query = "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" ++ urlEscape query

type Url = String

-- | Perform curl call and return the first result
getGoogleSearch :: String -> IO (Maybe (Url, String))
getGoogleSearch qry = do
    c <- (second $ decode) <$> curlGetString_ (googleSearchUrl qry) method_GET
    return $ case c of
                  (CurlOK, Right json) -> turnIntoTuple json
                  _                    -> Nothing

turnIntoTuple :: JSON -> Maybe (Url, String)
turnIntoTuple (Object m) = do
    Object rdata <- T.lookup (B8.fromString "responseData") m
    Array res    <- T.lookup (B8.fromString "results") rdata
    case res of
         (Object result: _) -> do
             String url   <- T.lookup (B8.fromString "url") result
             String title <- T.lookup (B8.fromString "title") result
             return (B8.toString url, stripTags $ B8.toString title)
         _ -> Nothing

turnIntoTuple _ = Nothing

stripTags :: String -> String
stripTags = fst . foldl' step ("", False)
  where step (s,False) '<' = (s, True)
        step (s,False)  a  = (s++[a], False)
        step (s,True)  '>' = (s, False)
        step (s,True)   _  = (s, True)
