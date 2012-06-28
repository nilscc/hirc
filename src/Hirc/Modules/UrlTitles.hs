{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.UrlTitles
    ( urlTitlesModule
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Codec.Binary.UTF8.String
import Data.Char
import Text.HTML.TagSoup
import Text.Regex.Posix

import Network.Curl

import Hirc

--------------------------------------------------------------------------------
-- The module

urlTitlesModule :: Module
urlTitlesModule = Module "UrlTitles" Nothing $ do
  withParams $ \[_,text] -> do
    let urls = filter (=~ "^(http://|https://|www\\.)") (words text)
    case urls of
         (url:_) -> do
           title <- getTitle url
           maybe (return ())
                 (say . ("Title: " ++))
                 title
         _ -> return ()


--------------------------------------------------------------------------------
-- Helper stuff

-- | Combination of performCurl and getTitle
getTitle :: MonadIO m => URLString -> m (Maybe String)
getTitle url = liftIO $ do
  (code, s) <- curlGetString url [ CurlFollowLocation True, CurlMaxFileSize (1000*1000)
                                 , CurlEncoding "UTF-8" ]
  return $
    case code of
         CurlOK -> let t = getTitle' s
                    in do guard (not $ null t)
                          Just $ decodeString t
         _      -> Nothing

-- | Get the title element of our XML data
getTitle' :: String -> String
getTitle'
  = strip
  . innerText
  . findTag "title"
  . findTag "head"
  . parseTags

findTag :: String -> [Tag String] -> [Tag String]
findTag (lower -> t)
  = uncurry (++)
  . second safeHead
  . span      (not . isTagCloseName t . fmap lower)
  . dropWhile (not . isTagOpenName  t . fmap lower)
 where
  safeHead []    = []
  safeHead (x:_) = [x]

lower :: String -> String
lower = map toLower

strip :: String -> String
strip = foldr f "" . dropWhile isSpace
  where -- remove trailing space
        f s ""     | isSpace s = ""
        -- remove multiple spaces
        f s (r:rs) | isSpace s && isSpace r = r:rs
        f s r = s:r
