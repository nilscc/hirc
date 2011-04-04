{-# LANGUAGE ViewPatterns #-}
-- TODO: Fix!

module Commands.UrlTitle
    ( getTitle
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Text.HTML.TagSoup

import Network.Curl

-- | Combination of performCurl and getTitle
getTitle :: MonadIO m => URLString -> m (Maybe String)
getTitle url = liftIO $ do
  (code, s) <- curlGetString url [CurlFollowLocation True]
  return $
    case code of
         CurlOK -> let t = getTitle' s
                    in do guard (not $ null t)
                          Just t
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
