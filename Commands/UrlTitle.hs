module Commands.UrlTitle
    (
    -- * Pure operations
      getTitle
    -- * IO part
    , performCurl
    , getTitleOfUrl
    ) where

import Control.Monad
import Data.Char
import Network.Curl hiding (Content)
import Text.XML.Light

type XML = [Content]

-- | Run curl and return the parsed XML
performCurl :: URLString -> IO (Maybe XML)
performCurl url = do
    (r,s) <- curlGetString url [CurlFollowLocation True, CurlMaxRedirs 20]
    return $ case r of
                  CurlOK -> Just $ parseXML s
                  _      -> Nothing

-- | Get the title element of our XML data
getTitle :: XML -> Maybe String
getTitle xml =
    case concatMap (filterElementsName $ (== "title") . map toLower . qName) (onlyElems xml) of
         [Element { elContent = [Text CData { cdData = s }] }] -> Just s
         _ -> Nothing

-- | Combination of performCurl and getTitle
getTitleOfUrl :: URLString -> IO (Maybe String)
getTitleOfUrl url = maybe Nothing getTitle `fmap` performCurl url
