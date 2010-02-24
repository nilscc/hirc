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
         [Element { elContent = [Text CData { cdData = s }] }] -> justIfNotNull . concatWith " " . map stripSpaces $ lines s
         _ -> Nothing

  where justIfNotNull "" = Nothing
        justIfNotNull a | length a > 150 = Just . (++ "...") $ take 150 a
                        | otherwise      = Just a

        stripSpaces s = foldr step "" $ dropWhile isSpace s
          where step s "" | isSpace s = ""
                          | otherwise = [s]
                step s r = s : r

        concatWith s = foldr step ""
          where step "" b = b
                step a "" = a
                step a b  = a ++ s ++ b

-- | Combination of performCurl and getTitle
getTitleOfUrl :: URLString -> IO (Maybe String)
getTitleOfUrl url = maybe Nothing getTitle `fmap` performCurl url
