-- TODO: Fix!

module Commands.UrlTitle
    (
    -- * Pure operations
      getTitle
    -- * IO part
    , performCurl
    , getTitleOfUrl
    ) where

import Data.Char
import Text.XML.Light

import Commands.Curl

type URLString = String

-- | Get the title element of our XML data
getTitle :: [Content] -> Maybe String
getTitle xml =
    case concatMap (filterElementsName $ (== "title") . map toLower . qName) (onlyElems xml) of
         [Element { elContent = cont }] ->
             case onlyText cont of
                  (CData { cdData = s } :_) ->
                      justIfNotNull . concatWith " " . map stripSpaces $ lines s
                  _ -> Nothing
         _ -> Nothing

  where justIfNotNull "" = Nothing
        justIfNotNull a | length a > 100 = Just . (++ "...") $ take 100 a
                        | otherwise      = Just a

        stripSpaces s = foldr step "" $ dropWhile isSpace s
          where step x "" | isSpace x = ""
                          | otherwise = [x]
                step x r = x : r

        concatWith s = foldr step ""
          where step "" b = b
                step a "" = a
                step a b  = a ++ s ++ b

-- | Combination of performCurl and getTitle
getTitleOfUrl :: URLString -> IO (Maybe String)
getTitleOfUrl url = maybe Nothing getTitle `fmap` performCurl url
