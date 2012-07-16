{-# LANGUAGE ScopedTypeVariables #-}

module Commands.Curl
    ( performCurl
    ) where

import Network.Curl hiding (Content)
import Text.XML.Light
import qualified Data.ByteString.UTF8 as B8

type XML = [Content]

-- | Run curl and return the parsed XML
performCurl :: URLString -> IO (Maybe XML)
performCurl url = do
    (r,ty :: B8.ByteString) <- curlGetString_ url [CurlFollowLocation True, CurlMaxRedirs 20, CurlMaxFileSize (10*1024), CurlEncoding ""]

    return $ case r of
                  CurlOK -> Just . parseXML $ B8.toString ty
                  _      -> Nothing
