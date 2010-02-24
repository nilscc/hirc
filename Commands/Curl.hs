module Commands.Curl
    ( performCurl
    ) where

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
