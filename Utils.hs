module Utils
    (
      concatWith
    , urlEscape
    , delayUntil
    ) where

import Data.Char
import Data.Word
import Data.Map ((!))
import Data.ByteString.Internal (c2w)
import Data.Time
import Control.Concurrent
import Network.URI

import qualified Data.Map as M

-- | Concat a list of string with another string between every list element
concatWith :: String -> [String] -> String
concatWith "" a = concat a
concatWith s  a = foldr step "" a
  where step "" a = a
        step a "" = a
        step a  b = a ++ s ++ b

urlEscape :: String -> String
urlEscape = escapeURIString isAllowedInURI

{-
-- | Escape a URL String, stolen from
-- http://hackage.haskell.org/packages/archive/HaskellNet/0.2.1/doc/html/Text-URI.html#v%3Aescape
urlEscape :: String -> String
urlEscape [] = ""
urlEscape (c:cs) | c `elem` validChars = c : urlEscape cs
                 | otherwise           = escChar (c2w c) ++ urlEscape cs

  where validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!$^&*-_=+|/."
        escChar c  = '%' : map (m!) [c `div` 16, c `mod` 16]
        m          = M.fromList $ zip [0..] "0123456789abcdef"
-}

-- | Delay a thread until a given `UTCTime`
delayUntil :: UTCTime -> IO ()
delayUntil time = do
  now <- getCurrentTime
  let tdiff = diffUTCTime time now
  if tdiff > 0 then do
    -- delay for max. 5 minutes before checking again
    threadDelay $ 1000000 * (round tdiff `mod` (5*60))
    delayUntil time
   else
    return ()
