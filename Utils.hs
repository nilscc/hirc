module Utils
    (
      concatWith
    ) where

import Data.Char

-- | Concat a list of string with another string between every list element
concatWith :: String -> [String] -> String
concatWith "" a = concat a
concatWith s  a = foldr step "" a
  where step "" a = a
        step a "" = a
        step a  b = a ++ s ++ b
