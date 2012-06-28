module Main where

import Hirc
import Modules

main :: IO ()
main = run
  [ newHirc xinutec  ["##hirc"]        defaultModules
  -- , newHirc freenode ["##fcj-acronym"] defaultModules
  ]

xinutec, freenode :: IrcServer
xinutec  = IrcServer "irc.xinutec.org"  6667 (stdReconnect 3)
freenode = IrcServer "irc.freenode.org" 6667 (stdReconnect 3)

defaultModules :: [Module]
defaultModules = [urlTitles, poker]
