module Main where

import Hirc
import Hirc.Modules

main :: IO ()
main = run
  [ newHirc xinutec  ["##hirc"]  defaultModules
  , newHirc freenode []          defaultModules
  ]

xinutec, freenode :: IrcServer
xinutec  = IrcServer "irc.xinutec.org"  6667 (stdReconnect 3)
freenode = IrcServer "irc.freenode.org" 6667 (stdReconnect 3)

defaultModules :: [Module]
defaultModules =
  [ adminModule AdminSettings{ adminPassword = Nothing, adminUsers = ["McManiaC"] }
  , urlTitlesModule
  , pokerModule
  ]
