module Main where

import Hirc
import Hirc.Modules

--import Hirc.Modules.H3st
--import Hirc.Modules.Test

main :: IO ()
main = run
  [ newHirc xinutec  ["##hirc"]  defaultModules
  -- [ newHirc freenode ["##norsk"] defaultModules
  ]

xinutec, freenode :: IrcServer
xinutec  = IrcServer "irc.xinutec.org"  6667 (stdReconnect 3)
freenode = IrcServer "irc.freenode.org" 6667 (stdReconnect 3)

defaultModules :: [Module]
defaultModules =
  [ adminModule emptyAdminSettings { globalAdmins = ["McManiaC"] }
  --, h3stModule
  , urlTitlesModule
  --, testModule
  --, pokerModule
  ]
