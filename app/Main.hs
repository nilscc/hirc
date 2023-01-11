module Main where

import Hirc
import Hirc.Modules

--import Hirc.Modules.H3st
--import Hirc.Modules.Test

main :: IO ()
main = run
  [ HircDefinition
      { modulesDefinition = defaultModules
      , ircDefinition = IrcDefinition
          { ircServer = n0xy
          , ircChannels = [ "#w0bm" ]
          , ircNickname = "hirc"
          , ircUsername = "hirc"
          , ircRealname = "hirc"
          }
      , logDefinition = LogSettings
          { logLevel = 4
          , logPrintLevel = 4
          , logFile = "hirc.log"
          }
      }
  --xinutec  ["##hirc"]  defaultModules
  -- [ newHirc freenode ["##norsk"] defaultModules
  ]

xinutec, freenode :: IrcServer
xinutec  = IrcServer "irc.xinutec.org"  6667 (stdReconnect 3)
freenode = IrcServer "irc.freenode.org" 6667 (stdReconnect 3)

n0xy :: IrcServer
n0xy = IrcServer "irc.n0xy.net" 6697 (stdReconnect 3)


defaultModules :: [Module]
defaultModules =
  [ adminModule emptyAdminSettings { globalAdmins = ["McManiaC"] }
  --, h3stModule
  --, urlTitlesModule
  --, testModule
  --, pokerModule
  ]
