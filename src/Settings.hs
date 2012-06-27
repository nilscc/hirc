{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Settings where

import Hirc
import Modules

servers :: [IrcServer]
servers =
  -- [ IrcServer "irc.freenode.org" 6667 (stdReconnect 3)
              -- [ "##norsk" ]
  [ IrcServer "irc.xinutec.org"  6667 (stdReconnect 3)
              -- [ "#linux", "#weirdshit" ]
              [ "##hirc" ]
  ]

-- modules :: [Module]
-- modules = [googleTranslate]

--------------------------------------------------------------------------------
-- User commands

handleUserCommands :: WithMessage ()
handleUserCommands = do


  -- no result :(
  withParams $ \[_,text] ->
    logM 2 $ "No result for: \"" ++ text ++ "\"" :: HircM ()
