{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-incomplete-patterns
            -fno-warn-missing-fields #-}

module Hirc
  ( -- requireHandle

    -- * Types
    -- ** Hirc types & functions
    Hirc
  , newHirc
  , HircM
  , HircError (..)
  , HircState (..)
  , HircSettings (..)
  , Module (..)

  , getNickname
  , getUsername
  , getRealname
  , setNickname

    -- ** Messages and user commands
  , WithMessage
  , module Hirc.Messages
  , module Hirc.Commands
  , answerTo, answer, answer'
  , onValidPrefix

    -- ** Managed types
  , Managed
  , ManagedState (..)
  , ManagedSettings (..)

    -- ** Connection types
  , IrcServer (..)
  , Reconnect (..)
  , ConnectionCommand (..)
  , Nickname
  , Username
  , Realname
  , To
  , module Hirc.Connection

    -- ** Logging
  , LogM (..)
  , LogSettings (..)
  , module Hirc.Logging

    -- * Reexports
  , module Control.Concurrent.MState
  , module Control.Monad.Reader
  , module Control.Exception.Peel

  ) where

import Prelude                      hiding (catch)

import Control.Concurrent.MState
import Control.Monad.Reader
import Control.Exception.Peel
import Text.Regex.Posix

import Hirc.Commands
import Hirc.Connection
import Hirc.Messages
import Hirc.Logging
import Hirc.Types

setNickname :: String -> HircM ()
setNickname n = modifyM_ $ \s -> s { ircNickname = n }

getNickname :: HircM String
getNickname = gets ircNickname

getUsername :: HircM String
getUsername = gets ircUsername

getRealname :: HircM String
getRealname = gets ircRealname

--------------------------------------------------------------------------------
-- IRC replies

answerTo :: Filtered m
         => (Either String String -> m ())
         -> WithMessage ()
answerTo m = withParams $ \[c,_] -> do
  n <- lift getNickname
  if c == n then
    -- private message to user
    withNickname $ m . Left
   else
    -- public message to channel
    runFiltered $ m (Right c)

-- | Answer and add the nick for public channels
answer :: String
       -> WithMessage ()
answer text =
  answerTo $
    either (\n -> lift $
             sendCmd $ PrivMsg n text)
           (\c -> withNickname $ \n ->
             sendCmd $ PrivMsg c (n ++ ": " ++ text))

-- | Answer without prefix the nick in a public channel
answer' :: String
        -> WithMessage ()
answer' text =
  answerTo $ \to ->
    sendCmd $ PrivMsg (either id id to) text

--------------------------------------------------------------------------------
-- IRC tests

onValidPrefix :: WithMessage ()
              -> WithMessage ()
onValidPrefix wm =
  withParams $ \[c,_] -> do
    myNick <- lift getNickname
    if c == myNick then
      -- direct query
      wm
     else
      -- public channel with valid prefix
      userCommand $ \(validPrefix myNick -> True) ->
        wm
 where
  validPrefix :: Nickname -> String -> Bool
  validPrefix n s = s =~ ("^" ++ escape n ++ "[:,.-\\!]?$")

  escape n = foldr esc "" n
  esc '\\' r = "\\\\" ++ r
  esc '['  r = "\\[" ++ r
  esc ']'  r = "\\]" ++ r
  esc '{'  r = "\\{" ++ r
  esc '}'  r = "\\}" ++ r
  esc a    r = a:r

--------------------------------------------------------------------------------
-- Default stuff

-- | Default Hirc state
newHirc :: IrcServer -> [Channel] -> [Module] -> Hirc
newHirc srv chs mods = Hirc
  { server     = srv
  , channels   = chs
  , nickname   = "hirc"
  , username   = "hirc"
  , realname   = "hirc"
  , eventQueue = defEventQueue
  , modules    = mods
  }

-- irc commands: join
joinCmd :: String -> HircM ()
joinCmd chan = do
  logM 1 $ "Joining: \"" ++ chan ++ "\""
  sendCmd $ Join chan

-- irc commands: pong
pongCmd :: HircM ()
pongCmd = do
  logM 4 "PING? PONG!"
  sendCmd Pong

-- | Default event queue
defEventQueue :: HircM ()
defEventQueue = do

  nn <- gets ircNickname
  un <- gets ircUsername
  rn <- gets ircRealname

  connect nn un rn

  srv <- asks $ server . runningHirc
  logM 1 $ "Connected to: " ++ (host srv)

  -- setup channels
  chs <- asks $ channels . runningHirc
  mapM_ joinCmd chs

  let logException (e :: IOException) = do
        logM 1 $ "IO exception: " ++ show e
        sendCmd $ Quit Nothing
  handle logException . forever . handleIncomingMessage $ do

    onCommand "PING" $
      pongCmd

    onCommand "NICK" $
      withNickAndUser $ \n u ->
      withParams      $ \[new] ->
      doneAfter       $ do
        myNick <- getNickname
        myUser <- getUsername
        when (n == myNick && u == myUser) $ do
             setNickname new
             logM 1 $ "Nick changed to \"" ++ new ++ "\""

    onCommand "INVITE" $
      withParams $ \[_,chan] -> joinCmd chan

    onCommand "PRIVMSG" $ do

      -- run CTCP shit, this stuff is a TODO
      withParams $ \[_,text] ->
        when (isCTCP text) $ do
          handleCTCP text `catch` \(e :: SomeException) ->
            logM 1 $ "CTCP exception: " ++ show e
          done

      -- run user modules
      mods <- lift . asks $ modules . runningHirc
      mapM_ (\m -> runModule m `catch` moduleException m) mods


--------------------------------------------------------------------------------
-- Exceptions

moduleException :: Module -> SomeException -> WithMessage ()
moduleException Module{ moduleName } e =
  logM 1 $ "Module exception in \"" ++ moduleName ++ "\": " ++ show e


--------------------------------------------------------------------------------
-- CTCP

isCTCP :: String -> Bool
isCTCP s = not (null s)
        && head s == '\001' 
        && last s == '\001'

handleCTCP :: String -> WithMessage ()

handleCTCP "\001VERSION\001" =
  withNickname $ \to -> do
    logM 2 $ "Sending CTCP VERSION reply to \"" ++ to ++ "\""
    sendCmd $ Notice to "\001VERSION hirc v0.2\001"

handleCTCP t =
  logM 2 $ "Unhandled CTCP: " ++ t

