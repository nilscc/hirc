{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-incomplete-patterns
            -fno-warn-missing-fields #-}

module Hirc
  ( -- requireHandle

    -- * Hirc types & functions
    Hirc (..)
  , newHirc
  , run
  --, HircM
  --, HircError (..)
  --, HircState (..)
  --, HircSettings (..)

  , getNickname, getUsername, getRealname, setNickname

    -- * Module types & functions
  , Module (..)
  , IsModuleStateValue -- (..)
  , List
  , Map
  , module Hirc.ModuleState

    -- * Messages & user commands
  , MessageM
  , HircM
  , IsHircCommand (..)
  , userCommand, onValidPrefix --, onCommand
  , done, doneAfter, getMessage, getCurrentChannel
  , withNickname, withUsername, withNickAndUser, withServer, withParams

    -- * IRC types & functions
  , Message (..)
  , IrcServer (..)
  , Reconnect (..)
  , stdReconnect
  --, ConnectionCommand (..)
  , Channel
  , Nickname
  , Username
  , Realname
  , answer, say, whisper, answerTo
  , joinChannel, partChannel, sendNotice, quitServer

    -- * Concurrency
  , newThread
  , wait

    -- * Logging
  , LogM (..)
  , LogSettings (..)
  , logM

    -- * Other
  , module Hirc.Utils

  , module Control.Concurrent.MState
  , module Control.Monad.Reader
  , module Control.Exception.Peel

  ) where

import Prelude                      hiding (catch)

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MState
import Control.Monad.Reader
import Control.Exception.Peel
import Text.Regex.Posix

import Hirc.Commands
import Hirc.Connection
import Hirc.Messages
import Hirc.ModuleState
import Hirc.Logging
import Hirc.Types
import Hirc.Utils

setNickname :: String -> MessageM ()
setNickname n = lift $ modifyM_ $ \s -> s { ircNickname = n }

getNickname :: MessageM String
getNickname = gets ircNickname

getUsername :: MessageM String
getUsername = gets ircUsername

getRealname :: MessageM String
getRealname = gets ircRealname

--------------------------------------------------------------------------------
-- IRC stuff

-- | Reply in a query to the user of the current message
whisper :: String -> MessageM ()
whisper txt = withNickname $ \n -> sendCmd $ PrivMsg n txt

answerTo :: Filtered m
         => (Either Nickname Channel -> m ())
         -> MessageM ()
answerTo m = withParams $ \[c,_] -> do
  n <- getNickname
  if c == n then
    -- private message to user
    withNickname $ m . Left
   else
    -- public message to channel
    runFiltered $ m (Right c)

-- | Answer and add the nick in public channels
answer :: String
       -> MessageM ()
answer text =
  answerTo $
    either (\n -> lift $
             sendCmd $ PrivMsg n text)
           (\c -> withNickname $ \n ->
             sendCmd $ PrivMsg c (n ++ ": " ++ text))

-- | Answer without prefix the nick in a public channel
say :: String
    -> MessageM ()
say text =
  answerTo $ \to ->
    sendCmd $ PrivMsg (either id id to) text

joinChannel :: Channel -> MessageM ()
joinChannel ch = lift $ sendCmd $ Join ch -- TODO: store current channels somewhere?

partChannel :: Channel -> MessageM ()
partChannel ch = lift $ sendCmd $ Part ch

sendNotice :: Nickname -> String -> MessageM ()
sendNotice n s = lift $ sendCmd $ Notice n s

quitServer :: Maybe String -- ^ optional quit message
           -> MessageM ()
quitServer mqmsg = lift $ sendCmd $ Quit mqmsg



--------------------------------------------------------------------------------
-- IRC tests

-- | Require prefixing user commands with the name of the bot (in a public
-- channel)
onValidPrefix :: MessageM ()
              -> MessageM ()
onValidPrefix wm =
  withParams $ \[c,_] -> do
    myNick <- getNickname
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

-- | Create a new Hirc instance
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

    -- load modules
    mods <- lift . asks $ modules . runningHirc

    onCommand "PING" $
      pongCmd

    onCommand "NICK" $
      withNickAndUser $ \n u ->
      withParams      $ \[new] ->
      doneAfter       $ do
        myNick <- getNickname
        myUser <- getUsername
        if n == myNick && u == myUser then do
           setNickname new
           logM 1 $ "Nick changed to \"" ++ new ++ "\""
         else
           -- call `onNickChange' if existant
           mapM_ (\m -> withModCtxt m (maybe (return ()) (\f -> f u new) (onNickChange m)) `catch` moduleException m)
                 mods

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
      mapM_ (\m -> withModCtxt m (runModule m) `catch` moduleException m)
            mods
 where
  withModCtxt m f = local (second $ \ctxt -> ctxt { ctxtModule = Just m }) f

--------------------------------------------------------------------------------
-- Exceptions

moduleException :: Module -> SomeException -> MessageM ()
moduleException Module{ moduleName } e =
  logM 1 $ "Module exception in \"" ++ moduleName ++ "\": " ++ show e


--------------------------------------------------------------------------------
-- CTCP

isCTCP :: String -> Bool
isCTCP s = not (null s)
        && head s == '\001' 
        && last s == '\001'

handleCTCP :: String -> MessageM ()

handleCTCP "\001VERSION\001" =
  withNickname $ \to -> do
    logM 2 $ "Sending CTCP VERSION reply to \"" ++ to ++ "\""
    sendCmd $ Notice to "\001VERSION hirc v0.2\001"

handleCTCP t =
  logM 2 $ "Unhandled CTCP: " ++ t


--------------------------------------------------------------------------------
-- Concurrency

newThread :: MessageM () -> MessageM ThreadId
newThread m = do
  r <- ask
  lift $ forkM (runReaderT m r :: HircM ())

wait :: Int   -- ^ number of seconds
     -> MessageM ()
wait sec = liftIO $ threadDelay (sec * 1000000)
