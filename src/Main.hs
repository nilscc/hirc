{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS -fno-warn-unused-do-bind
            -fno-warn-incomplete-patterns
            #-}

module Main where

import Prelude                      hiding (catch)
import Text.Regex.Posix

import Commands.GoogleTranslation
import Commands.UrlTitle
import Hirc                         hiding (join)
import Hirc.Types (Filtered, runFiltered)

--
-- IRC settings
--

servers :: [IrcServer]
servers =
  [ IrcServer "irc.freenode.org" 6667 (stdReconnect 3)
              [ "##norsk" ]
  , IrcServer "irc.xinutec.org"  6667 (stdReconnect 3)
              [ "#linux", "#weirdshit" ]
  ]

--
-- main
--

main :: IO ()
main = run $ mapM_ (manage `flip` myHirc) servers

join :: String -> Hirc ()
join chan = do
  logM 1 $ "Joining: \"" ++ chan ++ "\""
  sendCmd $ Join chan

pong :: Hirc ()
pong = do
  logM 4 "PING? PONG!"
  sendCmd Pong

myHirc :: Hirc ()
myHirc = do

  connect "hirc" "hirc" "hirc"

  srv <- asks server
  logM 1 $ "Connected to: " ++ (host srv)

  -- setup channels
  mapM_ join (channels srv)

  let logException (e :: SomeException) = do
        logM 1 $ "Some exception: " ++ show e
        sendCmd $ Quit Nothing
  handle logException . forever . handleIncomingMessage $ do

    onCommand "PING" $
      pong

    onCommand "NICK" $
      withNickAndUser $ \n u ->
      withParams      $ \[new] ->
      doneAfter       $ do
        myNick <- getNickname
        myUser <- getUsername
        when (n == myNick && u == myUser) $ do
             setNickname new
             logM 1 $ "Nick changed to \"" ++ new ++ "\""

    onCommand "INVITE" $ withParams $ \[_,chan] ->
      join chan

    onCommand "PRIVMSG" $ do
      
      withParams $ \[_,text] ->
        when (isCTCP text) $ do
          handleCTCP text `catch` \(e :: SomeException) ->
            logM 1 $ "CTCP exception: " ++ show e
          done

      onValidPrefix $
        handleUserCommands `catch` \(e :: SomeException) ->
          logM 1 $ "User command exception: " ++ show e

      showUrlTitles `catch` \(e :: SomeException) ->
        logM 1 $ "Url title exception: " ++ show e

--------------------------------------------------------------------------------
-- User commands

handleUserCommands :: WithMessage ()
handleUserCommands = do

  -- google translation
  userCommand $ \"translate" lang1 lang2 (unwords -> what) -> do

    result <- getGoogleTranslation lang1 lang2 what
    case result of
         Just (Right translation) -> withNickname $ \n -> do
           logM 2 $ "Sending translation to " ++ show n
           answer translation
         e ->
           logM 2 $ "Translation failed with: " ++ show e 
    done

  -- no result :(
  withParams $ \[_,text] ->
    logM 2 $ "No result for: \"" ++ text ++ "\"" :: Hirc ()


--------------------------------------------------------------------------------
-- URL stuff

showUrlTitles :: WithMessage ()
showUrlTitles = withParams $ \[_,text] -> do
  let urls = filter (=~ "^(http://|https://|www\\.)") (words text)
  case urls of
       (url:_) -> do
         title <- getTitle url
         maybe (return ())
               (answer' . ("Title: " ++))
               title
       _ -> return ()


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


--------------------------------------------------------------------------------
-- Other

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
