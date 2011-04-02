{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-do-bind
            -fno-warn-incomplete-patterns
            #-}

module Main where

import Prelude hiding (catch)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Exception.Peel
import Data.Maybe (fromMaybe)
import Data.Time

import Connection
import Commands
import Hirc
import Logging
import Messages

--
-- IRC settings
--

nickName :: Nickname
nickName = "hirc"
userName :: Username
userName = "hirc"
realName :: Realname
realName = "hirc"

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

myHirc :: Hirc ()
myHirc = do

  connect nickName userName realName

  srv <- asks server
  putSLn $ "Connected to: " ++ (host srv)

  -- setup channels
  mapM_ (sendCmd . Join) (channels srv)

  onError (sendCmd $ Quit Nothing) . forever . handleIncomingMessage $ do

    onCommand "PING" $ do
      logM 4 "PING? PONG!"
      sendCmd Pong

    onCommand "INVITE" $ withParams $ \[_,chan] -> do
      logM 1 $ "Joining: \"" ++ chan ++ "\""
      sendCmd $ Join chan

    onCommand "PRIVMSG" $ withNickname $ \nick' -> withParams $ \[chan', text] -> do

      if (isCTCP text) then
        forkM_ $ handleCTCP nick' text
       else do
        -- handle private messages correctly
        let (pref,chan) | chan' == nickName = ("", nick')
                        | otherwise         = (nickName ++ ": ", chan')

        forkM_ . onError (return ()) $

            let run' rpl = case rpl of

                  SafeReply rpl' -> run' $ rpl'

                  TextReply to str -> do
                    logM 2 $ "Sending text reply: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                    mapM_ (sendCmd . PrivMsg (fromMaybe chan to)) (lines str)

                  IOReply to io -> do
                    logM 2 "Running IO command..."
                    s <- liftIO io `catch` \(e::SomeException) -> do
                      logM 2 $ "Exception in IO command: " ++ show e
                      return Nothing
                    case s of
                         Just str -> do
                           logM 2 $ "IO command successful, sending: "
                                 ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                           sendCmd $ PrivMsg (fromMaybe chan to) str
                         Nothing ->
                           logM 2 $ "Fail: No Function result (\"" ++ text ++ "\")"


             -- wait 1 second between each event
             in mapM_ (\r -> run' r) $ parseCommand pref nick' text 

--------------------------------------------------------------------------------
-- CTCP

isCTCP :: String -> Bool
isCTCP s = not (null s)
        && head s == '\001' 
        && last s == '\001'

handleCTCP :: To -> String -> Hirc ()

handleCTCP to "\001VERSION\001" = do
  logM 2 "Sending CTCP VERSION reply"
  sendCmd $ Notice to "\001VERSION hirc v0.2\001"

handleCTCP _ t =
  logM 2 $ "Unhandled CTCP: " ++ t

--
-- Exception handling
--

-- | Catch exceptions
onError :: (Show e, Error e, MonadError e m, MonadIO m) => m a -> m a -> m a
onError f = catchError `flip` (\e -> putSLn ("Exception in Main: " ++ show e) >> f)

-- | Ignore exceptions and return `Nothing` if an exception occurs
safe :: (Show e, Error e, MonadError e m, MonadIO m) => m a -> m (Maybe a)
safe io = onError (return Nothing) (io >>= return . Just)


--
-- Other
--

putS :: MonadIO m => String -> m ()
putS s = do
  now <- liftIO getCurrentTime
  liftIO $ putStr $ "(" ++ show now ++ ") " ++ s

putSLn :: MonadIO m => String -> m ()
putSLn s = putS $ s ++ "\n"
