{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Data.Maybe (fromMaybe)
import Data.Time

import Connection
import Commands
import Hirc

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
  , IrcServer "irc.quakenet.org" 6667 (stdReconnect 3)
              [ "#dang0r" ]
  ]

--
-- main
--

main :: IO ()
main = run $ mapM_ (manage `flip` hirc) servers

putS :: MonadIO m => String -> m ()
putS s = do
  now <- liftIO getCurrentTime
  liftIO $ putStr $ "(" ++ show now ++ ") " ++ s

putSLn :: MonadIO m => String -> m ()
putSLn s = putS $ s ++ "\n"

-- connectToServer :: IrcServer -> IO ()
-- connectToServer srv = do

hirc :: Hirc ()
hirc = do

  connect nickName userName realName

  srv <- asks server
  putSLn $ "Connected to: " ++ (host srv)

  -- setup channels
  mapM_ (sendCmd . Join) (channels srv)

  onError (sendCmd $ Quit Nothing) . forever $ do

    msg <- getMsg
    case msg of
         Message { msg_command = "PING" } -> do

           -- put "Ping? "
           sendCmd Pong
           -- putSLn "Pong!"

         Message { msg_command = "INVITE", msg_params = [_,chan] } -> do

           putS $ "Joining: \"" ++ chan ++ "\"..."
           sendCmd $ Join chan
           putSLn "OK"

         Message { msg_command = "PRIVMSG", msg_prefix = Just (NickName nick' _ _), msg_params = [chan', text] } -> do

           -- handle private messages correctly
           let (pref,chan) | chan' == nickName = ("", nick')
                             | otherwise         = (nickName ++ ": ", chan')

           forkM . onError (return ()) $

               let run' rpl = case rpl of

                                   SafeReply rpl' -> run' $ rpl'

                                   TextReply to str -> do
                                     putSLn $ "Sending text reply: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                     mapM_ (sendCmd . PrivMsg (fromMaybe chan to)) (lines str)

                                   IOReply to io -> do
                                     putS "Running IO command..."
                                     s <- safe (liftIO io)
                                     case s of
                                          Just (Just str) -> do putSLn $ "OK! Sending: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                                                sendCmd $ PrivMsg (fromMaybe chan to) str
                                          Just Nothing    -> putSLn $ "Fail: No Function result"
                                          _               -> putSLn $ "Fail: Exception"


                -- wait 1 second between each event
                in mapM_ (\r -> run' r) $ parseCommand pref nick' text 

           return ()

         _ -> return ()

--
-- Exception handling
--

-- | Catch exceptions
onError :: (Show e, Error e, MonadError e m, MonadIO m) => m a -> m a -> m a
onError f = catchError `flip` (\e -> putSLn ("Exception in Main: " ++ show e) >> f)

-- | Ignore exceptions and return `Nothing` if an exception occurs
safe :: (Show e, Error e, MonadError e m, MonadIO m) => m a -> m (Maybe a)
safe io = onError (return Nothing) (io >>= return . Just)
