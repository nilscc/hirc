{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

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

  onError (sendCmd $ Quit Nothing) . forever $ do

    msg <- getMsg
    case msg of
         Message { msg_command = "PING" } -> do

           logM 4 "PING? PONG!"
           sendCmd Pong

         Message { msg_command = "INVITE", msg_params = [_,chan] } -> do

           logM 1 $ "Joining: \"" ++ chan ++ "\""
           sendCmd $ Join chan

         Message { msg_command = "PRIVMSG", msg_prefix = Just (NickName nick' _ _), msg_params = [chan', text] }
           | isCTCP text ->
             handleCTCP nick' text
           | otherwise   -> do

             -- handle private messages correctly
             let (pref,chan) | chan' == nickName = ("", nick')
                             | otherwise         = (nickName ++ ": ", chan')

             forkM . onError (return ()) $

                 let run' rpl = case rpl of

                                     SafeReply rpl' -> run' $ rpl'

                                     TextReply to str -> do
                                       logM 2 $ "Sending text reply: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                       mapM_ (sendCmd . PrivMsg (fromMaybe chan to)) (lines str)

                                     IOReply to io -> do
                                       logM 2 "Running IO command..."
                                       s <- liftIO $ io `catch` \(_::SomeException) -> return Nothing
                                       case s of
                                            Just str -> do logM 2 $
                                                             "IO command successful, sending: "
                                                             ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                                           sendCmd $ PrivMsg (fromMaybe chan to) str
                                            Nothing  -> logM 2 $ "Fail: No Function result (\"" ++ text ++ "\")"


                  -- wait 1 second between each event
                  in mapM_ (\r -> run' r) $ parseCommand pref nick' text 

             return ()

         _ -> return ()

isCTCP :: String -> Bool
isCTCP s = not (null s)
        && head s == '\001' 
        && last s == '\001'

handleCTCP :: To -> String -> Hirc ()
handleCTCP to "\001VERSION\001" = do
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
