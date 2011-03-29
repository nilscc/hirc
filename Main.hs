{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time
import Network (PortNumber)

import qualified Control.Exception as E

import Connection
import Commands
import Types

--
-- IRC settings
--

nickName   = "hirc"
userName   = "hirc"
realName   = "hirc"

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

main = do
  running <- mapM (runManaged `flip` connectionLoop) servers
  manageConnections running connectionLoop

put :: String -> IO ()
put s = do
  now <- getCurrentTime
  putStr $ "(" ++ show now ++ ") " ++ s

putLn :: String -> IO ()
putLn s = put $ s ++ "\n"

connectionLoop :: IrcServer -> IO ()
connectionLoop srv = do

  (read, send) <- connect nickName userName realName (host srv) (port srv)

  putLn $ "Connected to: " ++ (host srv)

  -- setup channels
  mapM_ (send . Join) (channels srv)

  onException (send $ Quit Nothing) . forever $ do

    msg <- read
    case msg of
         Message { msg_command = "PING" } -> do

           -- put "Ping? "
           send Pong
           -- putLn "Pong!"

         Message { msg_command = "INVITE", msg_params = [_,chan] } -> do

           put $ "Joining: \"" ++ chan ++ "\"..."
           send $ Join chan
           putLn "OK"

         Message { msg_command = "PRIVMSG", msg_prefix = Just (NickName nick _ _), msg_params = [chan', text] } -> do

           -- handle private messages correctly
           let (prefix,chan) | chan' == nickName = ("", nick)
                             | otherwise         = (nickName ++ ": ", chan')

           forkIO . onException (return ()) $

               let run rpl = case rpl of

                                  SafeReply rpl' -> run $ rpl'

                                  TextReply to str -> do
                                    putLn $ "Sending text reply: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                    mapM_ (send . PrivMsg (fromMaybe chan to)) (lines str)

                                  IOReply to io -> do
                                    put "Running IO command..."
                                    s <- safe io
                                    case s of
                                         Just (Just str) -> do putLn $ "OK! Sending: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                                               send $ PrivMsg (fromMaybe chan to) str
                                         Just Nothing    -> putLn $ "Fail: No Function result"
                                         _               -> putLn $ "Fail: Exception"


                -- wait 1 second between each event
                in mapM_ (\r -> run r) $ parseCommand prefix nick text 

           return ()

         _ -> return ()

--
-- Exception handling
--

-- | Catch exceptions
onException :: IO a -> IO a -> IO a
onException f = E.handle (\(e :: E.SomeException) -> putLn ("Exception in Main: " ++ show e) >> f)

-- | Ignore exceptions and return `Nothing` if an exception occurs
safe :: IO a -> IO (Maybe a)
safe io = onException (return Nothing) (Just <$> io)
