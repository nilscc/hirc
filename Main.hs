{-# LANGUAGE ScopedTypeVariables #-}

module Main () where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Maybe (fromMaybe)

import qualified Control.Exception as E

import Connection
import Commands

nickName   = "hirc"
userName   = "hirc"
realName   = "hirc"

hostName   = "irc.xinutec.org"
port       = 6667

main = do

    (read, send) <- connect nickName userName realName hostName port

    putStrLn $ "Connected to: " ++ hostName

    onException (send $ Quit Nothing) . forever $ do

        msg <- read
        case msg of
             Message { msg_command = "PING" } -> do

                 -- putStr "Ping? "
                 send Pong
                 -- putStrLn "Pong!"

             Message { msg_command = "INVITE", msg_params = [_,chan] } -> do

                 putStr $ "Joining: \"" ++ chan ++ "\"..."
                 send $ Join chan
                 putStrLn "OK"

             Message { msg_command = "PRIVMSG", msg_prefix = Just (NickName nick _ _), msg_params = [chan, text] } -> do

                 -- putStrLn $ chan ++ ": <" ++ nick ++ "> " ++ text

                 forkIO . onException (return ()) $

                     let
                         run rpl = case rpl of

                                        Just (SafeReply rpl') -> run $ Just rpl'

                                        Just (TextReply to str) -> do

                                            putStrLn $ "Sending text reply: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                            mapM_ (send . PrivMsg (fromMaybe chan to)) (lines str)

                                        Just (IOReply to io) -> do

                                            putStr "Running IO command..."
                                            s <- safe io
                                            case s of
                                                 Just (Just str) -> do putStrLn $ "OK! Sending: " ++ maybe "" (\c -> "(" ++ c ++ ") ") to ++ str
                                                                       send $ PrivMsg (fromMaybe chan to) str
                                                 Just Nothing    -> putStrLn $ "Fail: No Function result"
                                                 _               -> putStrLn $ "Fail: Exception"

                                        _ -> return ()

                     in run $ parseCommand (nickName ++ ": ") nick text 
                 return ()
             _ -> return ()

--
-- Catch exceptions
--
onException :: IO a -> IO a -> IO a
onException f = E.handle (\(e :: E.IOException) -> f)

--
-- Ignore exceptions
--
safe :: IO a -> IO (Maybe a)
safe io = onException (return Nothing) (Just <$> io)
