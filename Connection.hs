{-# LANGUAGE ScopedTypeVariables #-}

module Connection
    (
    -- * start connection
      connect

    -- * Data types
    , ConnectionCommand (..)
    , module Network.IRC
    ) where


import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Control.Exception as E

import qualified Data.Set as S

import Network
import Network.IRC
import System.IO

type To = String

data ConnectionCommand = Send Message
                       | PrivMsg To String  -- ^ private message
                       | Join Channel
                       | Part Channel
                       | Ping
                       | Pong
                       | Quit (Maybe String)

--
-- | Connect to a IRC server, returns two functions. The first one will return
-- incoming messages, the second will handle commands
--
connect :: UserName     -- ^ nick
        -> UserName     -- ^ user
        -> UserName     -- ^ realname
        -> HostName     -- ^ irc server
        -> PortNumber   -- ^ port
        -> IO (IO Message, ConnectionCommand -> IO ())
connect nick' user' realname host port = do

    handle <- connectTo host (PortNumber port)
    hSetBuffering handle LineBuffering
    hSetBinaryMode handle False

    msg <- newChan
    cmd <- newEmptyMVar

    listenId <- forkIO $ listenForMessage handle msg
    forkIO $ receiveCommand cmd handle listenId

    putMVar cmd (Send $ nick nick')
    putMVar cmd (Send $ user user' "*" "*" realname)

    return (readChan msg, putMVar cmd)

--
-- Wait for commands, execute them
--
receiveCommand :: MVar ConnectionCommand    -- ^ command mvar
               -> Handle                    -- ^ server handle
               -> ThreadId                  -- ^ thread id of listenForMessage
               -> IO ()
receiveCommand cmd h listenId = forever . safe Nothing $ do

    cmd <- takeMVar cmd
    case cmd of

         Send msg           -> send h $ msg
         PrivMsg to msg     -> send h $ privmsg to msg
         Join chan          -> send h $ joinChan chan
         Part chan          -> send h $ part chan

         Ping               -> send h $ Message Nothing "PING" []
         Pong               -> send h $ Message Nothing "PONG" []

         Quit msg           -> do
             -- shutdown everything
             send h $ quit msg
             killThread listenId
             hClose h
             fail "receiveCommand: Shutting down" -- exception will get cought

--
-- Listen on handle and put incoming messages to our Chan
--
listenForMessage :: Handle -> Chan Message -> IO ()
listenForMessage h msgChan = forever . safe (Just h) $ do
    l <- hGetLine h
    case decode l of
         Just msg -> writeChan msgChan msg
         Nothing  -> return () -- todo

--
-- Error handling, skip a char on error
--
safe :: Maybe Handle -> IO () -> IO ()
safe mh = E.handle (\(e :: E.SomeException) -> putStrLn ("Exception in Connection: " ++ show e)
                                            >> case mh of
                                                    Just handle -> do hSetBinaryMode handle True
                                                                      hGetChar handle -- skip char
                                                                      hSetBinaryMode handle False
                                                    _ -> return ())

--
-- Send a message
--
send :: Handle -> Message -> IO ()
send h = hPutStrLn h . encode
