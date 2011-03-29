{-# LANGUAGE ScopedTypeVariables #-}

module Connection
    (
    -- * start connection
      connect

    -- * Data types
    , ConnectionCommand (..)
    , module Connection.Reconnects
    , module Network.IRC
    ) where


import Prelude hiding (log)

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Time
import Network
import Network.IRC
import System.IO

import qualified Control.Exception as E
import qualified Data.Set as S

import Connection.Reconnects

type To = String

data ConnectionCommand
  = Send Message
  | PrivMsg To String  -- ^ private message
  | Join Channel
  | Part Channel
  | Ping
  | Pong
  | Quit (Maybe String)
  deriving Show

--
-- Logging
--

logFile :: Handle -> FilePath
logFile handle = "logs/" ++ show handle ++ ".log"

log :: Handle -> String -> IO ()
log h s = appendFile (logFile h) (s ++ "\n")

startLogging :: Handle -> IO ()
startLogging h = do
  now <- getCurrentTime
  writeFile (logFile h) $
    "New logging session  --  " ++ show now ++ "\n\n"

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

  -- logging
  startLogging handle

  msg <- newChan
  cmd <- newChan

  listenId <- forkIO $
    listenForMessage handle msg
  forkIO $
    receiveCommand cmd handle listenId

  writeChan cmd (Send $ nick nick')
  writeChan cmd (Send $ user user' "*" "*" realname)

  waitFor001 (readChan msg) (log handle)

  return (readChan msg, writeChan cmd)

--
-- Wait for the 001 message before "giving away" our connection
--
waitFor001 :: IO Message
           -> (String -> IO ())
           -> IO ()
waitFor001 getMsg log' = do
  msg <- getMsg
  log' $ "waitFor001 --- " ++ show msg
  case msg of
       Message { msg_command = "001" } -> return ()
       _                               -> waitFor001 getMsg log'

--
-- Wait for commands, execute them
--
receiveCommand :: Chan ConnectionCommand    -- ^ command channel
               -> Handle                    -- ^ server handle
               -> ThreadId                  -- ^ thread id of listenForMessage
               -> IO ()
receiveCommand cmd h listenId = forever . safe Nothing $ do

  cmd <- readChan cmd
  log h $ "receiveCommand --- " ++ show cmd
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
         fail "receiveCommand: Shutting down" -- exception will (hopefully) get cought

--
-- Listen on handle and put incoming messages to our Chan
--
listenForMessage :: Handle -> Chan Message -> IO ()
listenForMessage h msgChan = forever . safe (Just h) $ do
  l <- hGetLine h
  log h $ "listenForMessage --- " ++ l
  case decode l of
       Just msg -> writeChan msgChan msg
       Nothing  -> return () -- todo

--
-- Error handling, skip a char on error
--
safe :: Maybe Handle -> IO () -> IO ()
safe mh = E.handle (\(e :: E.SomeException) -> do
  putStrLn ("Exception in Connection: " ++ show e)
  case mh of
       Just handle -> do
         hSetBinaryMode handle True
         hGetChar handle -- skip char
         hSetBinaryMode handle False
       _ -> return ())

--
-- Send a message
--
send :: Handle -> Message -> IO ()
send h = hPutStrLn h . encode
