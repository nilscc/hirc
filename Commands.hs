module Commands
    (
    -- * command parsing
      parseCommand
    , Reply (..)
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Maybe (isNothing)
import Network.Curl
import Network.URI

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), string, spaces)
import qualified Text.ParserCombinators.Parsec as P

import Commands.Notes

type SendTo = Maybe String

--
-- | Wrapper for plain text commands and IO commands
--
data Reply = TextReply SendTo String
           | IOReply   SendTo (IO (Maybe String))
           | SafeReply Reply                        -- ^ this one cannot be "given away"

-- {{{ Internal parser stuff...

-- Every Monad is an Applicative.
instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

-- Every MonadPlus is an Alternative.
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus


--
-- | Run parser
--
parseCommand :: String          -- ^ command prefix
             -> String          -- ^ from
             -> String          -- ^ text to parse
             -> Maybe Reply
parseCommand prefix from text = either (const Nothing) (Just) $
    parse (commands prefix from) "parseCommand" text

--
-- Look for prefix, then parse commands
--
commands :: String -> String -> Parser Reply
commands prefix from = (try (string prefix) *> commandsWithPrefix from Nothing) <|> commandsWithoutPrefix from Nothing



--
-- Little modification to the parsers default functions:
--
string :: String -> Parser String
string = try . P.string
string' = P.string

spaces :: Parser ()
spaces = skipMany1 space

text to t =
    case to of
         Just nick -> TextReply Nothing (nick ++ ": " ++ t)
         Nothing   -> TextReply Nothing t

io to f =
    case to of
         Just nick -> IOReply Nothing (maybe Nothing (Just . (nick ++) . (": " ++)) <$> f)
         Nothing   -> IOReply Nothing f

-- }}}

commandsWithPrefix :: String -> Maybe String -> Parser Reply
commandsWithPrefix from to = msum

    [ do
        string "tell"
        spaces
        to' <- many1 alphaNum
        spaces
        msg <- many1 anyChar
        return . SafeReply . IOReply Nothing $ do
            -- Store in our database.
            noteMessage from to' msg
            return $ Just "Consider it noted."

    , do
        string "read"
        return . SafeReply . IOReply (Just from) $ do
            -- read from our database
            msgs <- readMessages from
            return . Just $ unlines msgs

    -- basicly just aliases:
    , text to <$> msum
        [ string "fu"         >> return "Fuck you!"
        , string "le-fu"      >> return "Le fu - we do not le rage so vulgarity. http://n-sch.de/lefu.png"
        , string "faen"       >> return "http://www.youtube.com/watch?v=AkJf0md1kG8"
        , string "perkele"    >> return "Perkele! http://www.youtube.com/watch?v=i9K2BxMsdm4"
        ]

    , do
        string "give"
        spaces
        to' <- many1 alphaNum
        spaces
        rpl <- commandsWithPrefix from (Just to')
        case rpl of
             SafeReply _ -> mzero   -- fail! we cannot give away "safe" replies
             _           -> return rpl


    ]

commandsWithoutPrefix :: String -> Maybe String -> Parser Reply
commandsWithoutPrefix from to = msum []

    {-
    [ let

          -- thats google stuff:
          {-
          parseTitle = manyTill anyChar (string "<h3 class=r>") *> manyTill onlyText (string "</h3>")
          onlyText = (++) <$> manyTill anyChar (char '<') <* many (noneOf ">")
                          <*> (onlyText <|> return [])
          -}

          -- no very sensible parser... :)
          parseTitle = manyTill anyChar         (try $ string "<title>" <|> string "<TITLE>")
                    *> manyTill (noneOf "</>")  (try $ string "</title" <|> string "</TITLE>")

          getTitle url = do
                res <- second (parse parseTitle "parseTitle") <$> curlGetString url [CurlFollowLocation True]
                case res of
                     (CurlOK, Right s) -> return . Just $ "Title: " ++ trunc s ++ " (" ++ url ++ ")"
                     _                 -> return Nothing

      in io Nothing . getTitle <$> (manyTill anyChar (try . lookAhead $ string "http://" <|> {- string "https://" <|> -} string "www.") *> many1 (noneOf " "))

    ]

trunc s' =
    let s = concat $ lines s'
    in if length s > 80
          then take 80 s ++ "..."
          else s

curl url = curlGetString url [CurlFollowLocation True, CurlMaxRedirs 20]

unpackIO (Right (IOReply io)) = putStrLn "ok" >> io
unpackIO _ = return Nothing

-}


