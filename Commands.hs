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
import Data.Time
import System.Locale

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), string, spaces)
import qualified Text.ParserCombinators.Parsec as P

import Commands.Notes
import Commands.UrlTitle

type SendTo = Maybe String

--
-- | Wrapper for plain text commands and IO commands
--
data Reply = TextReply SendTo String
           | IOReply   SendTo (IO (Maybe String))
           | SafeReply Reply                        -- ^ this one cannot be "given away"
           deriving Show

instance Show (IO a) where show _ = "IO"

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
             -> [Reply]
parseCommand prefix from text = either (const []) id $
    parse (commands prefix from) "parseCommand" text

--
-- Look for prefix, then parse commands
--
commands :: String -> String -> Parser [Reply]
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

commandsWithPrefix :: String -> Maybe String -> Parser [Reply]
commandsWithPrefix from to = msum

    -- basicly just aliases:
    [ pure . text to <$> msum
        [ string "fu"         >> return "Fuck you!"
        , string "le fu-"     >> return "Le fu-, we do not le rage so vulgarity. http://n-sch.de/lefu.png"
        , string "faen"       >> return "http://www.youtube.com/watch?v=AkJf0md1kG8"
        , string "perkele"    >> return "Perkele! http://www.youtube.com/watch?v=i9K2BxMsdm4"
        , string "penis"      >> return "8========D"
        ]

    {-
    , do
        string "tell"
        spaces
        to' <- many1 alphaNum
        spaces
        msg <- many1 anyChar
        return . pure . SafeReply . IOReply Nothing $ do
            -- Store in our database.
            noteMessage from to' msg
            return $ Just "Consider it noted."

    , do
        string "read"
        return . pure . SafeReply . IOReply (Just from) $ do
            -- read from our database
            msgs <- readMessages from
            putStrLn . unlines $ map (\(c,f,m) -> formatTime defaultTimeLocale "%m-%d %R - " c ++ f ++ ": " ++ m) msgs
            return $ Just "moep"
    -}

    , do
        string "give"
        spaces
        to' <- many1 alphaNum
        spaces
        rpl <- commandsWithPrefix from (Just to')
        let foo rpl = case rpl of
                           SafeReply _ -> mzero   -- fail! we cannot give away "safe" replies
                           _           -> return rpl
        mapM foo rpl


    ]

commandsWithoutPrefix :: String -> Maybe String -> Parser [Reply]
commandsWithoutPrefix from to = msum

    [ do
        anyChar `manyTill` (string "http://" <|> string "www.")
        url <- anyChar `manyTill` (spaces <|> eof)
        return . pure . SafeReply . IOReply Nothing $ fmap (("Title: " ++) . take 150) <$> getTitleOfUrl url
    ]

    {-
    [ do

        return . pure . SafeReply . IOReply Nothing $ do
            -- get the number of new messages
            n <- newMessages from
            if n > 0
               then return . Just $ from ++ ": You have " ++ show n ++ " new message(s)! Call `read` to view them."
               else mzero
    ]
    -}
