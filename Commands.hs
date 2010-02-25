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
import Commands.GoogleTranslation
import Commands.GoogleSearch
import Utils

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
commands prefix from = (try (string prefix) *> commandsWithPrefix from []) <|> commandsWithoutPrefix from []



--
-- Little modification to the parsers default functions:
--
string :: String -> Parser String
string = try . P.string
string' = P.string

spaces :: Parser ()
spaces = skipMany1 space

text :: [String] -> String -> Reply
text [] t = TextReply Nothing t
text to t = TextReply Nothing (concatWith ", " to ++ ": " ++ t)

io :: [String] -> IO (Maybe String) -> Reply
io [] f = IOReply Nothing f
io to f = IOReply Nothing (maybe Nothing (Just . (concatWith ", " to ++).(": " ++)) <$> f)

-- }}}

-- | Cut a string after n characters
cutAt :: Int -> String -> String
cutAt n s | length s > n = take n s ++ "..."
          | otherwise    = s

commandsWithPrefix :: String -> [String] -> Parser [Reply]
commandsWithPrefix from to = msum

    -- basicly just aliases:
    [ pure . text to <$> msum
        [ string "fu"           >> return "Fuck you!"
        , string "le-fu"        >> return "Le fu-, we do not le rage so vulgarity. http://n-sch.de/lefu.png"
        , string "faen"         >> return "http://www.youtube.com/watch?v=AkJf0md1kG8"
        , string "perkele"      >> return "Perkele! http://www.youtube.com/watch?v=i9K2BxMsdm4"
        , string "penis"        >> return "8========D"
        ]

    , do
        string "help"
        pure . text to <$> (<|>) (eof    >> return "translate google give fu le-fu faen perkele penis")
                                 (spaces >> msum [ string "translate"      >> return "translate <language> [to|→] <language> <string>"
                                                 , string "give"           >> return "give <name> <command>"
                                                 , string "google"         >> return "google <string>"
                                                 ])

    , do
        string "translate"
        spaces

        from' <- many1 letter
        spaces
        (string "to"   >> spaces) <|> (char '→' >> spaces) <|> return ()
        to'   <- many1 letter
        spaces

        what <- many1 anyChar
        return . pure . io to $ do
            trans <- getGoogleTranslation from' to' what
            return $ case trans of
                          Just (Left t)  -> Just t
                          Just (Right t) -> Just $ "Translation: " ++ cutAt 100 t
                          _ -> Nothing

    , do
        string "google"
        spaces

        qry <- many anyChar
        return . pure . io to $ do
            res <- getGoogleSearch qry
            return $ case res of
                          Just (url,title) -> Just $ "Result: " ++ cutAt 100 title ++ " <" ++ url ++">"
                          _ -> Nothing
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
        rpl <- commandsWithPrefix from $ if to' `elem` to
                                            then to
                                            else to ++ [to']

        let foo rpl = case rpl of
                           SafeReply _ -> mzero   -- fail! we cannot give away "safe" replies
                           _           -> return rpl
        mapM foo rpl


    -- try to parse everything without prefixes since we had no success with prefixes so far :)
    , commandsWithoutPrefix from to
    ]

commandsWithoutPrefix :: String -> [String] -> Parser [Reply]
commandsWithoutPrefix from to = msum

    [ do
        anyChar `manyTill` (string "http://" <|> string "www.")
        url <- anyChar `manyTill` (spaces <|> eof)
        return . pure . IOReply Nothing $ fmap (("Title: " ++) . take 150) <$> getTitleOfUrl url
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
