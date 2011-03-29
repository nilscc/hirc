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
import Data.Char
import Data.List (intercalate)
import System.Locale
import System.Directory
import System.IO
import Numeric

import qualified Data.Map as M

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), string, spaces)
import qualified Text.ParserCombinators.Parsec as P

import Commands.Notes
import Commands.Json
import Commands.UrlTitle
import Commands.GoogleTranslation
-- import Commands.GoogleSearch
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

    [ do
        string "help"
        pure . text to <$> (<|>) (eof    >> return "translate google fucking give chr ord")
                                 (spaces >> msum [ string "translate"      >> return "translate <language> [to|→] <language> <string>"
                                                 , string "give"           >> return "give <name> <command>"
                                                 , string "google"         >> return "google <string>"
                                                 , string "fucking"        >> return "fucking <word>"
                                                 , string "chr"            >> return "chr <list of hex numbers>"
                                                 , string "ord"            >> return "ord <string>"
                                                 ])

    {-
    , do
        string "fucking"
        spaces
        what <- many1 $ satisfy (not . isSpace)
        return . pure . io [] $ do

            -- little with show & read :)
            let fname = "fucking.data"
            exists <- doesFileExist fname
            m <- if exists
                    then do h <- openFile fname ReadMode
                            s <- hGetContents h
                            last s `seq` return ()
                            hClose h
                            return (read s :: M.Map (String,String) Int)
                    else return $ M.empty

            let n  = maybe 1 (+1) $ M.lookup (from,what) m
            writeFile fname . show $ M.insert (from,what) n m
            return . Just $ "You were angry about " ++ (concat $ lines what) ++ " " ++ show n ++ " times."
    -}

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
                          Just (Left t)  -> Just $ "Error: " ++ t
                          Just (Right t) -> Just $ cutAt 150 t
                          _ -> Nothing

    {-
    , do
        string "google"
        spaces

        qry <- many anyChar
        return . pure . io to $ do
            res <- getGoogleSearch qry
            return $ case res of
                          Just (url,title) -> Just $ "Result: " ++ cutAt 100 title ++ " <" ++ url ++">"
                          _ -> Nothing

    , do
        string "chr"
        spaces

        s <- many anyChar
        return . pure . text to $
            let str = map chr $ read s
            in str

    , do
        string "ord"
        spaces
        s <- many anyChar
        return . pure . text to $
            let hex = map (\c -> "0x" ++ showHex (ord c) "") s
            in "[" ++ intercalate ", " hex ++ "]"

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


    , do
        -- use either "give" followed by necessary spaces or "," with optional spaces
        (string "give" >> skipMany1 space) <|> (char ',' >> skipMany space)
        to' <- many1 (alphaNum <|> oneOf "{}[]|_-'`^")
        spaces <|> lookAhead (() <$ char ',')
        rpl <- commandsWithPrefix from $ if to' `elem` to
                                            then to
                                            else to ++ [to']

        let foo rpl = case rpl of
                           SafeReply _ -> mzero   -- fail! we cannot give away "safe" replies
                           _           -> return rpl
        mapM foo rpl
    -}

    -- basicly just aliases:
    , do
        str <- many1 anyChar
        return . pure . io to $ readCommands "commands.json" str
    ]

commandsWithoutPrefix :: String -> [String] -> Parser [Reply]
commandsWithoutPrefix from to = choice

    [ do
        anyChar `manyTill` choice [ string "http://"
                                  , string "https://"
                                  , string "www."
                                  ]
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
