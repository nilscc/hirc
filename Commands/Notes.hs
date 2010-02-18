module Commands.Notes
    (
    -- * Read & tell messages
      noteMessage
    , readMessages
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

sqlite3Db = "notes.db"

-- | Add a message to our sqlite3 database
noteMessage :: String   -- ^ from
            -> String   -- ^ to
            -> String   -- ^ the message
            -> IO ()
noteMessage from to msg = do

    con <- connectSqlite3 sqlite3Db

    run con "CREATE TABLE IF NOT EXISTS notes (fromNick VARCHAR (80), toNick VARCHAR (80), msg TEXT)" []
    run con "INSERT INTO notes VALUES (?, ?, ?)" [toSql from, toSql to, toSql msg]
    commit con

    disconnect con

-- | Read all messages from a user and remove everything afterwars
readMessages :: String -> IO [String]
readMessages for = do

    con <- connectSqlite3 sqlite3Db

    results <- quickQuery' con "SELECT * FROM notes WHERE toNick = ?" [toSql for]
    let messages = map (\[f,t,m] -> fromSql f ++ ": " ++ fromSql m) results

    -- Delete the old messages
    run con "DELETE FROM notes WHERE toNick = ?" [toSql for]
    commit con

    disconnect con
    return messages
