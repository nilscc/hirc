module Commands.Notes
    (
    -- * Read & tell messages
      noteMessage
    , readMessages
    , newMessages
    ) where

import Data.Time
import Database.HDBC
import Database.HDBC.Sqlite3

sqlite3Db = "notes.db"

withDb f = do
    con <- connectSqlite3 sqlite3Db
    r <- f con
    disconnect con
    return r

-- | Add a message to our sqlite3 database
noteMessage :: String   -- ^ from
            -> String   -- ^ to
            -> String   -- ^ the message
            -> IO ()
noteMessage from to msg = withDb $ \con -> do

    now <- getCurrentTime

    run con "CREATE TABLE IF NOT EXISTS notes ( id          INTEGER PRIMARY KEY AUTOINCREMENT,  \
                                              \ date        INTEGER,                            \
                                              \ fromNick    VARCHAR,                            \
                                              \ toNick      VARCHAR,                            \
                                              \ message     TEXT                                \
                                              \ )" []

    run con "INSERT INTO notes VALUES (?, ?, ?, ?, ?)" [ SqlNull
                                                       , toSql now
                                                       , toSql from
                                                       , toSql to
                                                       , toSql msg
                                                       ]

    commit con

-- | Read all messages from a user and remove everything afterwars
readMessages :: String -> IO [(UTCTime, String, String)]
readMessages for = withDb $ \con -> do

    results <- quickQuery' con "SELECT * FROM notes WHERE toNick = ? ORDER BY date" [toSql for]
    let messages = map (\[_,c,f,t,m] -> (fromSql c, fromSql f, fromSql m)) results

    -- Delete the old messages
    run con "DELETE FROM notes WHERE toNick = ?" [toSql for]
    commit con

    return messages

-- | Return the number of new messages, then remove the new user from the
-- newmessages list
newMessages :: String -> IO Int
newMessages toNick = withDb $ \con -> do
    quickQuery' con "SELECT * FROM notes COUNT * WHERE toNick = ?" [toSql toNick]
    return 0
