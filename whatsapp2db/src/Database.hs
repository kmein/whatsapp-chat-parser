module Database (writeSqlite3) where

import Chat

import Database.HDBC
import Database.HDBC.SqlValue (SqlValue, toSql)
import Database.HDBC.Sqlite3 (connectSqlite3)

_TABLE_NAME_ :: String
_TABLE_NAME_ = "messages"

message2sql :: Message -> [SqlValue]
message2sql (Message dt u m) = [toSql dt, toSql u, toSql m]

insertChat :: (IConnection conn) => conn -> Chat -> IO ()
insertChat conn ms = do
    stmt <- prepare conn $ "INSERT INTO " ++ _TABLE_NAME_ ++ "(date_time, user, message) VALUES (?,?,?)"
    executeMany stmt (map message2sql ms)

createTable
    :: (IConnection conn)
    => conn -> IO Integer
createTable conn =
    run
        conn
        ("CREATE TABLE " ++
         _TABLE_NAME_ ++
         " (id INTEGER PRIMARY KEY AUTOINCREMENT, date_time DATETIME NOT NULL, user VARCHAR(30), message TEXT NOT NULL)")
        []

writeSqlite3 :: FilePath -> Chat -> IO ()
writeSqlite3 path chat = do
    conn <- connectSqlite3 path
    _ <- createTable conn
    insertChat conn chat
    commit conn
    disconnect conn
