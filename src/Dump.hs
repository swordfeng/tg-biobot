
import DB
import qualified Database.HDBC.Sqlite3 as Sqlite3

main = do
    conn <- Sqlite3.connectSqlite3 "data.db"
    print =<< getAllUserBio conn