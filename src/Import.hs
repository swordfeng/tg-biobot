import DB
import Data.TCache

main = do
    initDB
    d <- read <$> getContents :: IO [(String, String, String)]
    atomicallySync $ importUserBio d