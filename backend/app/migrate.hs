import qualified Database as DB
import qualified Env
import qualified Migrations

main :: IO ()
main = do
    config <- Env.readEnvVars
    conn <- DB.open $ Env.sqliteFile config
    Migrations.migrate conn >>= mapM_ putStrLn
