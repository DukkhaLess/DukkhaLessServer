module Schema where

import           Protolude
import           Conf                           ( MigrationsPath(..) )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Bifunctor                 ( first )
import           Hasql.Migration
import           Hasql.Pool
import           Hasql.Transaction.Sessions


data MigrationFailureReason
  = UsageErrorReason UsageError
  | MigrationErrorReason MigrationError
  deriving Show

runMigrations :: MigrationsPath -> Pool -> IO (Either MigrationFailureReason ())
runMigrations (MigrationsPath p) pool = do
  commands <- loadMigrationsFromDirectory p
  let transactions = map runMigration (MigrationInitialization : commands)
  let sessions     = map (transaction Serializable Write) transactions
  let executableQueries =
        map (ExceptT . (map (first UsageErrorReason)) . use pool) sessions
  let queries = map
        (maybe (pure ()) (ExceptT . pure . Left . MigrationErrorReason) =<<)
        executableQueries
  result <- runExceptT $ sequence queries
  pure $ map (const ()) result

