module Schema
      ( module Schema.Types
      , runMigrations
      , MigrationFailureReason(..)
      , createIO
      , updateIO
      ) where

import           Protolude
import           Conf                           ( MigrationsPath(..) )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Bifunctor                 ( first )
import           Hasql.Migration
import           Hasql.Pool
import           Hasql.Transaction.Sessions
import           Schema.Types
import Types


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

createIO :: forall m a. MonadIO m => a -> m (Create a)
createIO a = do
  now <- liftIO $ getCurrentTime
  pure $ Create (LastUpdated now) (CreatedAt now) a

updateIO :: forall m a. MonadIO m => a -> m (Update a)
updateIO a = do
      now <- liftIO $ getCurrentTime
      pure $ Update (LastUpdated now) a
