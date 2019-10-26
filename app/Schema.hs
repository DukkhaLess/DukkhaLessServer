module Schema
      ( module Schema.Types
      , runMigrations
      , MigrationFailureReason(..)
      , createIO
      , updateIO
      , runStatement
      )
where

import           Protolude
import           Conf                           ( MigrationsPath(..) )
import           Control.Lens
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Bifunctor                 ( first )
import           Hasql.Migration
import qualified Hasql.Pool                    as HP
import qualified Hasql.Statement               as Statement
import qualified Hasql.Session                 as Session
import           Hasql.Transaction.Sessions
import           Schema.Types
import qualified Types                         as T


data MigrationFailureReason
  = UsageErrorReason HP.UsageError
  | MigrationErrorReason MigrationError
  deriving Show

runMigrations
      :: MigrationsPath -> HP.Pool -> IO (Either MigrationFailureReason ())
runMigrations (MigrationsPath p) pool = do
      commands <- loadMigrationsFromDirectory p
      let transactions = map runMigration (MigrationInitialization : commands)
      let sessions     = map (transaction Serializable Write) transactions
      let executableQueries = map
                (ExceptT . (map (first UsageErrorReason)) . HP.use pool)
                sessions
      let
            queries = map
                  (maybe (pure ())
                         (ExceptT . pure . Left . MigrationErrorReason) =<<
                  )
                  executableQueries
      result <- runExceptT $ sequence queries
      pure $ map (const ()) result

createIO :: forall m a . MonadIO m => a -> m (Create a)
createIO a = do
      now <- liftIO $ getCurrentTime
      pure $ Create (T.LastUpdated now) (T.CreatedAt now) a

updateIO :: forall m a . MonadIO m => a -> m (Update a)
updateIO a = do
      now <- liftIO $ getCurrentTime
      pure $ Update (T.LastUpdated now) a

runStatement
      :: MonadIO m
      => MonadReader r m
      => T.HasConnectionPool r HP.Pool
      => a
      -> Statement.Statement a b
      -> m (Either HP.UsageError b)
runStatement a statement = do
      pool <- asks (^. T.connectionPool)
      liftIO $ HP.use pool (Session.statement a statement)
