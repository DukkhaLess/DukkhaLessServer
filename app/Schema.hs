module Schema where

import           Protolude
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Bifunctor                 ( first )
import           Hasql.Connection
import           Hasql.Migration
import           Hasql.Session
import           Hasql.Transaction.Sessions
import           Data.UUID.Types                ( UUID )
import           Data.Time.LocalTime            ( LocalTime )
import           Data.Text                      ( Text )

data User
  = User
    { _userUuid :: UUID
    , _userUsername :: Text
    , _userHashedPassword :: Text
    , _userPublicKey :: Text
    , _userLastUpdated :: LocalTime
    , _userCreatedAt :: LocalTime
    }

data MigrationFailureReason
  = QueryFailureReason QueryError
  | MigrationErrorReason MigrationError

runMigrations :: FilePath -> Connection -> IO (Either MigrationFailureReason ())
runMigrations p conn = do
  commands <- loadMigrationsFromDirectory p
  let transactions = map runMigration commands
  let sessions     = map (transaction Serializable Write) transactions
  let executableQueries = map
        (ExceptT . (map (first QueryFailureReason)) . flip run conn)
        sessions
  let queries = map
        (maybe (pure ()) (ExceptT . pure . Left . MigrationErrorReason) =<<)
        executableQueries
  result <- runExceptT $ sequence queries
  pure $ map (const ()) result
