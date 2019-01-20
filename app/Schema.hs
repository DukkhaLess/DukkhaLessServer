module Schema where

import           Protolude
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


runMigrations :: FilePath -> Connection -> IO (Maybe MigrationError)
runMigrations p conn = do
  commands <- loadMigrationsFromDirectory p
  let transactions      = map runMigration commands
  let sessions          = map (transaction Serializable Write) transactions
  let executableQueries = map (flip run conn) sessions
  pure executableQueries
