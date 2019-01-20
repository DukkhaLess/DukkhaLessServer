module Schema where

import           Protolude
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


runMigrations :: FilePath -> IO [Session (Maybe MigrationError)]
runMigrations p =
  loadMigrationsFromDirectory p
    <&> map runMigration
    <&> map (transaction Serializable Write)
