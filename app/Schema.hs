{-# LANGUAGE FunctionalDependencies #-}
module Schema where

import           Protolude
import           Conf                           ( MigrationsPath(..) )
import           Control.Lens.TH                ( declareClassy )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Bifunctor                 ( first )
import           Hasql.Migration
import           Hasql.Pool
import           Hasql.Transaction.Sessions
import           Data.UUID.Types                ( UUID )
import           Data.Time.LocalTime            ( LocalTime )
import           Data.Text                      ( Text )


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


declareClassy [d|
  data Create t
      = Create
        { _createT :: t
        , _createLastUpdated :: LocalTime
        , _createCreatedAt :: LocalTime
        }

  data Update t
      = Update
        { _updateT :: t
        , _updateLastUpdated :: LocalTime
        }

  data Timestamped t
      = Timestamped
        { _timestampedT
        , _timestampedCreatedAt :: LocalTime
        , timestamptedLastUpdated :: LocalTime
        }

  data User
    = User
      { _userUuid :: UUID
      , _userUsername :: Text
      , _userHashedPassword :: Text
      , _userPublicKey :: Text
      }

  data Journal
    = Journal
      { _journalUuid :: UUID
      , _journalUserUuid :: UUID
      , _journalTitlteContent :: Text
      , _journalContent :: Text
      }

  |]
