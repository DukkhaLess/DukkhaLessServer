module Schema where

import           Protolude                      ( Show
                                                , Eq
                                                , Identity
                                                , (.)
                                                )
import           Database.Beam
import           Data.Text                      ( Text )

data UserT f
  = User
    { _userUuid :: Columnar f Text
    , _userUsername :: Columnar f Text
    , _userHashedPassword :: Columnar f Text
    , _userPublicKey :: Columnar f Text
    }
    deriving (Generic)
instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userUuid

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserId = PrimaryKey UserT Identity
instance Beamable (PrimaryKey UserT)

data DukkhalessDb f
  = DukkhalessDb
    { _dukkalessUsers :: f (TableEntity UserT)
    }
    deriving Generic

instance Database be DukkhalessDb

dukkhalessDb :: DatabaseSettings be DukkhalessDb
dukkhalessDb = defaultDbSettings
