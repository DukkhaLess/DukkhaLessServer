module Schema where

import           Database.Beam
import           Schema.V0001

dukkhalessDb :: DatabaseSettings be DukkhalessDb
dukkhalessDb = defaultDbSettings
