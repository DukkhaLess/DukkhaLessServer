{-# LANGUAGE Rank2Types #-}
module Queries.Encoders where

import           Data.Functor.Contravariant     ( contramap )
import           Control.Lens
import qualified Hasql.Encoders                as HE
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import qualified Types                         as T

text :: forall a . T.HasText a Text => HE.Value a
text = contramap (^. T.text) HE.text

timestamptz :: forall a . T.HasUTCTime a UTCTime => HE.Value a
timestamptz = contramap (^. T.uTCTime) HE.timestamptz

uuid :: forall a . T.HasUUID a UUID => HE.Value a
uuid = contramap (^. T.uUID) HE.uuid
