{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Common
  ( module Beckn.Types.Common,
    module Common,
    HasField,
  )
where

import Beckn.External.Encryption as Common (EncFlow)
import Beckn.External.FCM.Types as Common (FCMFlow)
import Beckn.Storage.DB.Config as Common (DBFlow)
import Beckn.Storage.Esqueleto.Config as Common (EsqDBFlow)
import Beckn.Types.App as Common
import Beckn.Types.Forkable as Common
import Beckn.Types.GuidLike as Common
import Beckn.Types.Logging as Common
import Beckn.Types.MonadGuid as Common
import Beckn.Types.Monitoring.Kafka as Common
import Beckn.Types.Time as Common
import Beckn.Utils.Dhall (FromDhall)
import Data.Aeson
import Data.Generics.Labels ()
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra (HasField)

newtype IdObject = IdObject
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

class FromBeckn a b where
  fromBeckn :: a -> b

class ToBeckn a b where
  toBeckn :: b -> a

-- This type class is not strictly an isomorphism. We use the name 'Iso' to
-- denote that it is always expected that a type from the beckn spec should
-- have a corresponding type defined by us allowing conversion (which may be
-- lossy) between the two, when defined as an instance of this typeclass.
class (FromBeckn a b, ToBeckn a b) => BecknSpecIso a b

newtype Meters = Meters
  { getMeters :: Int
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum)
  deriving stock (Generic)

newtype Kilometers = Kilometers
  { getKilometers :: Double
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Fractional, Real, Ord, Eq, Enum)
  deriving stock (Generic)

type HasInConfig env config field t =
  ( HasField "config" env config,
    HasField field config t
  )

askConfig ::
  (MonadReader r m, HasField "config" r c) =>
  (c -> a) ->
  m a
askConfig f = asks (f . (.config))
