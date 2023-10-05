{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
-- TODO check this is safe
{-# LANGUAGE UndecidableSuperClasses #-}

module Storage.DBModel where

import qualified Data.Serialize as Serialize
import Database.Beam.Postgres (Postgres)
import EulerHS.KVConnector.Types (MeshMeta)
import Kernel.Prelude
import qualified Sequelize

class
  ( Sequelize.Model Postgres table,
    Show (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    MeshMeta Postgres table,
    Serialize.Serialize (table Identity)
  ) =>
  IsDbTable (table :: (Type -> Type) -> Type)

data DBModel = AppInstalls | BlackListOrg | Booking
  deriving stock (Show, Read, Enum, Bounded, Generic)
  deriving anyclass (FromJSON)
