{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.DriverLocation where

import Beckn.Types.Id
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.AST as B
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Syntax as B
import qualified Database.PostgreSQL.Simple.FromField as Pg
import EulerHS.Prelude hiding (id, state)
import Types.Storage.Person (Person)

data DriverLocationT f = DriverLocation
  { driverId :: B.C f (Id Person),
    lat :: B.C f Double,
    long :: B.C f Double,
    point :: B.C f Point,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverLocation = DriverLocationT Identity

type DriverLocationPrimaryKey = B.PrimaryKey DriverLocationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table DriverLocationT where
  data PrimaryKey DriverLocationT f = DriverLocationPrimaryKey (B.C f (Id Person))
    deriving (Generic, B.Beamable)
  primaryKey = DriverLocationPrimaryKey . driverId

deriving instance Show DriverLocation

deriving instance Eq DriverLocation

instance ToJSON DriverLocation

instance FromJSON DriverLocation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverLocationT)
fieldEMod =
  B.setEntityName "driver_location"
    <> B.modifyTableFields
      B.tableModification
        { driverId = "driver_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

data Point = Point
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, ToSchema)

instance HasSqlValueSyntax B.Value Point where
  sqlValueSyntax _ = sqlValueSyntax SqlNull

instance HasSqlValueSyntax B.PgValueSyntax Point where
  sqlValueSyntax _ = sqlValueSyntax SqlNull

instance FromBackendRow Postgres Point

instance Pg.FromField Point where
  fromField _ Nothing = return Point
  fromField _ (Just _) = return Point
