module Types.Storage.DriverStats where

import Beckn.Types.Amount (Amount)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App (DriverId)

data DriverStatsT f = DriverStats
  { _driverId :: B.C f DriverId,
    _completedRidesNumber :: B.C f Int,
    _earnings :: B.C f Amount,
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverStats = DriverStatsT Identity

type DriverStatsPrimaryKey = B.PrimaryKey DriverStatsT Identity

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f = DriverStatsPrimaryKey (B.C f DriverId)
    deriving (Generic, B.Beamable)
  primaryKey = DriverStatsPrimaryKey . _driverId

instance ToJSON DriverStats where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON DriverStats where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverStatsT)
fieldEMod =
  B.setEntityName "driver_stats"
    <> B.modifyTableFields
      B.tableModification
        { _driverId = "driver_id",
          _completedRidesNumber = "completed_rides_number",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
