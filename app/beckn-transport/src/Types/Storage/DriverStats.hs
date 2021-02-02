module Types.Storage.DriverStats where

import Data.Time (UTCTime (..))
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App (DriverId)

data DriverStatsT f = DriverStats
  { _driverId :: B.C f DriverId,
    _idleSince :: B.C f UTCTime
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
          _idleSince = "idle_since"
        }
