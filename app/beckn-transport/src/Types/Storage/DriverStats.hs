module Types.Storage.DriverStats where

import Beckn.Types.ID
import Beckn.Types.Storage.Person (Driver)
import Data.Time (UTCTime (..))
import qualified Database.Beam as B
import EulerHS.Prelude

data DriverStatsT f = DriverStats
  { _driverId :: B.C f (ID Driver),
    _idleSince :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverStats = DriverStatsT Identity

type DriverStatsPrimaryKey = B.PrimaryKey DriverStatsT Identity

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f = DriverStatsPrimaryKey (B.C f (ID Driver))
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
