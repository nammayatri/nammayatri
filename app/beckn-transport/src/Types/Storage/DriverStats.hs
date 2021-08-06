module Types.Storage.DriverStats where

import Beckn.Types.Id
import Data.Time (UTCTime (..))
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App

data DriverStatsT f = DriverStats
  { driverId :: B.C f (Id Driver),
    idleSince :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverStats = DriverStatsT Identity

type DriverStatsPrimaryKey = B.PrimaryKey DriverStatsT Identity

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f = DriverStatsPrimaryKey (B.C f (Id Driver))
    deriving (Generic, B.Beamable)
  primaryKey = DriverStatsPrimaryKey . driverId

deriving instance FromJSON DriverStats

deriving instance ToJSON DriverStats

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverStatsT)
fieldEMod =
  B.setEntityName "driver_stats"
    <> B.modifyTableFields
      B.tableModification
        { driverId = "driver_id",
          idleSince = "idle_since"
        }
