{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetCSV where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetCSVT f = FleetCSVT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    day :: (B.C f Data.Time.Calendar.Day),
    filePath :: (B.C f Data.Text.Text),
    fleetOwnerId :: (B.C f Data.Text.Text),
    merchantId :: (B.C f Data.Text.Text),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetCSVT where
  data PrimaryKey FleetCSVT f = FleetCSVId (B.C f Data.Time.Calendar.Day) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetCSVId <$> day <*> fleetOwnerId

type FleetCSV = FleetCSVT Identity

$(enableKVPG (''FleetCSVT) [('day), ('fleetOwnerId)] [])

$(mkTableInstances (''FleetCSVT) "fleet_csv")
