{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.GTFSFeedInfo where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data GTFSFeedInfoT f = GTFSFeedInfoT
  { feedId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    vehicleType :: (B.C f BecknV2.FRFS.Enums.VehicleCategory),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table GTFSFeedInfoT where
  data PrimaryKey GTFSFeedInfoT f = GTFSFeedInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = GTFSFeedInfoId . feedId

type GTFSFeedInfo = GTFSFeedInfoT Identity

$(enableKVPG (''GTFSFeedInfoT) [('feedId)] [])

$(mkTableInstances (''GTFSFeedInfoT) "gtfs_feed_info")
