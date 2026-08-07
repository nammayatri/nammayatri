{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonUsageStats where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PersonUsageStats
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonUsageStatsT f = PersonUsageStatsT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    lastPurchasedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    passTypeId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    personId :: (B.C f Kernel.Prelude.Text),
    productType :: (B.C f Domain.Types.PersonUsageStats.FRFSProductType),
    purchaseCount :: (B.C f Kernel.Prelude.Int),
    staticPersonId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    ticketCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleServiceTierType :: (B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType)),
    vehicleType :: (B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory))
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonUsageStatsT where
  data PrimaryKey PersonUsageStatsT f = PersonUsageStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonUsageStatsId . id

type PersonUsageStats = PersonUsageStatsT Identity

$(enableKVPG (''PersonUsageStatsT) [('id)] [[('personId)], [('staticPersonId)]])

$(mkTableInstances (''PersonUsageStatsT) "person_usage_stats")
