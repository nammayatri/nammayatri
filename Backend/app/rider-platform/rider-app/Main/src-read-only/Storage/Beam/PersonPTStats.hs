{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonPTStats where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PersonPTStats
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonPTStatsT f = PersonPTStatsT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    lastPurchasedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    passTypeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    personId :: B.C f Kernel.Prelude.Text,
    productType :: B.C f Domain.Types.PersonPTStats.FRFSProductType,
    purchaseCount :: B.C f Kernel.Prelude.Int,
    staticPersonId :: B.C f Kernel.Prelude.Text,
    ticketCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleServiceTierType :: B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType),
    vehicleType :: B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonPTStatsT where
  data PrimaryKey PersonPTStatsT f = PersonPTStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonPTStatsId . id

type PersonPTStats = PersonPTStatsT Identity

$(enableKVPG ''PersonPTStatsT ['id] [['personId], ['staticPersonId]])

$(mkTableInstances ''PersonPTStatsT "person_pt_stats")
