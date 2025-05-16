{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSSearch where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.JourneyLeg.Types
import Tools.Beam.UtilsTH

data FRFSSearchT f = FRFSSearchT
  { fromStationId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isOnSearchReceived :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    agency :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    convenienceCost :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    isDeleted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    journeyLegOrder :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onSearchFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    pricingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    skipBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyLegStatus :: B.C f (Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgTransactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    quantity :: B.C f Kernel.Prelude.Int,
    recentLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    riderId :: B.C f Kernel.Prelude.Text,
    routeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStationId :: B.C f Kernel.Prelude.Text,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSSearchT where
  data PrimaryKey FRFSSearchT f = FRFSSearchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSSearchId . id

type FRFSSearch = FRFSSearchT Identity

$(enableKVPG ''FRFSSearchT ['id] [['riderId]])

$(mkTableInstances ''FRFSSearchT "frfs_search")
