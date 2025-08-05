{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedEntity where

import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.SharedEntity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SharedEntityT f = SharedEntityT
  { bapSharedEntityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bookingIds :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverQuoteIds :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    entityType :: B.C f Domain.Types.SharedEntity.SharedEntityType,
    estimateIds :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    pairingTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rideIds :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    searchRequestForDriverIds :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    searchRequestIds :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    status :: B.C f Domain.Types.SharedEntity.SharedEntityStatus,
    totalSeats :: B.C f Kernel.Prelude.Int,
    transactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tripCategory :: B.C f Domain.Types.Common.TripCategory,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f BecknV2.OnDemand.Enums.VehicleCategory,
    waypoints :: B.C f Data.Aeson.Value
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedEntityT where
  data PrimaryKey SharedEntityT f = SharedEntityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedEntityId . id

type SharedEntity = SharedEntityT Identity

$(enableKVPG ''SharedEntityT ['id] [['driverId], ['transactionId]])

$(mkTableInstances ''SharedEntityT "shared_entity")
