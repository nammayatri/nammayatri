{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TowingRequest where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TowingRequest
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data TowingRequestT f = TowingRequestT
  { agencyEntityId :: B.C f Kernel.Prelude.Text,
    agentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    evidence :: B.C f Data.Aeson.Value,
    id :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    notes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    parkingFine :: B.C f Kernel.Types.Common.HighPrecMoney,
    paymentMethod :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentReceivedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    paymentReceivedBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentStatus :: B.C f Domain.Types.TowingRequest.TowingPaymentStatus,
    pickupLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    pickupLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    releasedToIdNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    releasedToIdNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    releasedToIdType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    releasedToName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    releasedToPhoneEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    releasedToPhoneHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    releasedToRelation :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestNumber :: B.C f Kernel.Prelude.Text,
    route :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    status :: B.C f Domain.Types.TowingRequest.TowingStatus,
    storageFeePerDay :: B.C f Kernel.Types.Common.HighPrecMoney,
    ticketServiceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    totalFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    towingFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    towingVehicleId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleNumber :: B.C f Kernel.Prelude.Text,
    vehicleType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    violationId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TowingRequestT where
  data PrimaryKey TowingRequestT f = TowingRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TowingRequestId . id

type TowingRequest = TowingRequestT Identity

$(enableKVPG ''TowingRequestT ['id] [['agencyEntityId], ['vehicleNumber], ['violationId]])

$(mkTableInstances ''TowingRequestT "towing_request")
