{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.UnserviceableCityLead where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data UnserviceableCityLeadT f = UnserviceableCityLeadT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    deviceToken :: B.C f (Kernel.Prelude.Maybe Kernel.External.Notification.FCM.Types.FCMRecipientToken),
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    merchantId :: B.C f Kernel.Prelude.Text,
    mobileCountryCode :: B.C f Kernel.Prelude.Text,
    mobileNumberEncrypted :: B.C f Kernel.Prelude.Text,
    mobileNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    name :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, B.Beamable)

instance B.Table UnserviceableCityLeadT where
  data PrimaryKey UnserviceableCityLeadT f = UnserviceableCityLeadId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = UnserviceableCityLeadId . id

type UnserviceableCityLead = UnserviceableCityLeadT Identity

$(enableKVPG ''UnserviceableCityLeadT ['id] [])

$(mkTableInstances ''UnserviceableCityLeadT "unserviceable_city_lead")
