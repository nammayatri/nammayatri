{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Time
import Tools.Beam.UtilsTH

data RideDetailsT f = RideDetailsT
  { createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    defaultServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverName :: B.C f Kernel.Prelude.Text,
    driverNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    fleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rcId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleAge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Months),
    vehicleClass :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleColor :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleModel :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleNumber :: B.C f Kernel.Prelude.Text,
    vehicleVariant :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideDetailsT where
  data PrimaryKey RideDetailsT f = RideDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideDetailsId . id

type RideDetails = RideDetailsT Identity

$(enableKVPG ''RideDetailsT ['id] [['driverNumberHash]])

$(mkTableInstances ''RideDetailsT "ride_details")
