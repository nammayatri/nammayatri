{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleServiceTier where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data VehicleServiceTierT f = VehicleServiceTierT
  { airConditionedThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    allowedVehicleVariant :: B.C f [Domain.Types.VehicleVariant.VehicleVariant],
    autoSelectedVehicleVariant :: B.C f [Domain.Types.VehicleVariant.VehicleVariant],
    baseVehicleServiceTier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    defaultForVehicleVariant :: B.C f [Domain.Types.VehicleVariant.VehicleVariant],
    driverRating :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    fareAdditionPerKmOverBaseServiceTier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    id :: B.C f Kernel.Prelude.Text,
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isIntercityEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isRentalsEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    longDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    luggageCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    oxygen :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    priority :: B.C f Kernel.Prelude.Int,
    scheduleBookingListEligibilityTags :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    seatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    serviceTierType :: B.C f Domain.Types.Common.ServiceTierType,
    shortDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    stopFcmSuppressCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    stopFcmThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    vehicleIconUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleRating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    ventilator :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleServiceTierT where
  data PrimaryKey VehicleServiceTierT f = VehicleServiceTierId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleServiceTierId . id

type VehicleServiceTier = VehicleServiceTierT Identity

$(enableKVPG ''VehicleServiceTierT ['id] [])

$(mkTableInstances ''VehicleServiceTierT "vehicle_service_tier")
