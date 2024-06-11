{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleServiceTier where

import qualified Database.Beam as B
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data VehicleServiceTierT f = VehicleServiceTierT
  { airConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    allowedVehicleVariant :: B.C f [Domain.Types.Vehicle.Variant],
    autoSelectedVehicleVariant :: B.C f [Domain.Types.Vehicle.Variant],
    defaultForVehicleVariant :: B.C f [Domain.Types.Vehicle.Variant],
    driverRating :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    id :: B.C f Kernel.Prelude.Text,
    longDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    luggageCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    oxygen :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    priority :: B.C f Kernel.Prelude.Int,
    seatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    serviceTierType :: B.C f Domain.Types.ServiceTierType.ServiceTierType,
    shortDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
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

$(enableKVPG ''VehicleServiceTierT ['id] [['merchantOperatingCityId]])

$(mkTableInstances ''VehicleServiceTierT "vehicle_service_tier")

{-
	DSL Source Link: file://./../../../spec/Storage/VehicleServiceTier.yaml
-}
