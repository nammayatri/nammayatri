{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Vehicle where

import qualified Data.Aeson
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Vehicle
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleT f = VehicleT
  { airConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    capacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    category :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    color :: B.C f Kernel.Prelude.Text,
    downgradeReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverId :: B.C f Kernel.Prelude.Text,
    energyType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    luggageCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    mYManufacturing :: B.C f (Kernel.Prelude.Maybe Data.Time.Calendar.Day),
    make :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    model :: B.C f Kernel.Prelude.Text,
    oxygen :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    registrationCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.RegistrationCategory),
    registrationNo :: B.C f Kernel.Prelude.Text,
    ruleBasedUpgradeTiers :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    selectedServiceTiers :: B.C f [Domain.Types.Common.ServiceTierType],
    size :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    variant :: B.C f Domain.Types.VehicleVariant.VehicleVariant,
    vehicleClass :: B.C f Kernel.Prelude.Text,
    vehicleName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleRating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    vehicleTags :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    ventilator :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleT where
  data PrimaryKey VehicleT f = VehicleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleId . driverId

type Vehicle = VehicleT Identity

$(enableKVPG ''VehicleT ['driverId] [['registrationNo]])

$(mkTableInstances ''VehicleT "vehicle")
