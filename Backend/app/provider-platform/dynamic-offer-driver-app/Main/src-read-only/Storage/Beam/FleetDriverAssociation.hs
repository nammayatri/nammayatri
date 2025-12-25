{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetDriverAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetDriverAssociationT f = FleetDriverAssociationT
  { associatedOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    associatedTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    fleetOwnerId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isActive :: B.C f Kernel.Prelude.Bool,
    onboardedOperatorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    onboardingVehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    requestReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    responseReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetDriverAssociationT where
  data PrimaryKey FleetDriverAssociationT f = FleetDriverAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetDriverAssociationId . id

type FleetDriverAssociation = FleetDriverAssociationT Identity

$(enableKVPG ''FleetDriverAssociationT ['id] [['driverId], ['fleetOwnerId]])

$(mkTableInstances ''FleetDriverAssociationT "fleet_driver_association")
