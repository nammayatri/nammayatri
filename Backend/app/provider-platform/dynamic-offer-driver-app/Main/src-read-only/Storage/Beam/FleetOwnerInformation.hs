{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetOwnerInformation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FleetOwnerInformation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetOwnerInformationT f = FleetOwnerInformationT
  { blocked :: B.C f Kernel.Prelude.Bool,
    enabled :: B.C f Kernel.Prelude.Bool,
    fleetOwnerPersonId :: B.C f Kernel.Prelude.Text,
    fleetType :: B.C f Domain.Types.FleetOwnerInformation.FleetType,
    gstImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gstNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    verified :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetOwnerInformationT where
  data PrimaryKey FleetOwnerInformationT f = FleetOwnerInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetOwnerInformationId . fleetOwnerPersonId

type FleetOwnerInformation = FleetOwnerInformationT Identity

$(enableKVPG ''FleetOwnerInformationT ['fleetOwnerPersonId] [])

$(mkTableInstances ''FleetOwnerInformationT "fleet_owner_information")
