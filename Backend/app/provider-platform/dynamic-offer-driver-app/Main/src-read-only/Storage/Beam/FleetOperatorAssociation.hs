{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetOperatorAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetOperatorAssociationT f = FleetOperatorAssociationT
  { associatedOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    associatedTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fleetOwnerId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isActive :: B.C f Kernel.Prelude.Bool,
    operatorId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetOperatorAssociationT where
  data PrimaryKey FleetOperatorAssociationT f = FleetOperatorAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetOperatorAssociationId . id

type FleetOperatorAssociation = FleetOperatorAssociationT Identity

$(enableKVPG ''FleetOperatorAssociationT ['id] [['fleetOwnerId], ['operatorId]])

$(mkTableInstances ''FleetOperatorAssociationT "fleet_operator_association")
