{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetBadgeAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FleetBadgeType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetBadgeAssociationT f = FleetBadgeAssociationT
  { associatedOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    associatedTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    badgeId :: B.C f Kernel.Prelude.Text,
    badgeType :: B.C f Domain.Types.FleetBadgeType.FleetBadgeType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    fleetOwnerId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isActive :: B.C f Kernel.Prelude.Bool,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetBadgeAssociationT where
  data PrimaryKey FleetBadgeAssociationT f = FleetBadgeAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetBadgeAssociationId . id

type FleetBadgeAssociation = FleetBadgeAssociationT Identity

$(enableKVPG ''FleetBadgeAssociationT ['id] [['badgeId], ['badgeType], ['driverId], ['fleetOwnerId]])

$(mkTableInstances ''FleetBadgeAssociationT "fleet_badge_association")
