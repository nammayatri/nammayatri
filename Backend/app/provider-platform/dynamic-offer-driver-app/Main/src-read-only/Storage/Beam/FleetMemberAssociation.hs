{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetMemberAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetMemberAssociationT f = FleetMemberAssociationT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    enabled :: B.C f Kernel.Prelude.Bool,
    fleetMemberId :: B.C f Kernel.Prelude.Text,
    fleetOwnerId :: B.C f Kernel.Prelude.Text,
    groupCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isFleetOwner :: B.C f Kernel.Prelude.Bool,
    level :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    order :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    parentGroupCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetMemberAssociationT where
  data PrimaryKey FleetMemberAssociationT f = FleetMemberAssociationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetMemberAssociationId <$> fleetMemberId <*> fleetOwnerId

type FleetMemberAssociation = FleetMemberAssociationT Identity

$(enableKVPG ''FleetMemberAssociationT ['fleetMemberId, 'fleetOwnerId] [])

$(mkTableInstances ''FleetMemberAssociationT "fleet_member_association")
