{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetControlGroupMemberAssociation where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetControlGroupMemberAssociationT f = FleetControlGroupMemberAssociationT
  { fleetControlGroupId :: (B.C f Data.Text.Text),
    fleetMemberId :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetControlGroupMemberAssociationT where
  data PrimaryKey FleetControlGroupMemberAssociationT f = FleetControlGroupMemberAssociationId (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetControlGroupMemberAssociationId <$> fleetControlGroupId <*> fleetMemberId

type FleetControlGroupMemberAssociation = FleetControlGroupMemberAssociationT Identity

$(enableKVPG (''FleetControlGroupMemberAssociationT) [('fleetControlGroupId), ('fleetMemberId)] [])

$(mkTableInstances (''FleetControlGroupMemberAssociationT) "fleet_control_group_member_association")
