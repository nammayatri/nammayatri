{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetMemberAssociation where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetMemberAssociationT f = FleetMemberAssociationT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    enabled :: (B.C f Kernel.Prelude.Bool),
    fleetMemberId :: (B.C f Kernel.Prelude.Text),
    fleetOwnerId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetMemberAssociationT where
  data PrimaryKey FleetMemberAssociationT f = FleetMemberAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetMemberAssociationId . fleetMemberId

type FleetMemberAssociation = FleetMemberAssociationT Identity

$(enableKVPG (''FleetMemberAssociationT) [('fleetMemberId)] [[('fleetOwnerId)]])

$(mkTableInstances (''FleetMemberAssociationT) "fleet_member_association")