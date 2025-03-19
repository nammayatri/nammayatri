{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetControlGroup where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetControlGroupT f = FleetControlGroupT
  { fleetControlGroupId :: (B.C f Data.Text.Text),
    fleetControlGroupName :: (B.C f Data.Text.Text),
    fleetOwnerId :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetControlGroupT where
  data PrimaryKey FleetControlGroupT f = FleetControlGroupId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetControlGroupId . fleetControlGroupId

type FleetControlGroup = FleetControlGroupT Identity

$(enableKVPG (''FleetControlGroupT) [('fleetControlGroupId)] [])

$(mkTableInstances (''FleetControlGroupT) "fleet_control_group")
