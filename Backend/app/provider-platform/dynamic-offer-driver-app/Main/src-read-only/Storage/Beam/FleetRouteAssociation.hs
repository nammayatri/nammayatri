{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetRouteAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetRouteAssociationT f = FleetRouteAssociationT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    fleetOwnerId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetRouteAssociationT where
  data PrimaryKey FleetRouteAssociationT f = FleetRouteAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetRouteAssociationId . id

type FleetRouteAssociation = FleetRouteAssociationT Identity

$(enableKVPG ''FleetRouteAssociationT ['id] [])

$(mkTableInstances ''FleetRouteAssociationT "fleet_route_association")
