{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateRoute where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateRouteT f = CorporateRouteT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    shiftId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    direction :: B.C f Kernel.Prelude.Text,
    estimatedDurationMinutes :: B.C f Kernel.Prelude.Int,
    estimatedDistanceMeters :: B.C f Kernel.Prelude.Int,
    vehicleTier :: B.C f Kernel.Prelude.Text,
    maxCapacity :: B.C f Kernel.Prelude.Int,
    polyline :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateRouteT where
  data PrimaryKey CorporateRouteT f = CorporateRouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateRouteId . id

type CorporateRoute = CorporateRouteT Identity

$(enableKVPG ''CorporateRouteT ['id] [['corporateEntityId], ['shiftId]])

$(mkTableInstances ''CorporateRouteT "corporate_route")
