{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateRouteStopAssignment where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateRouteStopAssignmentT f = CorporateRouteStopAssignmentT
  { id :: B.C f Kernel.Prelude.Text,
    routeStopId :: B.C f Kernel.Prelude.Text,
    employeeId :: B.C f Kernel.Prelude.Text,
    effectiveFrom :: B.C f Kernel.Prelude.UTCTime,
    effectiveTo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateRouteStopAssignmentT where
  data PrimaryKey CorporateRouteStopAssignmentT f = CorporateRouteStopAssignmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateRouteStopAssignmentId . id

type CorporateRouteStopAssignment = CorporateRouteStopAssignmentT Identity

$(enableKVPG ''CorporateRouteStopAssignmentT ['id] [['routeStopId], ['employeeId]])

$(mkTableInstances ''CorporateRouteStopAssignmentT "corporate_route_stop_assignment")
