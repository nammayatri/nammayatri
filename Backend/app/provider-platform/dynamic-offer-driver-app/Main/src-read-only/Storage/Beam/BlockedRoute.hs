{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BlockedRoute where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Utils.ComputeIntersection
import Tools.Beam.UtilsTH

data BlockedRouteT f = BlockedRouteT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    enabled :: B.C f Kernel.Prelude.Bool,
    endSegment :: B.C f Kernel.Utils.ComputeIntersection.LineSegment,
    id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    startSegment :: B.C f Kernel.Utils.ComputeIntersection.LineSegment,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table BlockedRouteT where
  data PrimaryKey BlockedRouteT f = BlockedRouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BlockedRouteId . id

type BlockedRoute = BlockedRouteT Identity

$(enableKVPG ''BlockedRouteT ['id] [])

$(mkTableInstances ''BlockedRouteT "blocked_route")
