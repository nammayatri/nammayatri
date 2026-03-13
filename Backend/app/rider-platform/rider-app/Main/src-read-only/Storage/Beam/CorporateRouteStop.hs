{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateRouteStop where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateRouteStopT f = CorporateRouteStopT
  { id :: B.C f Kernel.Prelude.Text,
    routeId :: B.C f Kernel.Prelude.Text,
    sequence :: B.C f Kernel.Prelude.Int,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    address :: B.C f Kernel.Prelude.Text,
    estimatedArrivalOffset :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateRouteStopT where
  data PrimaryKey CorporateRouteStopT f = CorporateRouteStopId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateRouteStopId . id

type CorporateRouteStop = CorporateRouteStopT Identity

$(enableKVPG ''CorporateRouteStopT ['id] [['routeId]])

$(mkTableInstances ''CorporateRouteStopT "corporate_route_stop")
