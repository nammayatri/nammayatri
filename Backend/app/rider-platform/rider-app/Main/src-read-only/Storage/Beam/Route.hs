{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Route where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude hiding (sequence)
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data RouteT f = RouteT
  { endLat :: (B.C f Kernel.Prelude.Double),
    endLon :: (B.C f Kernel.Prelude.Double),
    longName :: (B.C f Kernel.Prelude.Text),
    routeId :: (B.C f Kernel.Prelude.Text),
    shortName :: (B.C f Kernel.Prelude.Text),
    startLat :: (B.C f Kernel.Prelude.Double),
    startLon :: (B.C f Kernel.Prelude.Double),
    timeBounds :: (B.C f Kernel.Types.TimeBound.TimeBound),
    vehicleType :: (B.C f Domain.Types.Station.FRFSVehicleType),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteT where
  data PrimaryKey RouteT f = RouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteId . routeId

type Route = RouteT Identity

$(enableKVPG (''RouteT) [('routeId)] [])

$(mkTableInstances (''RouteT) "route")
