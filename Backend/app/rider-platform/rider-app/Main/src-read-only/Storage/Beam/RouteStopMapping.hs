{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteStopMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude hiding (sequence)
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data RouteStopMappingT f = RouteStopMappingT
  { id :: (B.C f Kernel.Prelude.Text),
    routeId :: (B.C f Kernel.Prelude.Text),
    sequence :: (B.C f Kernel.Prelude.Int),
    stopId :: (B.C f Kernel.Prelude.Text),
    timeBounds :: (B.C f Kernel.Types.TimeBound.TimeBound),
    vehicleType :: (B.C f Domain.Types.Station.FRFSVehicleType),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteStopMappingT where
  data PrimaryKey RouteStopMappingT f = RouteStopMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteStopMappingId . id

type RouteStopMapping = RouteStopMappingT Identity

$(enableKVPG (''RouteStopMappingT) [('id)] [[('routeId)]])

$(mkTableInstances (''RouteStopMappingT) "route_stop_mapping")
