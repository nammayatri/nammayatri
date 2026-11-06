{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OpenMeterRide where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.OpenMeterRide
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data OpenMeterRideT f = OpenMeterRideT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    distanceMeters :: (B.C f Kernel.Types.Common.Meters),
    encodedPolyline :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    endLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    endLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    endReason :: (B.C f (Kernel.Prelude.Maybe Domain.Types.OpenMeterRide.OpenMeterEndReason)),
    endTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    estimatedGapMeters :: (B.C f Kernel.Types.Common.Meters),
    fare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    id :: (B.C f Kernel.Prelude.Text),
    lastHeartbeatAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    lastLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    lastLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    personId :: (B.C f Kernel.Prelude.Text),
    pointsCount :: (B.C f Kernel.Prelude.Int),
    serverLinearDistanceMeters :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    serverStartedAt :: (B.C f Kernel.Prelude.UTCTime),
    startLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    startLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    startTime :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Domain.Types.OpenMeterRide.OpenMeterRideStatus),
    tariffSnapshot :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    trackingConfig :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    waitingTimeSeconds :: (B.C f Kernel.Types.Common.Seconds)
  }
  deriving (Generic, B.Beamable)

instance B.Table OpenMeterRideT where
  data PrimaryKey OpenMeterRideT f = OpenMeterRideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OpenMeterRideId . id

type OpenMeterRide = OpenMeterRideT Identity

$(enableKVPG (''OpenMeterRideT) [('id)] [[('personId)]])

$(mkTableInstances (''OpenMeterRideT) "open_meter_ride")
