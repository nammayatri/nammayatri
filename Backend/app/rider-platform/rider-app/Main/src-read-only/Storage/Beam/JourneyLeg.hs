{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyLeg where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data JourneyLegT f = JourneyLegT
  { distance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    duration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    endLocationLat :: B.C f Kernel.Prelude.Double,
    endLocationLon :: B.C f Kernel.Prelude.Double,
    estimatedMaxFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedMinFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fromArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopPlatformCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isDeleted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isSkipped :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyId :: B.C f Kernel.Prelude.Text,
    legId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mode :: B.C f Domain.Types.Common.MultimodalTravelMode,
    sequenceNumber :: B.C f Kernel.Prelude.Int,
    startLocationLat :: B.C f Kernel.Prelude.Double,
    startLocationLon :: B.C f Kernel.Prelude.Double,
    toArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopPlatformCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyLegT where
  data PrimaryKey JourneyLegT f = JourneyLegId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyLegId . id

type JourneyLeg = JourneyLegT Identity

$(enableKVPG ''JourneyLegT ['id] [['journeyId], ['legId]])

$(mkTableInstances ''JourneyLegT "journey_leg")
