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
  { agencyGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    agencyName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distance :: B.C f Kernel.Types.Common.HighPrecDistance,
    distanceUnit :: B.C f Kernel.Types.Common.DistanceUnit,
    duration :: B.C f Kernel.Types.Common.Seconds,
    endLocationLat :: B.C f Kernel.Prelude.Double,
    endLocationLon :: B.C f Kernel.Prelude.Double,
    fromArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    journeyId :: B.C f Kernel.Prelude.Text,
    legId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mode :: B.C f Domain.Types.Common.MultimodalTravelMode,
    frequency :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    routeColorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeColorName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeLongName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeShortName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sequenceNumber :: B.C f Kernel.Prelude.Int,
    startLocationLat :: B.C f Kernel.Prelude.Double,
    startLocationLon :: B.C f Kernel.Prelude.Double,
    toArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
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

$(enableKVPG ''JourneyLegT ['id] [['journeyId]])

$(mkTableInstances ''JourneyLegT "journey_leg")