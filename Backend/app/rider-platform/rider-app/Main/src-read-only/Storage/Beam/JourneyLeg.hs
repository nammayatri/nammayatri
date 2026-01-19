{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyLeg where

import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.JourneyLeg
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data JourneyLegT f = JourneyLegT
  { agencyGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    agencyName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    busConductorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    busDriverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    busLocationData :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    changedBusesInSequence :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    distance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    duration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    endLocationLat :: B.C f Kernel.Prelude.Double,
    endLocationLon :: B.C f Kernel.Prelude.Double,
    estimatedMaxFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedMinFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    finalBoardedBusNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    finalBoardedBusNumberSource :: B.C f (Kernel.Prelude.Maybe Domain.Types.JourneyLeg.BusBoardingMethod),
    finalBoardedBusServiceTierType :: B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType),
    finalBoardedDepotNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    finalBoardedScheduleNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    finalBoardedWaybillId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopPlatformCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    groupCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isDeleted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    legPricingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    legId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    serviceTypes :: B.C f (Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType]),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    mode :: B.C f Domain.Types.Common.MultimodalTravelMode,
    multimodalSearchRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    osmEntrance :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    osmExit :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    providerRouteId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sequenceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    startLocationLat :: B.C f Kernel.Prelude.Double,
    startLocationLon :: B.C f Kernel.Prelude.Double,
    straightLineEntrance :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    straightLineExit :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    toArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopPlatformCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    userBookedBusServiceTierType :: B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyLegT where
  data PrimaryKey JourneyLegT f = JourneyLegId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyLegId . id

type JourneyLeg = JourneyLegT Identity

$(enableKVPG ''JourneyLegT ['id] [['groupCode], ['journeyId], ['legId]])

$(mkTableInstances ''JourneyLegT "journey_leg")
