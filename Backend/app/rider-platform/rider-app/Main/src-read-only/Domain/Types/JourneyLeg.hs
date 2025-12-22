{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyLeg where

import qualified API.Types.UI.RiderLocation
import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RouteDetails
import qualified Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data JourneyLeg = JourneyLeg
  { agency :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalAgency,
    busConductorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    busDriverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    busLocationData :: [API.Types.UI.RiderLocation.BusLocation],
    changedBusesInSequence :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    endLocation :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2,
    estimatedMaxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedMinFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    finalBoardedBusNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    finalBoardedBusNumberSource :: Kernel.Prelude.Maybe Domain.Types.JourneyLeg.BusBoardingMethod,
    finalBoardedBusServiceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    finalBoardedDepotNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    finalBoardedScheduleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    finalBoardedWaybillId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromStopDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails,
    groupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg,
    isDeleted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legPricingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    legSearchId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    liveVehicleAvailableServiceTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    mode :: Domain.Types.Common.MultimodalTravelMode,
    multimodalSearchRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    osmEntrance :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    osmExit :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    routeDetails :: [Domain.Types.RouteDetails.RouteDetails],
    sequenceNumber :: Kernel.Prelude.Int,
    startLocation :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2,
    straightLineEntrance :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    straightLineExit :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    toArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toStopDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails,
    userBookedBusServiceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BusBoardingMethod = UserActivated | UserSpotBooked | Detected deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BusBoardingMethod)
