{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyLeg where

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
import qualified Lib.JourneyLeg.Types
import qualified Tools.Beam.UtilsTH

data JourneyLeg = JourneyLeg
  { agency :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalAgency,
    changedBusesInSequence :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    endLocation :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2,
    entrance :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    estimatedMaxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedMinFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    exit :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalLegGate,
    finalBoardedBusNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromStopDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails,
    id :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg,
    isDeleted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkipped :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legSearchId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mode :: Domain.Types.Common.MultimodalTravelMode,
    routeDetails :: [Domain.Types.RouteDetails.RouteDetails],
    sequenceNumber :: Kernel.Prelude.Int,
    serviceTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    startLocation :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2,
    status :: Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus,
    toArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toStopDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
