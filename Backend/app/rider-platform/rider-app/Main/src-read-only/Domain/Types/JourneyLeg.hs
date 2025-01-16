{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyLeg where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data JourneyLeg = JourneyLeg
  { agency :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalAgency,
    distance :: Kernel.Types.Common.Distance,
    duration :: Kernel.Types.Common.Seconds,
    endLocation :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2,
    estimatedMaxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedMinFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fromArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromStopDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails,
    id :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legSearchId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mode :: Domain.Types.Common.MultimodalTravelMode,
    routeDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalRouteDetails,
    sequenceNumber :: Kernel.Prelude.Int,
    startLocation :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2,
    toArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toStopDetails :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
