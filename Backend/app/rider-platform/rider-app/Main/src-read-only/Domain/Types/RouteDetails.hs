{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteDetails where

import Data.Aeson
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RouteDetails = RouteDetails
  { agencyGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    agencyName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endLocationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    endLocationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    frequency :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fromStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopPlatformCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails,
    journeyLegId :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg,
    routeColorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeColorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startLocationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    startLocationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    subLegOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    toArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toDepartureTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    toStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStopGtfsId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toStopPlatformCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
