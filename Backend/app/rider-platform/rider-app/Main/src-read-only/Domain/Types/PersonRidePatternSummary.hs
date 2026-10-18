{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonRidePatternSummary (module Domain.Types.PersonRidePatternSummary, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.PersonRidePatternSummary as ReExport
import qualified Domain.Types.Extra.PersonRidePatternSummary
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonRidePatternSummary = PersonRidePatternSummary
  { analysisWindowDays :: Kernel.Prelude.Int,
    cancelledTaxiRides :: Kernel.Prelude.Int,
    completedTaxiRides :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    dayOfWeekCounts :: [Kernel.Prelude.Int],
    freeFormSummary :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hourOfDayCounts :: [Kernel.Prelude.Int],
    lastSummarizedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastTripAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    medianFareByService :: Domain.Types.Extra.PersonRidePatternSummary.MoneyByServiceMap,
    medianTipValue :: Domain.Types.Extra.PersonRidePatternSummary.MoneyByServiceMap,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    publicTransportTrips :: Kernel.Prelude.Int,
    routePatterns :: Domain.Types.Extra.PersonRidePatternSummary.RoutePatternMap,
    scheduledTaxiRides :: Kernel.Prelude.Int,
    serviceTakenCounts :: Domain.Types.Extra.PersonRidePatternSummary.ServiceCountMap,
    tipComfortability :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    totalTrips :: Kernel.Prelude.Int,
    uniqueRoutesTravelled :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    windowEnd :: Kernel.Prelude.UTCTime,
    windowStart :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
