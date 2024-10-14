{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequest where

import Data.Aeson
import Domain.Types.Extra.LocationInstance ()
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderDetails
import qualified Domain.Types.Trip
import qualified Domain.Types.YudhishthiraTH as T
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH
import qualified Tools.Maps

data SearchRequest = SearchRequest
  { area :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    bapCity :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    bapCountry :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country,
    bapId :: Kernel.Prelude.Text,
    bapUri :: Kernel.Types.Common.BaseUrl,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    customerLanguage :: Kernel.Prelude.Maybe Tools.Maps.Language,
    customerNammaTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverDefaultExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLocation :: Domain.Types.Location.Location,
    hasStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    isAdvanceBookingEnabled :: Kernel.Prelude.Bool,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDashboardRequest :: Kernel.Prelude.Bool,
    isReallocationEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    messageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupZoneGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    poolingConfigVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    poolingLogicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails),
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    searchTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.UTCTime,
    stops :: [Domain.Types.Location.Location],
    toLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data T = T
  { lon :: Double,
    lat :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TT = TT
  { lon :: Double,
    lat :: Double,
    name :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

$(T.generateAllDefault ''Lib.Types.SpecialLocation.Area [("Area", ["Default"])])
$(T.generateAllDefault ''Kernel.Types.Beckn.Context.City [("City", ["Bangalore"])])
$(T.generateAllDefault ''Kernel.Types.Beckn.Context.Country [("Country", ["India"])])
$(T.generateAllDefault ''Kernel.Types.Common.Currency [("Currency", ["INR"])])
$(T.generateAllDefault ''Tools.Maps.Language [("Language", ["ENGLISH"])])
$(T.generateAllDefault ''Kernel.Types.Common.DistanceUnit [("DistanceUnit", ["Meter"])])
$(T.generateAllDefault ''Domain.Types.Trip.TripMode [("TripMode", ["RideOtp"])])
$(T.generateAllDefault ''Domain.Types.Trip.OneWayMode [("OneWayMode", ["OneWayRideOtp"])])
$(T.generateAllDefault ''Domain.Types.Trip.TripCategory [])
$(T.generateAllDefault ''SearchRequest [("tripCategory", ["Just (OneWay_OneWayRideOtp)"])])
