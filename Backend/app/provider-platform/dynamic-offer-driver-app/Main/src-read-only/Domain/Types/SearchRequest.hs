{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequest where

import Data.Aeson
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ParcelType
import qualified Domain.Types.Person
import qualified Domain.Types.RiderDetails
import qualified Domain.Types.RiderPreferredOption
import qualified Domain.Types.Trip
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Lib.Types.SpecialLocation
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH
import qualified Tools.Maps

data SearchRequest = SearchRequest
  { area :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    bapCity :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    bapCountry :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country,
    bapId :: Kernel.Prelude.Text,
    bapUri :: Kernel.Types.Common.BaseUrl,
    cloudType :: Kernel.Prelude.Maybe Kernel.Types.Version.CloudType,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    customerLanguage :: Kernel.Prelude.Maybe Tools.Maps.Language,
    customerNammaTags :: Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValue],
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverDefaultExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverIdForSearch :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    dynamicPricingLogicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
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
    isReserveRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    messageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    numberOfLuggages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelType :: Kernel.Prelude.Maybe Domain.Types.ParcelType.ParcelType,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode,
    pickupGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupZoneGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    poolingConfigVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    poolingLogicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    preferSafetyPlus :: Kernel.Prelude.Bool,
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails),
    riderPreferredOption :: Domain.Types.RiderPreferredOption.RiderPreferredOption,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    searchTags :: Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValue],
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.UTCTime,
    stops :: [Domain.Types.Location.Location],
    toLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory,
    userBackendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    userBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    userClientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    userSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
