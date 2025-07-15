{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Quote where

import Data.Aeson
import qualified Domain.Action.UI.DriverOffer
import qualified Domain.Action.UI.SpecialZoneQuote
import qualified Domain.Types.Common
import qualified Domain.Types.DriverOffer
import qualified Domain.Types.InterCityDetails
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.QuoteBreakup
import qualified Domain.Types.RentalDetails
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SpecialZoneQuote
import qualified Domain.Types.TripTerms
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data Quote = Quote
  { backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    discount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    estimatedFare :: Kernel.Types.Common.Price,
    estimatedPickupDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedTotalFare :: Kernel.Types.Common.Price,
    id :: Kernel.Types.Id.Id Domain.Types.Quote.Quote,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSafetyPlus :: Kernel.Prelude.Bool,
    itemId :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Types.Common.BaseUrl,
    quoteBreakupList :: [Domain.Types.QuoteBreakup.QuoteBreakup],
    quoteDetails :: Domain.Types.Quote.QuoteDetails,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tollChargesInfo :: Kernel.Prelude.Maybe Domain.Types.Quote.TollChargesInfo,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    tripTerms :: Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, Show)

data MeterRideQuoteAPIDetails = MeterRideQuoteAPIDetails {quoteId :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MeterRideQuoteDetails = MeterRideQuoteDetails {quoteId :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OneWayQuoteAPIDetails = OneWayQuoteAPIDetails
  { distanceToNearestDriver :: Kernel.Types.Common.HighPrecMeters,
    distanceToNearestDriverWithUnit :: Kernel.Types.Common.Distance,
    quoteId :: Kernel.Prelude.Text,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OneWayQuoteDetails = OneWayQuoteDetails {distanceToNearestDriver :: Kernel.Types.Common.Distance, quoteId :: Kernel.Prelude.Text} deriving (Generic, Show)

data OneWaySpecialZoneQuoteAPIDetails = OneWaySpecialZoneQuoteAPIDetails {distanceToNearestDriver :: Kernel.Types.Common.HighPrecMeters, quoteId :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteAPIDetails
  = OneWayAPIDetails Domain.Types.Quote.OneWayQuoteAPIDetails
  | AmbulanceAPIDetails Domain.Action.UI.DriverOffer.DriverOfferAPIEntity
  | InterCityAPIDetails Domain.Types.InterCityDetails.InterCityDetailsAPIEntity
  | RentalAPIDetails Domain.Types.RentalDetails.RentalDetailsAPIEntity
  | DriverOfferAPIDetails Domain.Action.UI.DriverOffer.DriverOfferAPIEntity
  | OneWaySpecialZoneAPIDetails Domain.Action.UI.SpecialZoneQuote.SpecialZoneQuoteAPIEntity
  | DeliveryAPIDetails Domain.Action.UI.DriverOffer.DriverOfferAPIEntity
  | MeterRideAPIDetails Domain.Types.Quote.MeterRideQuoteAPIDetails
  deriving (Generic, Show)

data QuoteDetails
  = OneWayDetails Domain.Types.Quote.OneWayQuoteDetails
  | AmbulanceDetails Domain.Types.DriverOffer.DriverOffer
  | InterCityDetails Domain.Types.InterCityDetails.InterCityDetails
  | RentalDetails Domain.Types.RentalDetails.RentalDetails
  | DriverOfferDetails Domain.Types.DriverOffer.DriverOffer
  | OneWaySpecialZoneDetails Domain.Types.SpecialZoneQuote.SpecialZoneQuote
  | DeliveryDetails Domain.Types.DriverOffer.DriverOffer
  | MeterRideDetails Domain.Types.Quote.MeterRideQuoteDetails
  deriving (Generic, Show)

data TollChargesInfo = TollChargesInfo {tollCharges :: Kernel.Types.Common.Price, tollNames :: [Kernel.Prelude.Text]} deriving (Generic, Show)
