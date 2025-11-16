{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Booking (module Domain.Types.Booking, module ReExport) where

import Data.Aeson
import qualified Domain.Types.BookingStatus
import qualified Domain.Types.Client
import qualified Domain.Types.Common
import Domain.Types.Extra.Booking as ReExport
import qualified Domain.Types.Extra.Booking
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ParcelType
import qualified Domain.Types.Person
import qualified Domain.Types.Quote
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Trip
import qualified Domain.Types.TripTerms
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Lib.Yudhishthira.Types
import qualified SharedLogic.Type
import qualified Tools.Beam.UtilsTH

data Booking = Booking
  { backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    billingCategory :: SharedLogic.Type.BillingCategory,
    bookingDetails :: Domain.Types.Booking.BookingDetails,
    bppBookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Booking.BPPBooking),
    bppEstimateId :: Kernel.Prelude.Text,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    discount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverInsuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Types.Common.Price,
    estimatedStaticDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedTotalFare :: Kernel.Types.Common.Price,
    fromLocation :: Domain.Types.Location.Location,
    fulfillmentId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hasStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    initialPickupLocation :: Domain.Types.Location.Location,
    initiatedBy :: Kernel.Prelude.Maybe Domain.Types.Trip.TripParty,
    insuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBookingUpdated :: Kernel.Prelude.Bool,
    isDashboardRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isInsured :: Kernel.Prelude.Bool,
    isMultimodalSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPetRide :: Kernel.Prelude.Bool,
    isReferredRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    multimodalSearchRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentInstrument :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    paymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId,
    paymentStatus :: Kernel.Prelude.Maybe Domain.Types.Extra.Booking.PaymentStatus,
    paymentUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    preferSafetyPlus :: Kernel.Prelude.Bool,
    primaryExophone :: Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Types.Common.BaseUrl,
    quoteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Quote.Quote),
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.BookingStatus.BookingStatus,
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    tripTerms :: Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data AmbulanceBookingDetails = AmbulanceBookingDetails {distance :: Kernel.Types.Common.Distance, toLocation :: Domain.Types.Location.Location} deriving (Generic, Show, FromJSON, ToJSON)

data BPPBooking = BPPBooking {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingDetails
  = OneWayDetails Domain.Types.Booking.OneWayBookingDetails
  | RentalDetails Domain.Types.Booking.RentalBookingDetails
  | DriverOfferDetails Domain.Types.Booking.OneWayBookingDetails
  | OneWaySpecialZoneDetails Domain.Types.Booking.OneWaySpecialZoneBookingDetails
  | InterCityDetails Domain.Types.Booking.InterCityBookingDetails
  | AmbulanceDetails Domain.Types.Booking.AmbulanceBookingDetails
  | DeliveryDetails Domain.Types.Booking.DeliveryBookingDetails
  | MeterRideDetails Domain.Types.Booking.MeterRideBookingDetails
  deriving (Generic, Show, FromJSON, ToJSON)

data DeliveryBookingDetails = DeliveryBookingDetails
  { distance :: Kernel.Types.Common.Distance,
    otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    parcelQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelType :: Domain.Types.ParcelType.ParcelType,
    toLocation :: Domain.Types.Location.Location
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data InterCityBookingDetails = InterCityBookingDetails
  { distance :: Kernel.Types.Common.Distance,
    otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stops :: [Domain.Types.Location.Location],
    toLocation :: Domain.Types.Location.Location
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data MeterRideBookingDetails = MeterRideBookingDetails {distanceCovered :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance, toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location}
  deriving (Generic, Show, FromJSON, ToJSON)

data OneWayBookingDetails = OneWayBookingDetails
  { distance :: Kernel.Types.Common.Distance,
    isUpgradedToCab :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    stops :: [Domain.Types.Location.Location],
    toLocation :: Domain.Types.Location.Location
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OneWaySpecialZoneBookingDetails = OneWaySpecialZoneBookingDetails
  { distance :: Kernel.Types.Common.Distance,
    otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stops :: [Domain.Types.Location.Location],
    toLocation :: Domain.Types.Location.Location
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data RentalBookingDetails = RentalBookingDetails {otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, stopLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location}
  deriving (Generic, Show, FromJSON, ToJSON)
