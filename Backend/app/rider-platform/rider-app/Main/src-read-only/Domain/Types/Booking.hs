{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Booking (module Domain.Types.Booking, module ReExport) where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Client
import qualified Domain.Types.Common
import Domain.Types.Extra.Booking as ReExport
import qualified Domain.Types.Extra.Booking
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Quote
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Trip
import qualified Domain.Types.TripTerms
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data Booking = Booking
  { backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    bookingDetails :: Domain.Types.Booking.BookingDetails,
    bppBookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Booking.BPPBooking),
    bppEstimateId :: Kernel.Prelude.Text,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    discount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Types.Common.Price,
    estimatedStaticDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedTotalFare :: Kernel.Types.Common.Price,
    fromLocation :: Domain.Types.Location.Location,
    fulfillmentId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    initialPickupLocation :: Domain.Types.Location.Location,
    initiatedBy :: Kernel.Prelude.Maybe Domain.Types.Trip.TripParty,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBookingUpdated :: Kernel.Prelude.Bool,
    isDashboardRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId,
    paymentStatus :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.PaymentStatus,
    paymentUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    primaryExophone :: Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Types.Common.BaseUrl,
    quoteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Quote.Quote),
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.Extra.Booking.BookingStatus,
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    tripTerms :: Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, Show)

data AmbulanceBookingDetails = AmbulanceBookingDetails {distance :: Kernel.Types.Common.Distance, toLocation :: Domain.Types.Location.Location} deriving (Show)

data BPPBooking = BPPBooking {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingDetails
  = OneWayDetails Domain.Types.Booking.OneWayBookingDetails
  | RentalDetails Domain.Types.Booking.RentalBookingDetails
  | DriverOfferDetails Domain.Types.Booking.OneWayBookingDetails
  | OneWaySpecialZoneDetails Domain.Types.Booking.OneWaySpecialZoneBookingDetails
  | InterCityDetails Domain.Types.Booking.InterCityBookingDetails
  | AmbulanceDetails Domain.Types.Booking.AmbulanceBookingDetails
  | DeliveryDetails Domain.Types.Booking.DeliveryBookingDetails
  | OneWayScheduledDetails Domain.Types.Booking.OneWayScheduledBookingDetails
  deriving (Show)

data DeliveryBookingDetails = DeliveryBookingDetails {distance :: Kernel.Types.Common.Distance, otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, toLocation :: Domain.Types.Location.Location}
  deriving (Generic, Show)

data InterCityBookingDetails = InterCityBookingDetails {distance :: Kernel.Types.Common.Distance, otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, toLocation :: Domain.Types.Location.Location}
  deriving (Show)

data OneWayBookingDetails = OneWayBookingDetails {distance :: Kernel.Types.Common.Distance, toLocation :: Domain.Types.Location.Location} deriving (Show)

data OneWayScheduledBookingDetails = OneWayScheduledBookingDetails {distance :: Kernel.Types.Common.Distance, otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, toLocation :: Domain.Types.Location.Location}
  deriving (Show)

data OneWaySpecialZoneBookingDetails = OneWaySpecialZoneBookingDetails {distance :: Kernel.Types.Common.Distance, otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, toLocation :: Domain.Types.Location.Location}
  deriving (Show)

data RentalBookingDetails = RentalBookingDetails {otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, stopLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location} deriving (Show)
