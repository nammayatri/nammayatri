{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Booking where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.DeliveryPersonDetails
import qualified Domain.Types.Estimate
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.FareParameters
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod
import qualified Domain.Types.ParcelType
import qualified Domain.Types.RiderDetails
import qualified Domain.Types.Trip
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Common
import Kernel.Utils.TH
import qualified Lib.Types.SpecialLocation
import qualified Lib.Yudhishthira.Types
import qualified SharedLogic.Type
import qualified Tools.Beam.UtilsTH

data Booking = Booking
  { area :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area,
    bapCity :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    bapCountry :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country,
    bapId :: Kernel.Prelude.Text,
    bapUri :: Kernel.Prelude.Text,
    billingCategory :: SharedLogic.Type.BillingCategory,
    coinsRewardedOnGoldTierRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Utils.Common.Currency,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distanceToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    dynamicPricingLogicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimateId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate),
    estimatedCongestionCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    exotelDeclinedCallStatusReceivingTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fareParams :: Domain.Types.FareParameters.FareParameters,
    fromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLocation :: Domain.Types.Location.Location,
    hasStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    initiatedAs :: Kernel.Prelude.Maybe Domain.Types.Trip.TripParty,
    insuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDashboardRequest :: Kernel.Prelude.Bool,
    isInsured :: Kernel.Prelude.Bool,
    isPetRide :: Kernel.Prelude.Bool,
    isReferredRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSafetyPlus :: Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    maxEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    numberOfLuggages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelType :: Kernel.Prelude.Maybe Domain.Types.ParcelType.ParcelType,
    paymentId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentInstrument :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    paymentMethodId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod),
    paymentUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    primaryExophone :: Kernel.Prelude.Text,
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    quoteId :: Kernel.Prelude.Text,
    receiverDetails :: Kernel.Prelude.Maybe Domain.Types.DeliveryPersonDetails.DeliveryPersonDetails,
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails),
    riderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    senderDetails :: Kernel.Prelude.Maybe Domain.Types.DeliveryPersonDetails.DeliveryPersonDetails,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialZoneOtpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.Booking.BookingStatus,
    stopLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Location.Location),
    stops :: [Domain.Types.Location.Location],
    toLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleServiceTierName :: Kernel.Prelude.Text,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data BookingStatus = NEW | TRIP_ASSIGNED | COMPLETED | CANCELLED | REALLOCATED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data BookingType = SpecialZoneBooking | NormalBooking deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BookingType)
