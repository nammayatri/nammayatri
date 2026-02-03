{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Booking where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.Booking
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.ParcelType
import qualified Domain.Types.Trip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import qualified SharedLogic.Type
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { area :: B.C f (Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area),
    bapCity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City),
    bapCountry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country),
    bapId :: B.C f Kernel.Prelude.Text,
    bapUri :: B.C f Kernel.Prelude.Text,
    billingCategory :: B.C f (Kernel.Prelude.Maybe SharedLogic.Type.BillingCategory),
    coinsRewardedOnGoldTierRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    commission :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    configInExperimentVersions :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    disabilityTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    displayBookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distanceToPickup :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    dynamicPricingLogicVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    estimateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    estimatedCongestionCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    exotelDeclinedCallStatusReceivingTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fareParametersId :: B.C f Kernel.Prelude.Text,
    fromLocGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hasStops :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    initiatedAs :: B.C f (Kernel.Prelude.Maybe Domain.Types.Trip.TripParty),
    insuredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isDashboardRequest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isInsured :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isPetRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isReferredRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isSafetyPlus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxEstimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    numberOfLuggages :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    parcelQuantity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    parcelType :: B.C f (Kernel.Prelude.Maybe Domain.Types.ParcelType.ParcelType),
    paymentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentInstrument :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument),
    paymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentMode :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode),
    paymentUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    primaryExophone :: B.C f Kernel.Prelude.Text,
    providerId :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f Kernel.Prelude.Text,
    receiverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    receiverName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    receiverPrimaryExophone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    returnTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    riderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    riderName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    roundTrip :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    senderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    senderName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    senderPrimaryExophone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialZoneOtpCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.Booking.BookingStatus,
    stopLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toLocGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tollIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    tollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    transactionId :: B.C f Kernel.Prelude.Text,
    bookingType :: B.C f Domain.Types.Booking.BookingType,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleVariant :: B.C f Domain.Types.Common.ServiceTierType,
    vehicleServiceTierAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleServiceTierSeatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f = BookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingId . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['quoteId], ['specialZoneOtpCode], ['transactionId]])

$(mkTableInstances ''BookingT "booking")
