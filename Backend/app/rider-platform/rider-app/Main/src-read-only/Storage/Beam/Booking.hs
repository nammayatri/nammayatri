{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Booking where

import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.BookingStatus
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Extra.Booking
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.ParcelType
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Trip
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import qualified Kernel.Utils.Common
import qualified SharedLogic.Type
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    billingCategory :: B.C f (Kernel.Prelude.Maybe SharedLogic.Type.BillingCategory),
    distance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    fareProductType :: B.C f Domain.Types.FarePolicy.FareProductType.FareProductType,
    isUpgradedToCab :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    otpCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    parcelQuantity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    parcelType :: B.C f (Kernel.Prelude.Maybe Domain.Types.ParcelType.ParcelType),
    stopLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppBookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    itemId :: B.C f Kernel.Prelude.Text,
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    configInExperimentVersions :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    disabilityTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    discount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverInsuredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    estimatedDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    estimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    estimatedStaticDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedTotalFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fulfillmentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hasStops :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    initiatedBy :: B.C f (Kernel.Prelude.Maybe Domain.Types.Trip.TripParty),
    insuredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isBookingUpdated :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isDashboardRequest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isInsured :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isMultimodalSearch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isPetRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isReferredRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    multimodalSearchRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentInstrument :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument),
    paymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId),
    paymentStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.Booking.PaymentStatus),
    paymentUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    preferSafetyPlus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    primaryExophone :: B.C f Kernel.Prelude.Text,
    providerId :: B.C f Kernel.Prelude.Text,
    providerUrl :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    recentLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    returnTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    riderId :: B.C f Kernel.Prelude.Text,
    roundTrip :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    serviceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    serviceTierShortDesc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.BookingStatus.BookingStatus,
    riderTransactionId :: B.C f Kernel.Prelude.Text,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    tripTermsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory),
    vehicleIconUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleServiceTierAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    vehicleServiceTierSeatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleVariant :: B.C f Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f = BookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingId . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['bppBookingId], ['quoteId], ['riderId], ['riderTransactionId]])

$(mkTableInstancesWithTModifier ''BookingT "booking" [("bppBookingId", "bpp_ride_booking_id"), ("riderTransactionId", "transaction_id")])
