{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.Ride where

import Data.Aeson
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.RefundRequest
import qualified Domain.Types.Ride
import qualified Domain.Types.RideStatus
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.StopInformation as DSI
import qualified Domain.Types.VehicleVariant
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified SharedLogic.Type as SLT

-- Extra code goes here --
data EditLocation = EditLocation
  { gps :: Maps.LatLong,
    address :: DLA.LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RideAPIEntity = RideAPIEntity
  { allowedEditLocationAttempts :: Kernel.Prelude.Int,
    allowedEditPickupLocationAttempts :: Kernel.Prelude.Int,
    cancellationFeeIfCancelled :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancellationChargesOnCancel :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bppRideId :: Kernel.Types.Id.Id Domain.Types.Ride.BPPRide,
    chargeableRideDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    chargeableRideDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    computedPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    computedPriceWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    createdAt :: Kernel.Prelude.UTCTime,
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    billingCategory :: SLT.BillingCategory,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    stopsInfo :: [DSI.StopInformation],
    driverNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRatings :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    driverRegisteredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    endOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    isFreeRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    talkedWithDriver :: Kernel.Prelude.Bool,
    onlinePayment :: Kernel.Prelude.Bool,
    feedbackSkipped :: Kernel.Prelude.Bool,
    isPetRide :: Kernel.Prelude.Bool,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideOtp :: Kernel.Prelude.Text,
    rideRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    shortRideId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    startOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    status :: Domain.Types.RideStatus.RideStatus,
    tollConfidence :: Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence,
    traveledRideDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleAge :: Kernel.Prelude.Maybe Kernel.Types.Time.Months,
    vehicleColor :: Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleServiceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    favCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isAlreadyFav :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    destinationReachedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    isSafetyPlus :: Kernel.Prelude.Bool,
    isInsured :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    insuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tipAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    paymentStatus :: Domain.Types.Ride.PaymentStatus,
    refundRequestStatus :: Maybe Domain.Types.RefundRequest.RefundRequestStatus,
    driverCancellationDeductionOnPreviousRide :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
