{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Common types and functions to break import cycles between UI.Ride, UI.Ride.EndRide, and Dashboard.Ride
module Domain.Action.UI.Ride.Common
  ( DriverRideRes (..),
    mkDriverRideRes,
    Stop (..),
    DeliveryPersonDetailsAPIEntity (..),
    mkLocationFromLocationMapping,
    calculateLocations,
    makeStop,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.List (find)
import Control.Lens ((^?), _head)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import qualified Domain.Action.UI.Location as DLocUI
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as RD
import qualified Domain.Types.StopInformation as DSI
import qualified Domain.Types.VehicleVariant as DVeh
import GHC.Generics (Generic)
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common (BaseUrl, Distance, EncFlow, EsqDBFlow, HighPrecMeters, HighPrecMoney, Meters, Money, Months, PriceAPIEntity (..), Seconds, convertHighPrecMetersToDistance, convertMetersToDistance)
import Kernel.Types.Confidence (Confidence)
import Kernel.Types.Id
import SharedLogic.FareCalculator (fareSum)
import SharedLogic.Type (BillingCategory)
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.LocationMapping as QLM
import Prelude hiding (id)

-- DeliveryPersonDetailsAPIEntity type (match Ride.hs)
data DeliveryPersonDetailsAPIEntity = DeliveryPersonDetailsAPIEntity
  { name :: Text,
    primaryExophone :: Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- Stop type
-- Remove Eq from deriving clause due to missing Eq instance for DLoc.LocationAPIEntity
data Stop = Stop
  { location :: DLoc.LocationAPIEntity,
    stopInfo :: Maybe DSI.StopInformation
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BookingType = CURRENT | ADVANCED
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- DriverRideRes type (partial, add all fields as in Ride.hs)
data DriverRideRes = DriverRideRes
  { id :: Id DRide.Ride,
    shortRideId :: ShortId DRide.Ride,
    status :: DRide.RideStatus,
    fromLocation :: DLoc.LocationAPIEntity,
    toLocation :: Maybe DLoc.LocationAPIEntity,
    stops :: [Stop],
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.VehicleVariant,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    billingCategory :: BillingCategory,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    estimatedBaseFare :: Money,
    computedFareWithCurrency :: Maybe PriceAPIEntity,
    estimatedBaseFareWithCurrency :: PriceAPIEntity,
    estimatedDistance :: Maybe Meters,
    estimatedDistanceWithUnit :: Maybe Distance,
    driverSelectedFare :: Money,
    driverSelectedFareWithCurrency :: PriceAPIEntity,
    actualRideDistance :: HighPrecMeters,
    actualRideDistanceWithUnit :: Distance,
    rideRating :: Maybe Int,
    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    specialLocationTag :: Maybe Text,
    actualDuration :: Maybe Seconds,
    estimatedDuration :: Maybe Seconds,
    chargeableDistance :: Maybe Meters,
    chargeableDistanceWithUnit :: Maybe Distance,
    exoPhone :: Text,
    bapName :: Maybe Text,
    bapLogo :: Maybe BaseUrl,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    disabilityTag :: Maybe Text,
    coinsRewardedOnGoldTierRide :: Maybe Int,
    requestedVehicleVariant :: DVeh.VehicleVariant,
    isOdometerReadingsRequired :: Bool,
    vehicleServiceTier :: DVST.ServiceTierType,
    vehicleServiceTierName :: Text,
    isVehicleAirConditioned :: Maybe Bool,
    vehicleCapacity :: Maybe Int,
    driverGoHomeRequestId :: Maybe (Id DDGR.DriverGoHomeRequest),
    payerVpa :: Maybe Text,
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    customerCancellationDues :: HighPrecMoney,
    estimatedTollCharges :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    tollConfidence :: Maybe Confidence,
    customerCancellationDuesWithCurrency :: PriceAPIEntity,
    estimatedTollChargesWithCurrency :: Maybe PriceAPIEntity,
    parkingChargeWithCurrency :: Maybe PriceAPIEntity,
    tollChargesWithCurrency :: Maybe PriceAPIEntity,
    isFreeRide :: Maybe Bool,
    stopLocationId :: Maybe (Id DLoc.Location),
    tripCategory :: DTC.TripCategory,
    nextStopLocation :: Maybe DLoc.Location,
    lastStopLocation :: Maybe DLoc.Location,
    startOdometerReading :: Maybe DRide.OdometerReading,
    endOdometerReading :: Maybe DRide.OdometerReading,
    returnTime :: Maybe UTCTime,
    vehicleAge :: Maybe Months,
    roundTrip :: Bool,
    tripScheduledAt :: UTCTime,
    isValueAddNP :: Bool,
    bookingType :: BookingType,
    enableFrequentLocationUpdates :: Maybe Bool,
    fleetOwnerId :: Maybe Text,
    enableOtpLessRide :: Bool,
    cancellationSource :: Maybe DBCR.CancellationSource,
    tipAmount :: Maybe PriceAPIEntity,
    penalityCharge :: Maybe PriceAPIEntity,
    senderDetails :: Maybe DeliveryPersonDetailsAPIEntity,
    receiverDetails :: Maybe DeliveryPersonDetailsAPIEntity,
    extraFareMitigationFlag :: Maybe Bool,
    parcelType :: Maybe DParcel.ParcelType,
    parcelQuantity :: Maybe Int,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    isPetRide :: Bool,
    riderMobileNumber :: Maybe Text,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    paymentMode :: Maybe DMPM.PaymentMode,
    commissionCharges :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

mkDriverRideRes ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  Maybe DExophone.Exophone ->
  (DRide.Ride, DRB.Booking) ->
  Maybe DSM.BapMetadata ->
  Maybe (Id DDGR.DriverGoHomeRequest) ->
  Maybe DI.DriverInformation ->
  Bool ->
  [DSI.StopInformation] ->
  Maybe Text ->
  m DriverRideRes
mkDriverRideRes rideDetails driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId driverInfo isValueAddNP stopsInfo mbRiderMobileNumber = do
  let fareParams = booking.fareParams
      estimatedBaseFare = fareSum (fareParams{driverSelectedFare = Nothing}) Nothing -- it should not be part of estimatedBaseFare
  let initial = "" :: Text
  (nextStopLocation, lastStopLocation) <- case booking.tripCategory of
    DTC.Rental _ -> calculateLocations booking.id booking.stopLocationId
    _ -> return (Nothing, Nothing)
  cancellationReason <- if ride.status == DRide.CANCELLED then runInReplica (QBCR.findByRideId (Just ride.id)) else pure Nothing

  return $
    DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = DLocUI.makeLocationAPIEntity booking.fromLocation,
        toLocation = DLocUI.makeLocationAPIEntity <$> booking.toLocation,
        stops = map (makeStop stopsInfo) booking.stops,
        driverName = rideDetails.driverName,
        driverNumber,
        vehicleNumber = rideDetails.vehicleNumber,
        vehicleColor = fromMaybe initial rideDetails.vehicleColor,
        vehicleVariant = fromMaybe DVeh.SEDAN rideDetails.vehicleVariant,
        vehicleModel = fromMaybe initial rideDetails.vehicleModel,
        computedFare = roundToIntegral <$> ride.fare,
        computedFareWithCurrency = flip PriceAPIEntity ride.currency <$> ride.fare,
        estimatedDuration = booking.estimatedDuration,
        actualDuration = roundToIntegral <$> (diffUTCTime <$> ride.tripEndTime <*> ride.tripStartTime),
        estimatedBaseFare = roundToIntegral estimatedBaseFare,
        estimatedBaseFareWithCurrency = PriceAPIEntity estimatedBaseFare ride.currency,
        estimatedDistance = booking.estimatedDistance,
        estimatedDistanceWithUnit = convertMetersToDistance booking.distanceUnit <$> booking.estimatedDistance,
        driverSelectedFare = roundToIntegral $ fromMaybe 0.0 fareParams.driverSelectedFare,
        driverSelectedFareWithCurrency = flip PriceAPIEntity fareParams.currency $ fromMaybe 0.0 fareParams.driverSelectedFare,
        actualRideDistance = ride.traveledDistance,
        actualRideDistanceWithUnit = convertHighPrecMetersToDistance ride.distanceUnit ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt,
        riderName = booking.riderName,
        pickupDropOutsideOfThreshold = ride.pickupDropOutsideOfThreshold,
        tripStartTime = ride.tripStartTime,
        tripEndTime = ride.tripEndTime,
        specialLocationTag = booking.specialLocationTag,
        rideRating = rideRating <&> (.ratingValue),
        chargeableDistance = ride.chargeableDistance,
        chargeableDistanceWithUnit = convertMetersToDistance ride.distanceUnit <$> ride.chargeableDistance,
        exoPhone = maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExophone,
        customerExtraFee = roundToIntegral <$> fareParams.customerExtraFee,
        customerExtraFeeWithCurrency = flip PriceAPIEntity fareParams.currency <$> fareParams.customerExtraFee,
        bapName = bapMetadata <&> (.name),
        bapLogo = bapMetadata >>= (.logoUrl),
        disabilityTag = booking.disabilityTag,
        coinsRewardedOnGoldTierRide = booking.coinsRewardedOnGoldTierRide,
        requestedVehicleVariant = DVeh.castServiceTierToVariant booking.vehicleServiceTier,
        isOdometerReadingsRequired = DTC.isOdometerReadingsRequired booking.tripCategory,
        vehicleServiceTier = booking.vehicleServiceTier,
        vehicleServiceTierName = booking.vehicleServiceTierName,
        vehicleCapacity = booking.vehicleServiceTierSeatingCapacity,
        isVehicleAirConditioned = booking.isAirConditioned,
        driverGoHomeRequestId = goHomeReqId,
        payerVpa = driverInfo >>= (.payerVpa),
        autoPayStatus = driverInfo >>= (.autoPayStatus),
        isFreeRide = ride.isFreeRide,
        customerCancellationDues = fromMaybe 0 fareParams.customerCancellationDues,
        estimatedTollCharges = fareParams.tollCharges,
        parkingCharge = fareParams.parkingCharge,
        tollCharges = ride.tollCharges,
        tollConfidence = ride.tollConfidence,
        customerCancellationDuesWithCurrency = flip PriceAPIEntity fareParams.currency $ fromMaybe 0 fareParams.customerCancellationDues,
        estimatedTollChargesWithCurrency = flip PriceAPIEntity fareParams.currency <$> fareParams.tollCharges,
        parkingChargeWithCurrency = flip PriceAPIEntity fareParams.currency <$> fareParams.parkingCharge,
        tollChargesWithCurrency = flip PriceAPIEntity ride.currency <$> ride.tollCharges,
        startOdometerReading = ride.startOdometerReading,
        endOdometerReading = ride.endOdometerReading,
        stopLocationId = booking.stopLocationId,
        tripCategory = booking.tripCategory,
        returnTime = booking.returnTime,
        vehicleAge = rideDetails.vehicleAge,
        roundTrip = fromMaybe False booking.roundTrip,
        nextStopLocation = nextStopLocation,
        lastStopLocation = lastStopLocation,
        tripScheduledAt = booking.startTime,
        bookingType = if ride.status == DRide.NEW && ride.isAdvanceBooking && maybe False (.hasAdvanceBooking) driverInfo then ADVANCED else CURRENT,
        isValueAddNP,
        enableFrequentLocationUpdates = ride.enableFrequentLocationUpdates,
        fleetOwnerId = rideDetails.fleetOwnerId,
        enableOtpLessRide = fromMaybe False ride.enableOtpLessRide,
        cancellationSource = fmap (\cr -> cr.source) cancellationReason,
        tipAmount = flip PriceAPIEntity ride.currency <$> ride.tipAmount,
        penalityCharge = flip PriceAPIEntity ride.currency <$> ride.cancellationFeeIfCancelled,
        senderDetails = booking.senderDetails <&> (\sd -> DeliveryPersonDetailsAPIEntity (sd.name) sd.primaryExophone),
        receiverDetails = booking.receiverDetails <&> (\rd -> DeliveryPersonDetailsAPIEntity (rd.name) rd.primaryExophone),
        extraFareMitigationFlag = driverInfo >>= (.extraFareMitigationFlag),
        parcelType = booking.parcelType,
        parcelQuantity = booking.parcelQuantity,
        isInsured = Just $ ride.isInsured,
        insuredAmount = ride.insuredAmount,
        isPetRide = booking.isPetRide,
        riderMobileNumber = mbRiderMobileNumber,
        billingCategory = booking.billingCategory,
        paymentInstrument = booking.paymentInstrument,
        paymentMode = booking.paymentMode,
        commissionCharges = ride.commission
      }

-- calculateLocations moved from UI.Ride
makeStop :: [DSI.StopInformation] -> DLoc.Location -> Stop
makeStop stopsInfo stop =
  let stopInfo = find (\s -> s.stopLocId == stop.id) stopsInfo
   in Stop (DLocUI.makeLocationAPIEntity stop) stopInfo

calculateLocations ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DRB.Booking ->
  Maybe (Id DLoc.Location) ->
  m (Maybe DLoc.Location, Maybe DLoc.Location)
calculateLocations bookingId stopLocationId = do
  maxOrder <- QLM.maxOrderByEntity (bookingId.getId)
  case stopLocationId of
    Nothing -> do
      lastLoc <- if maxOrder == 0 then pure Nothing else mkLocationFromLocationMapping bookingId maxOrder
      return (Nothing, lastLoc)
    Just nextStopId -> do
      nextLoc <- QLoc.findById nextStopId
      lastLoc <- mkLocationFromLocationMapping bookingId (maxOrder - 1)
      return (nextLoc, lastLoc)

mkLocationFromLocationMapping ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DRB.Booking ->
  Int ->
  m (Maybe DLoc.Location)
mkLocationFromLocationMapping bookingId order = do
  locMap <- (^? _head) <$> QLM.findByEntityIdOrderAndVersion (bookingId.getId) order QLM.latestTag
  case locMap of
    Nothing -> pure Nothing
    Just locMap_ -> QLoc.findById locMap_.locationId
