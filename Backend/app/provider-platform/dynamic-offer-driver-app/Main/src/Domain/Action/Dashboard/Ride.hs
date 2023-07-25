{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.Dashboard.Ride
  ( rideList,
    rideInfo,
    rideSync,
    multipleRideSync,
    rideRoute,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Operators
import qualified Kernel.Storage.Clickhouse.Queries as CH
import qualified Kernel.Storage.Clickhouse.Types as CH
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SyncRide as SyncRide
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.DriverQuote as DQ
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error

---------------------------------------------------------------------
rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbReqShortRideId mbCustomerPhone mbDriverPhone mbFareDiff mbfrom mbto = do
  merchant <- findMerchantByShortId merchantShortId
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset = fromMaybe 0 mbOffset
  let mbShortRideId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbReqShortRideId
  mbCustomerPhoneDBHash <- getDbHash `traverse` mbCustomerPhone
  mbDriverPhoneDBHash <- getDbHash `traverse` mbDriverPhone
  now <- getCurrentTime
  rideItems <- runInReplica $ QRide.findAllRideItems merchant.id limit offset mbBookingStatus mbShortRideId mbCustomerPhoneDBHash mbDriverPhoneDBHash mbFareDiff now mbfrom mbto
  rideListItems <- traverse buildRideListItem rideItems
  let count = length rideListItems
  -- should we consider filters in totalCount, e.g. count all canceled rides?
  -- totalCount <- runInReplica $ QRide.countRides merchant.id
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.RideListRes {totalItems = count, summary, rides = rideListItems}
  where
    maxLimit = 20
    defaultLimit = 10

buildRideListItem :: EncFlow m r => QRide.RideItem -> m Common.RideListItem
buildRideListItem QRide.RideItem {..} = do
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  pure
    Common.RideListItem
      { rideId = cast @DRide.Ride @Common.Ride rideDetails.id,
        rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) rideShortId,
        customerName,
        customerPhoneNo,
        driverName = rideDetails.driverName,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        fareDiff,
        bookingStatus,
        rideCreatedAt = rideCreatedAt
      }

---------------------------------------------------------------------------------------------------

getActualRoute :: MonadFlow m => Common.DriverEdaKafka -> m Common.ActualRoute
getActualRoute Common.DriverEdaKafka {..} =
  case (lat, lon, ts, acc, rideStatus) of
    (Just lat_, Just lon_, ts_, acc_, rideStatus_) -> do
      lat' <- readMaybe lat_ & fromMaybeM (InvalidRequest "Couldn't find driver's location.")
      lon' <- readMaybe lon_ & fromMaybeM (InvalidRequest "Couldn't find driver's location.")
      let acc' = readMaybe $ fromMaybe "" acc_
      let rideStatus' = readMaybe $ fromMaybe "" rideStatus_
      logInfo $ "Driver's hearbeat's timestamp received from clickhouse " <> show ts_ -- can be removed after running once in prod
      ts' <- Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" ts_ & fromMaybeM (InvalidRequest "Couldn't find driver's timestamp.")
      pure $ Common.ActualRoute lat' lon' ts' acc' rideStatus'
    _ -> throwError $ InvalidRequest "Couldn't find driver's location."

rideRoute :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideRouteRes
rideRoute merchantShortId reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist rideId.getId)
  let rideQId = T.unpack rideId.getId
      driverQId = T.unpack ride.driverId.getId
  let rQFst = fetchDate $ show ride.createdAt
  let rQLst = fetchDate $ show (addUTCTime (intToNominalDiffTime 86400) ride.createdAt)
  ckhTbl <- CH.findAll (Proxy @Common.DriverEdaKafka) ((("partition_date" =.= rQFst) |.| ("partition_date" =.= rQLst)) &.& ("driver_id" =.= driverQId) &.& ("rid" =.= rideQId)) Nothing Nothing (Just $ CH.Asc "ts")
  actualRoute <- case ckhTbl of
    Left err -> do
      logError $ "Clickhouse error: " <> show err
      pure []
    Right y -> mapM getActualRoute y
  when (null actualRoute) $ throwError $ InvalidRequest "No route found for this ride."
  pure
    Common.RideRouteRes
      { actualRoute
      }
  where
    fetchDate dateTime = T.unpack $ T.take 10 dateTime

---------------------------------------------------------------------
rideInfo :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideInfoRes
rideInfo merchantShortId reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideDetails <- runInReplica $ QRideDetails.findById rideId >>= fromMaybeM (RideNotFound rideId.getId) -- FIXME RideDetailsNotFound
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)
  mQuote <- runInReplica $ DQ.findById (Id booking.quoteId)
  let driverId = ride.driverId

  -- merchant access checking
  unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist rideId.getId)

  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound rideId.getId)
  mDriverLocation <- QDrLoc.findByIdInReplica driverId

  mbBCReason <-
    if ride.status == DRide.CANCELLED
      then runInReplica $ QBCReason.findByRideId rideId -- it can be Nothing if cancelled by user
      else pure Nothing
  driverInitiatedCallCount <- runInReplica $ QCallStatus.countCallsByRideId rideId
  let cancellationReason =
        (coerce @DCReason.CancellationReasonCode @Common.CancellationReasonCode <$>) . join $ mbBCReason <&> (.reasonCode)
  let cancelledBy = castCancellationSource <$> (mbBCReason <&> (.source))
  let cancelledTime = case ride.status of
        DRide.CANCELLED -> Just ride.updatedAt
        _ -> Nothing
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  now <- getCurrentTime
  pure
    Common.RideInfoRes
      { rideId = cast @DRide.Ride @Common.Ride ride.id,
        customerName = booking.riderName,
        customerPhoneNo,
        rideOtp = ride.otp,
        customerPickupLocation = mkLocationAPIEntity booking.fromLocation,
        customerDropLocation = Just $ mkLocationAPIEntity booking.toLocation,
        actualDropLocation = ride.tripEndPos,
        driverId = cast @DP.Person @Common.Driver driverId,
        driverName = rideDetails.driverName,
        pickupDropOutsideOfThreshold = ride.pickupDropOutsideOfThreshold,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        driverStartLocation = ride.tripStartPos,
        driverCurrentLocation = getCoordinates <$> mDriverLocation,
        rideBookingTime = booking.createdAt,
        estimatedDriverArrivalTime = (\quote -> realToFrac quote.durationToPickup `addUTCTime` ride.createdAt) <$> mQuote,
        actualDriverArrivalTime = ride.driverArrivalTime,
        rideStartTime = ride.tripStartTime,
        rideEndTime = ride.tripEndTime,
        rideDistanceEstimated = Just booking.estimatedDistance,
        rideDistanceActual = roundToIntegral ride.traveledDistance,
        chargeableDistance = ride.chargeableDistance,
        maxEstimatedDistance = highPrecMetersToMeters <$> booking.maxEstimatedDistance,
        estimatedRideDuration = Just $ secondsToMinutes booking.estimatedDuration,
        estimatedFare = booking.estimatedFare,
        actualFare = ride.fare,
        driverOfferedFare = (.fareParams.driverSelectedFare) =<< mQuote,
        pickupDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just ride.createdAt),
        rideDuration = timeDiffInMinutes <$> ride.tripEndTime <*> ride.tripStartTime,
        bookingStatus = mkBookingStatus ride now,
        cancelledTime,
        cancelledBy,
        cancellationReason,
        driverInitiatedCallCount,
        bookingToRideStartDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just booking.createdAt),
        distanceCalculationFailed = ride.distanceCalculationFailed
      }

mkLocationAPIEntity :: DBLoc.BookingLocation -> Common.LocationAPIEntity
mkLocationAPIEntity DBLoc.BookingLocation {..} = do
  let DBLoc.LocationAddress {..} = address
  Common.LocationAPIEntity {..}

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator
  DBCReason.ByApplication -> Common.ByApplication

timeDiffInMinutes :: UTCTime -> UTCTime -> Minutes
timeDiffInMinutes t1 = secondsToMinutes . nominalDiffTimeToSeconds . diffUTCTime t1

-- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.tripStartTime = Nothing
mkBookingStatus :: DRide.Ride -> UTCTime -> Common.BookingStatus
mkBookingStatus ride now = do
  let sixHours = secondsToNominalDiffTime $ Seconds 21600
  let ongoing6HrsCond = maybe True (\tripStartTime -> diffUTCTime now tripStartTime >= sixHours) ride.tripStartTime
  let upcoming6HrsCond = diffUTCTime now ride.createdAt >= sixHours
  case ride.status of
    DRide.NEW | not upcoming6HrsCond -> Common.UPCOMING
    DRide.NEW -> Common.UPCOMING_6HRS
    DRide.INPROGRESS | not ongoing6HrsCond -> Common.ONGOING
    DRide.INPROGRESS -> Common.ONGOING_6HRS
    DRide.COMPLETED -> Common.COMPLETED
    DRide.CANCELLED -> Common.CANCELLED

---------------------------------------------------------------------
rideSync :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideSyncRes
rideSync merchantShortId reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)

  -- merchant access checking
  unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist rideId.getId)

  logTagInfo "dashboard -> syncRide : " $ show rideId <> "; status: " <> show ride.status

  SyncRide.rideSync Nothing (Just ride) booking merchant

---------------------------------------------------------------------

multipleRideSync :: ShortId DM.Merchant -> Common.MultipleRideSyncReq -> Flow Common.MultipleRideSyncRes
multipleRideSync merchantShortId rideSyncReq = do
  merchant <- findMerchantByShortId merchantShortId
  let rideIds = map (cast @Common.Ride @DRide.Ride) rideSyncReq.rideIds
  ridesBookingsZip <- QRide.findAllRidesBookingsByRideId merchant.id rideIds

  -- merchant access checking
  for_ ridesBookingsZip $ \(ride, booking) -> do unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist ride.id.getId)
  logTagInfo "dashboard -> syncRide : " $ show rideIds <> "; status: " <> show (map fun ridesBookingsZip)
  rideDataResult <-
    mapM
      ( \(ride, booking) ->
          mapLeft show
            <$> ( try @_ @SomeException $
                    mkMultipleRideData ride.id <$> SyncRide.rideSync Nothing (Just ride) booking merchant
                )
      )
      ridesBookingsZip
  return
    Common.MultipleRideSyncRes
      { list = rideDataResult
      }
  where
    fun (ride, _) = ride.status

mkMultipleRideData :: Id DRide.Ride -> Common.RideSyncRes -> Common.MultipleRideData
mkMultipleRideData rideId Common.RideSyncRes {..} =
  Common.MultipleRideData
    { rideId = cast @DRide.Ride @Common.Ride rideId,
      ..
    }
