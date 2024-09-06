{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.Dashboard.Ride
  ( shareRideInfo,
    shareRideInfoByShortId,
    rideList,
    rideInfo,
    multipleRideCancel,
    MultipleRideCancelReq,
    rideSync,
    ticketRideList,
  )
where

import qualified Beckn.ACL.Common as Common
import Beckn.ACL.Status
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import Data.Coerce (coerce)
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Action.UI.EstimateBP as EstimateBP
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Booking as DTB
import qualified Domain.Types.BookingCancellationReason as DBCReason
import Domain.Types.CancellationReason
import Domain.Types.Common
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Location (Location (..))
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Sos as DSos
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (count, isNothing, on)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.CachedQueries.Merchant (findByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide

data BookingCancelledReq = BookingCancelledReq
  { bookingId :: Id DTB.Booking,
    cancellationReasonCode :: CancellationReasonCode,
    cancellationStage :: CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype MultipleRideCancelReq = MultipleRideCancelReq
  { multipleRideCancelInfo :: [BookingCancelledReq]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Common.HideSecrets MultipleRideCancelReq where
  hideSecrets = identity

---------------------------------------------------------------------

mkCommonRideStatus :: DRide.RideStatus -> Common.RideStatus
mkCommonRideStatus rs = case rs of
  DRide.UPCOMING -> Common.UPCOMING_RIDE
  DRide.NEW -> Common.NEW
  DRide.INPROGRESS -> Common.INPROGRESS
  DRide.COMPLETED -> Common.COMPLETED
  DRide.CANCELLED -> Common.CANCELLED

mkCommonBookingLocation :: Location -> Common.Location
mkCommonBookingLocation Location {..} =
  Common.Location
    { id = cast @Location @Common.Location id,
      address = mkAddressRes address,
      ..
    }

mkAddressRes :: LocationAddress -> Common.LocationAddress
mkAddressRes LocationAddress {..} = Common.LocationAddress {..}

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  Flow Common.ShareRideInfoRes
shareRideInfo merchantId rideId = do
  ride <- B.runInReplica $ QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  buildShareRideInfo merchantId ride

shareRideInfoByShortId :: ShortId DM.Merchant -> ShortId Common.Ride -> Flow Common.ShareRideInfoRes
shareRideInfoByShortId merchantId rideId = do
  let rideShortId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) rideId
  ride <- QRide.findRideByRideShortId rideShortId >>= fromMaybeM (InvalidRequest "Ride ShortId Not Found")
  buildShareRideInfo merchantId ride

buildShareRideInfo ::
  ShortId DM.Merchant ->
  DRide.Ride ->
  Flow Common.ShareRideInfoRes
buildShareRideInfo merchantId ride = do
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  merchant <- findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  unless (merchant.id == booking.merchantId) $ throwError (RideDoesNotExist ride.id.getId)
  case ride.status of
    DRide.COMPLETED -> throwError $ RideInvalidStatus "This ride is completed"
    DRide.CANCELLED -> throwError $ RideInvalidStatus "This ride is cancelled"
    _ -> pure ()
  person <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  let mbtoLocation = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ mkCommonBookingLocation locationDetail.toLocation
        DB.DriverOfferDetails driverOfferDetail -> Just $ mkCommonBookingLocation driverOfferDetail.toLocation
        DB.DeliveryDetails deliveryDetails -> Just $ mkCommonBookingLocation deliveryDetails.toLocation
        DB.InterCityDetails details -> Just $ mkCommonBookingLocation details.toLocation
        _ -> Nothing
  let mbDistance = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ locationDetail.distance
        DB.DriverOfferDetails driverOfferDetail -> Just $ driverOfferDetail.distance
        DB.OneWaySpecialZoneDetails oneWaySpecialZoneDetail -> Just $ oneWaySpecialZoneDetail.distance
        DB.InterCityDetails details -> Just details.distance
        DB.DeliveryDetails details -> Just details.distance
        _ -> Nothing
  let driverNumber =
        ( case booking.tripCategory of
            Just (Delivery _) -> Just ride.driverMobileNumber
            _ -> Nothing
        )
          <&> (\number -> if ride.status `elem` [DRide.NEW, DRide.INPROGRESS] then number else "xxxx")
  sosDetails <- CQSos.findByRideId ride.id
  let fareProductType = mkFareProductType booking.bookingDetails
  return $
    Common.ShareRideInfoRes
      { id = cast ride.id,
        bookingId = cast ride.bookingId,
        status = mkCommonRideStatus ride.status,
        driverName = ride.driverName,
        driverNumber = driverNumber,
        driverRating = ride.driverRating,
        vehicleNumber = ride.vehicleNumber,
        vehicleModel = ride.vehicleModel,
        vehicleColor = fromMaybe "NA" ride.vehicleColor, -- TODO::remove this default value
        trackingUrl = ride.trackingUrl,
        estimatedDistance = distanceToHighPrecMeters <$> mbDistance,
        estimatedDistanceWithUnit = mbDistance,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        userFirstName = person.firstName,
        userLastName = person.lastName,
        fromLocation = mkCommonBookingLocation booking.fromLocation,
        toLocation = mbtoLocation,
        sosStatus = castSosStatus . (.status) <$> sosDetails,
        vehicleVariant = ride.vehicleVariant,
        nextStopLocation = getStopFromBookingDetails booking.bookingDetails,
        rideScheduledAt = booking.startTime,
        fareProductType = fareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
        tripCategory = getTripCategory booking.tripCategory fareProductType
      }

getStopFromBookingDetails :: DTB.BookingDetails -> Maybe Common.Location
getStopFromBookingDetails bookingDetails = case bookingDetails of
  DB.RentalDetails rentalDetails -> mkCommonBookingLocation <$> rentalDetails.stopLocation
  _ -> Nothing

castSosStatus :: DSos.SosStatus -> Common.SosStatus
castSosStatus = \case
  DSos.Pending -> Common.Pending
  DSos.Resolved -> Common.Resolved
  DSos.NotResolved -> Common.NotResolved
  DSos.MockPending -> Common.MockPending
  DSos.MockResolved -> Common.MockResolved

---------------------------------------------------------------------

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbReqShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  let limit_ = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset_ = fromMaybe 0 mbOffset
  let mbShortRideId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbReqShortRideId
  mbCustomerPhoneDBHash <- getDbHash `traverse` mbCustomerPhone
  now <- getCurrentTime
  when (isNothing mbBookingStatus && isNothing mbShortRideId && isNothing mbCustomerPhoneDBHash && isNothing mbDriverPhone) $ throwError $ InvalidRequest "Atleast one of the filter is required"
  case (mbFrom, mbTo) of
    (Just from', Just to') -> when (from' > to') $ throwError $ InvalidRequest "from date should be less than to date"
    _ -> pure ()
  rideItems <- B.runInReplica $ QRide.findAllRideItems merchant.id limit_ offset_ mbBookingStatus mbShortRideId mbCustomerPhoneDBHash mbDriverPhone mbFrom mbTo now
  logDebug (T.pack "rideItems: " <> T.pack (show $ length rideItems))
  rideListItems <- traverse buildRideListItem rideItems
  let count = length rideListItems
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.RideListRes {totalItems = count, summary, rides = rideListItems}
  where
    maxLimit = 20
    defaultLimit = 10

buildRideListItem :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => QRide.RideItem -> m Common.RideListItem
buildRideListItem QRide.RideItem {..} = do
  customerPhoneNo <- mapM decrypt person.mobileNumber
  let fareProductType = mkFareProductType bookingDetails
  pure
    Common.RideListItem
      { rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) ride.shortId,
        rideCreatedAt = ride.createdAt,
        rideId = cast @DRide.Ride @Common.Ride ride.id,
        customerName = person.firstName,
        customerPhoneNo,
        driverName = ride.driverName,
        driverPhoneNo = ride.driverMobileNumber,
        vehicleNo = ride.vehicleNumber,
        endOtp = ride.endOtp,
        nextStopLocation = getStopFromBookingDetails bookingDetails,
        fareProductType = fareProductType,
        tripCategory = getTripCategory tripCategory fareProductType,
        ..
      }

ticketRideList :: ShortId DM.Merchant -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.TicketRideListRes
ticketRideList merchantShortId mbRideShortId countryCode mbPhoneNumber _ = do
  merchant <- findMerchantByShortId merchantShortId
  let totalRides = 5
  let mbShortId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbRideShortId
  let code = fromMaybe "+91" countryCode
  personId <- case (mbPhoneNumber, mbShortId) of
    (Just number, _) -> do
      no <- getDbHash number
      person <- B.runInReplica $ QP.findByMobileNumberAndMerchantId code no merchant.id >>= fromMaybeM (PersonWithPhoneNotFound number)
      return person.id
    (Nothing, Just shortId) -> do
      ride <- QRide.findRideByRideShortId shortId >>= fromMaybeM (InvalidRequest "Ride ShortId Not Found")
      booking <- QRB.findByIdAndMerchantId ride.bookingId merchant.id >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      return booking.riderId
    (Nothing, Nothing) -> throwError $ InvalidRequest "Ride Short Id or Phone Number Not Received"
  mbNumberHash <- getDbHash `traverse` mbPhoneNumber
  now <- getCurrentTime
  rideItems <- B.runInReplica $ QRide.findAllRideItems merchant.id totalRides 0 Nothing mbShortId mbNumberHash Nothing Nothing Nothing now
  let rideItem = DL.sortBy (\a b -> compare b.ride.createdAt a.ride.createdAt) rideItems
  let lastNRides = map (.ride) rideItem
      lastNBookingStatus = map (.bookingStatus) rideItem
  ridesDetail <- mapM (\ride -> rideInfo merchant.id (cast ride.id)) lastNRides
  let rdList = map (makeRequiredRideDetail personId) (zip3 lastNRides ridesDetail lastNBookingStatus)
  return Common.TicketRideListRes {rides = rdList}
  where
    makeRequiredRideDetail :: Id DP.Person -> (DRide.Ride, Common.RideInfoRes, Common.BookingStatus) -> Common.RideInfo
    makeRequiredRideDetail personId (ride, detail, bookingStatus) =
      Common.RideInfo
        { rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) ride.shortId,
          customerName = detail.customerName,
          customerPhoneNo = detail.customerPhoneNo,
          driverName = detail.driverName,
          driverPhoneNo = detail.driverPhoneNo,
          vehicleNo = detail.vehicleNo,
          status = bookingStatus,
          rideCreatedAt = ride.createdAt,
          pickupLocationLat = Just detail.customerPickupLocation.lat,
          pickupLocationLon = Just detail.customerPickupLocation.lon,
          pickupLocationStreet = detail.customerPickupLocation.address.street,
          pickupLocationCity = detail.customerPickupLocation.address.city,
          pickupLocationState = detail.customerPickupLocation.address.state,
          pickupLocationCountry = detail.customerPickupLocation.address.country,
          pickupLocationBuilding = detail.customerPickupLocation.address.building,
          pickupLocationAreaCode = detail.customerPickupLocation.address.areaCode,
          pickupLocationArea = detail.customerPickupLocation.address.area,
          dropLocationLat = (.lat) <$> detail.customerDropLocation,
          dropLocationLon = (.lon) <$> detail.customerDropLocation,
          dropLocationStreet = (.address.street) =<< detail.customerDropLocation,
          dropLocationCity = (.address.city) =<< detail.customerDropLocation,
          dropLocationState = (.address.state) =<< detail.customerDropLocation,
          dropLocationCountry = (.address.country) =<< detail.customerDropLocation,
          dropLocationBuilding = (.address.building) =<< detail.customerDropLocation,
          dropLocationAreaCode = (.address.areaCode) =<< detail.customerDropLocation,
          dropLocationArea = (.address.area) =<< detail.customerDropLocation,
          fare = detail.actualFare,
          fareWithCurrency = detail.actualFareWithCurrency,
          personId = cast personId,
          classification = Ticket.CUSTOMER,
          nextStopLocation = detail.nextStopLocation,
          rideScheduledAt = detail.rideScheduledAt,
          fareProductType = detail.fareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
          tripCategory = detail.tripCategory,
          endOtp = ride.endOtp
        }

rideInfo :: Id DM.Merchant -> Id Common.Ride -> Flow Common.RideInfoRes
rideInfo merchantId reqRideId = do
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let estimatedDuration :: Maybe Seconds = (booking.estimatedDuration)
  estBreakup <- case booking.quoteId of
    Just quoteId -> do
      quote <- B.runInReplica $ QQuote.findById quoteId
      case quote of
        Just q -> do
          estimateBreakup <- EstimateBP.getEstimateBreakupFromQuote q
          return $ Just estimateBreakup
        Nothing -> return Nothing
    Nothing -> return Nothing
  fareBreakup <- B.runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType rideId.getId DFareBreakup.RIDE
  unless (merchantId == booking.merchantId) $ throwError (RideDoesNotExist rideId.getId)
  person <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  mbBCReason <-
    if ride.status == DRide.CANCELLED
      then B.runInReplica $ QBCReason.findByRideBookingId booking.id
      else -- QBCReason.findByRideBookingId booking.id
        pure Nothing
  let cancelledTime = case ride.status of
        DRide.CANCELLED -> Just ride.updatedAt
        _ -> Nothing
  let mbtoLocation = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ mkCommonBookingLocation locationDetail.toLocation
        DB.DriverOfferDetails driverOfferDetail -> Just $ mkCommonBookingLocation driverOfferDetail.toLocation
        _ -> Nothing
  let mbDistance = case booking.bookingDetails of
        DB.OneWayDetails locationDetail -> Just $ locationDetail.distance
        DB.DriverOfferDetails driverOfferDetail -> Just $ driverOfferDetail.distance
        DB.OneWaySpecialZoneDetails oneWaySpecialZoneDetail -> Just $ oneWaySpecialZoneDetail.distance
        DB.InterCityDetails details -> Just details.distance
        _ -> Nothing
  let cancelledBy = castCancellationSource <$> (mbBCReason <&> (.source))
  unencryptedMobileNumber <- mapM decrypt person.mobileNumber
  let fareProductType = mkFareProductType booking.bookingDetails
  pure
    Common.RideInfoRes
      { rideId = reqRideId,
        bookingId = cast ride.bookingId,
        rideStatus = mkCommonRideStatus ride.status,
        customerName = person.firstName,
        customerPhoneNo = unencryptedMobileNumber,
        rideOtp = ride.otp,
        customerPickupLocation = mkCommonBookingLocation booking.fromLocation,
        customerDropLocation = mbtoLocation,
        driverName = ride.driverName,
        driverPhoneNo = Just ride.driverMobileNumber,
        driverRegisteredAt = ride.driverRegisteredAt,
        vehicleNo = ride.vehicleNumber,
        vehicleModel = ride.vehicleModel,
        vehicleVariant = ride.vehicleVariant,
        vehicleServiceTierName = show <$> ride.vehicleServiceTierType,
        rideBookingTime = booking.createdAt,
        actualDriverArrivalTime = ride.driverArrivalTime,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        rideDistanceEstimated = distanceToHighPrecMeters <$> mbDistance,
        rideDistanceEstimatedWithUnit = mbDistance,
        rideDistanceActual = distanceToHighPrecMeters <$> ride.traveledDistance,
        rideDistanceActualWithUnit = ride.traveledDistance,
        chargeableDistance = distanceToHighPrecMeters <$> ride.chargeableDistance,
        chargeableDistanceWithUnit = ride.chargeableDistance,
        estimatedFare = booking.estimatedFare.amountInt,
        actualFare = ride.fare <&> (.amountInt),
        estimatedFareWithCurrency = mkPriceAPIEntity booking.estimatedFare,
        actualFareWithCurrency = mkPriceAPIEntity <$> ride.fare,
        estimatedRideDuration = estimatedDuration,
        rideDuration = timeDiffInSeconds <$> ride.rideEndTime <*> ride.rideStartTime,
        cancelledTime = cancelledTime,
        cancelledBy = cancelledBy,
        nextStopLocation = getStopFromBookingDetails booking.bookingDetails,
        rideScheduledAt = booking.startTime,
        fareProductType = fareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
        tripCategory = getTripCategory booking.tripCategory fareProductType,
        endOtp = ride.endOtp,
        estimateFareBP = map EstimateBP.transformEstimate' <$> estBreakup,
        merchantOperatingCityId = getId <$> ride.merchantOperatingCityId,
        fareBreakup = transformFareBreakup <$> fareBreakup,
        estimatedDistance = distanceToHighPrecMeters <$> booking.estimatedDistance,
        computedPrice = ride.totalFare <&> (.amount),
        rideCreatedAt = ride.createdAt
      }

transformFareBreakup :: DFareBreakup.FareBreakup -> Common.FareBreakup
transformFareBreakup DFareBreakup.FareBreakup {..} = do
  Common.FareBreakup
    { entityType = mkEntityType entityType,
      ..
    }

mkEntityType :: DFareBreakup.FareBreakupEntityType -> Common.FareBreakupEntityType
mkEntityType = \case
  DFareBreakup.BOOKING_UPDATE_REQUEST -> Common.BOOKING_UPDATE_REQUEST
  DFareBreakup.BOOKING -> Common.BOOKING
  DFareBreakup.RIDE -> Common.RIDE
  DFareBreakup.INITIAL_BOOKING -> Common.INITIAL_BOOKING

mkFareProductType :: DTB.BookingDetails -> FareProductType
mkFareProductType bookingDetails = case bookingDetails of
  DTB.OneWayDetails _ -> ONE_WAY
  DTB.RentalDetails _ -> RENTAL
  DTB.DriverOfferDetails _ -> DRIVER_OFFER
  DTB.OneWaySpecialZoneDetails _ -> ONE_WAY_SPECIAL_ZONE
  DTB.InterCityDetails _ -> INTER_CITY
  DTB.AmbulanceDetails _ -> AMBULANCE
  DTB.DeliveryDetails _ -> DRIVER_OFFER --Fix: Check later if this is correct
  DTB.OneWayScheduledDetails _ -> ONE_WAY_SPECIAL_ZONE

timeDiffInSeconds :: UTCTime -> UTCTime -> Seconds
timeDiffInSeconds t1 = nominalDiffTimeToSeconds . diffUTCTime t1

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator
  DBCReason.ByApplication -> Common.ByApplication

bookingCancel ::
  (CacheFlow m r, EsqDBFlow m r) =>
  BookingCancelledReq ->
  m ()
bookingCancel BookingCancelledReq {..} = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingId.getId)
  unless (isBookingCancellable booking) $
    throwError (BookingInvalidStatus (show booking.status))
  mbRide <- QRide.findActiveByRBId booking.id
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCReason.ByMerchant)
  bookingCancellationReason <- buildBookingCancellationReason booking (mbRide <&> (.id))
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  _ <- QRB.updateStatus booking.id DTB.CANCELLED
  _ <- QBPL.makeAllInactiveByBookingId booking.id
  _ <- whenJust mbRide $ \ride -> void $ QRide.updateStatus ride.id DRide.CANCELLED
  void $ QBCReason.upsert bookingCancellationReason
  where
    isBookingCancellable booking =
      booking.status `elem` [DTB.NEW, DTB.CONFIRMED, DTB.AWAITING_REASSIGNMENT, DTB.TRIP_ASSIGNED]

buildBookingCancellationReason ::
  (MonadFlow m) =>
  DTB.Booking ->
  Maybe (Id DRide.Ride) ->
  m DBCReason.BookingCancellationReason
buildBookingCancellationReason booking mbRideId = do
  now <- getCurrentTime
  return $
    DBCReason.BookingCancellationReason
      { bookingId = booking.id,
        rideId = mbRideId,
        merchantId = Just booking.merchantId,
        distanceUnit = booking.distanceUnit,
        source = DBCReason.ByMerchant,
        reasonCode = Just $ CancellationReasonCode "BOOKING_NEW_STATUS_MORE_THAN_6HRS",
        reasonStage = Nothing,
        additionalInfo = Nothing,
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        createdAt = now,
        updatedAt = now
      }

multipleRideCancel ::
  MultipleRideCancelReq ->
  Flow APISuccess
multipleRideCancel req = do
  mapM_ bookingCancel req.multipleRideCancelInfo
  pure Success

---------------------------------------------------------------------
rideSync ::
  DM.Merchant ->
  Id Common.Ride ->
  Flow APISuccess
rideSync merchant reqRideId = do
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  unless (merchant.id == booking.merchantId) $
    throwError (RideDoesNotExist rideId.getId)
  city <- case ride.merchantOperatingCityId of
    Nothing -> pure merchant.defaultCity
    Just merchantOperatingCityId -> CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  let dStatusReq = DStatusReq {booking, merchant, city}
  becknStatusReq <- buildStatusReqV2 dStatusReq
  messageId <- Utils.getMessageId becknStatusReq.statusReqContext
  Hedis.setExp (Common.makeContextMessageIdStatusSyncKey messageId) True 3600
  void $ withShortRetry $ CallBPP.callStatusV2 booking.providerUrl becknStatusReq booking.merchantId
  pure Success
