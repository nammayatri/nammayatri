{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride
  ( GetRideStatusResp (..),
    PickupStage (..),
    EditLocation,
    EditLocationReq (..),
    EditLocationResp (..),
    getRideStatus,
    editLocation,
    getDriverPhoto,
    getDeliveryImage,
  )
where

import AWS.S3 as S3
import qualified Beckn.ACL.Update as ACL
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Ord
import qualified Data.Text as Text
import Domain.Action.UI.Location (makeLocationAPIEntity)
import qualified Domain.Action.UI.Person as UPerson
import qualified Domain.Types.Booking as DB
import Domain.Types.Booking.API (buildRideAPIEntity)
import qualified Domain.Types.BookingUpdateRequest as DBUR
import Domain.Types.Extra.Ride (EditLocation, RideAPIEntity (..))
import Domain.Types.Location (LocationAPIEntity)
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (HasField)
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.LocationMapping as SLM
import qualified SharedLogic.Person as SLP
import qualified SharedLogic.Serviceability as Serviceability
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.Ride as QRide
import Storage.Queries.SafetySettings as QSafety
import Tools.Error
import qualified Tools.Maps as MapSearch

data PickupStage = OnTheWay | Reached | Reaching
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data GetRideStatusResp = GetRideStatusResp
  { fromLocation :: LocationAPIEntity,
    toLocation :: Maybe LocationAPIEntity,
    ride :: RideAPIEntity,
    customer :: UPerson.PersonAPIEntity,
    driverPosition :: Maybe MapSearch.LatLong
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data EditLocationReq = EditLocationReq
  { origin :: Maybe EditLocation,
    destination :: Maybe EditLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data EditLocationResp = EditLocationResp
  { bookingUpdateRequestId :: Maybe (Id DBUR.BookingUpdateRequest),
    result :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

getDriverPhoto :: Text -> Flow Text
getDriverPhoto filePath = S3.get $ Text.unpack filePath

getRideStatus ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetRideStatusResp
getRideStatus rideId personId = withLogTag ("personId-" <> personId.getId) do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  mbPos <-
    if ride.status == COMPLETED || ride.status == CANCELLED
      then return Nothing
      else Just <$> CallBPP.callGetDriverLocation ride.trackingUrl
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  rider <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
  let tag = customerDisability <&> (.tag)
  decRider <- decrypt rider
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just rider)
  isSafetyCenterDisabled <- SLP.checkSafetyCenterDisabled rider safetySettings
  ride' <- buildRideAPIEntity ride
  return $
    GetRideStatusResp
      { fromLocation = makeLocationAPIEntity booking.fromLocation,
        toLocation = case booking.bookingDetails of
          DB.OneWayDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.RentalDetails _ -> Nothing
          DB.OneWaySpecialZoneDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.InterCityDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.DriverOfferDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.AmbulanceDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.DeliveryDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.MeterRideDetails details -> makeLocationAPIEntity <$> details.toLocation,
        ride = ride',
        customer = UPerson.makePersonAPIEntity decRider tag isSafetyCenterDisabled safetySettings,
        driverPosition = mbPos <&> (.currPoint)
      }

editLocation ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv,
    MonadFlow m,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Id SRide.Ride ->
  (Id SPerson.Person, Id DM.Merchant) ->
  EditLocationReq ->
  m EditLocationResp
editLocation rideId (personId, merchantId) req = do
  when (isNothing req.origin && isNothing req.destination) do
    throwError PickupOrDropLocationNotFound
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let bookingId = ride.bookingId
  booking <- B.runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  when (not isValueAddNP) $ throwError (InvalidRequest "Edit location is not supported for non value add NP")
  case (req.origin, req.destination) of
    (Just pickup, _) -> do
      let attemptsLeft = fromMaybe merchant.numOfAllowedEditPickupLocationAttemptsThreshold ride.allowedEditPickupLocationAttempts
      when (attemptsLeft == 0) do
        throwError EditLocationAttemptsExhausted
      when (ride.status /= SRide.NEW) do
        throwError (InvalidRequest $ "Customer is not allowed to change pickup as the ride is not NEW for rideId: " <> ride.id.getId)
      pickupLocationMappings <- QLM.findAllByEntityIdAndOrder ride.id.getId 0
      {-
        Sorting down will sort mapping like this v-2, v-1, LATEST
      -}
      oldestMapping <- (listToMaybe $ sortBy (comparing (Down . (.version))) pickupLocationMappings) & fromMaybeM (InternalError $ "Latest mapping not found for rideId: " <> ride.id.getId)
      initialLocationForRide <- QL.findById oldestMapping.locationId >>= fromMaybeM (InternalError $ "Location not found for locationId:" <> oldestMapping.locationId.getId)
      let initialLatLong = Maps.LatLong {lat = initialLocationForRide.lat, lon = initialLocationForRide.lon}
          currentLatLong = pickup.gps
      let distance = CD.distanceBetweenInMeters initialLatLong currentLatLong
      when (distance > distanceToHighPrecMeters merchant.editPickupDistanceThreshold) do
        throwError EditPickupLocationNotServiceable

      res <- try @_ @SomeException (CallBPP.callGetDriverLocation ride.trackingUrl)
      case res of
        Right res' -> do
          let curDriverLocation = res'.currPoint
          let distanceOfDriverFromChangingPickup = CD.distanceBetweenInMeters curDriverLocation currentLatLong
          when (distanceOfDriverFromChangingPickup < distanceToHighPrecMeters merchant.driverDistanceThresholdFromPickup) do
            throwError $ DriverAboutToReachAtInitialPickup (show distanceOfDriverFromChangingPickup)
        Left err -> do
          logTagInfo "DriverLocationFetchFailed" $ show err

      startLocation <- buildLocation merchantId booking.merchantOperatingCityId pickup
      QL.create startLocation
      pickupMapForBooking <- SLM.buildPickUpLocationMapping startLocation.id bookingId.getId DLM.BOOKING (Just merchantId) ride.merchantOperatingCityId
      QLM.create pickupMapForBooking
      pickupMapForRide <- SLM.buildPickUpLocationMapping startLocation.id ride.id.getId DLM.RIDE (Just merchantId) ride.merchantOperatingCityId
      QLM.create pickupMapForRide
      pickupMapForSearchReq <- SLM.buildPickUpLocationMapping startLocation.id booking.transactionId DLM.SEARCH_REQUEST (Just merchantId) ride.merchantOperatingCityId
      QLM.create pickupMapForSearchReq
      let origin = Just $ startLocation{id = "0"}
      bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
      uuid <- generateGUID
      let dUpdateReq =
            ACL.UpdateBuildReq
              { bppBookingId,
                merchant,
                bppId = booking.providerId,
                bppUrl = booking.providerUrl,
                transactionId = booking.transactionId,
                messageId = uuid,
                city = merchant.defaultCity, -- TODO: Correct during interoperability
                details =
                  ACL.UEditLocationBuildReqDetails $
                    ACL.EditLocationBuildReqDetails
                      { bppRideId = ride.bppRideId,
                        origin,
                        status = ACL.CONFIRM_UPDATE,
                        destination = Nothing,
                        stops = Nothing
                      },
                ..
              }
      becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
      void . withShortRetry $ CallBPP.updateV2 booking.providerUrl becknUpdateReq
      QRB.updateIsBookingUpdated True booking.id
      QRide.updateEditPickupLocationAttempts ride.id (Just (attemptsLeft -1))
      pure $ EditLocationResp Nothing "Success"
    (_, Just destination) -> do
      let attemptsLeft = fromMaybe merchant.numOfAllowedEditLocationAttemptsThreshold ride.allowedEditLocationAttempts
      when (attemptsLeft == 0) do
        throwError EditLocationAttemptsExhausted
      when (ride.status == SRide.CANCELLED || ride.status == SRide.COMPLETED) do
        throwError (InvalidRequest $ "Customer is not allowed to change destination as the ride is in terminal state for rideId: " <> ride.id.getId)
      newDropLocation <- buildLocation merchantId booking.merchantOperatingCityId destination
      QL.create newDropLocation
      startLocMapping <- QLM.getLatestStartByEntityId booking.id.getId >>= fromMaybeM (InternalError $ "Latest start location mapping not found for bookingId: " <> booking.id.getId)
      oldDropLocMapping <- QLM.getLatestEndByEntityId booking.id.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingId: " <> booking.id.getId)
      bookingUpdateReq <- buildbookingUpdateRequest booking
      origin <- QL.findById startLocMapping.locationId >>= fromMaybeM (InternalError $ "Location not found for locationId:" <> startLocMapping.locationId.getId)
      let sourceLatLong = Maps.LatLong {lat = origin.lat, lon = origin.lon}
      -- let stopsLatLong = map (.gps) [destination] -----start using after adding stops
      void $ Serviceability.validateServiceabilityForEditDestination sourceLatLong destination.gps person
      QBUR.create bookingUpdateReq
      startLocMap <- SLM.buildPickUpLocationMapping startLocMapping.locationId bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
      QLM.create startLocMap
      oldDropLocMap <- SLM.buildDropLocationMapping oldDropLocMapping.locationId bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
      QLM.create oldDropLocMap
      newDropLocationMap <- SLM.buildDropLocationMapping newDropLocation.id bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
      QLM.create newDropLocationMap
      prevOrder <- QLM.maxOrderByEntity booking.id.getId
      let destination' = Just $ newDropLocation{id = show prevOrder}
      bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
      let dUpdateReq =
            ACL.UpdateBuildReq
              { bppId = booking.providerId,
                bppUrl = booking.providerUrl,
                transactionId = booking.transactionId,
                messageId = bookingUpdateReq.id.getId,
                city = merchant.defaultCity, -- TODO: Correct during interoperability
                details =
                  ACL.UEditLocationBuildReqDetails $
                    ACL.EditLocationBuildReqDetails
                      { bppRideId = ride.bppRideId,
                        origin = Nothing,
                        status = ACL.SOFT_UPDATE,
                        destination = destination',
                        stops = Nothing
                      },
                ..
              }
      becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
      void . withShortRetry $ CallBPP.updateV2 booking.providerUrl becknUpdateReq
      pure $ EditLocationResp (Just bookingUpdateReq.id) "Success"
    (_, _) -> throwError PickupOrDropLocationNotFound

buildLocation ::
  MonadFlow m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  EditLocation ->
  m DL.Location
buildLocation merchantId merchantOperatingCityId location = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DL.Location
      { id = guid,
        createdAt = now,
        updatedAt = now,
        lat = location.gps.lat,
        lon = location.gps.lon,
        address = location.address,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId
      }

buildbookingUpdateRequest :: MonadFlow m => DB.Booking -> m DBUR.BookingUpdateRequest
buildbookingUpdateRequest booking = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DBUR.BookingUpdateRequest
      { id = guid,
        status = DBUR.SOFT,
        createdAt = now,
        updatedAt = now,
        bookingId = booking.id,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        currentPointLat = Nothing,
        currentPointLon = Nothing,
        estimatedFare = Nothing,
        estimatedDistance = Nothing,
        oldEstimatedFare = booking.estimatedFare.amount,
        oldEstimatedDistance = distanceToHighPrecMeters <$> booking.estimatedDistance,
        totalDistance = Nothing,
        errorObj = Nothing,
        travelledDistance = Nothing,
        distanceUnit = booking.distanceUnit
      }

getDeliveryImage ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv,
    MonadFlow m,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Id SRide.Ride ->
  (Id SPerson.Person, Id DM.Merchant) ->
  m Text
getDeliveryImage rideId (_personId, merchantId) = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  CallBPPInternal.getDeliveryImage
    merchant.driverOfferApiKey
    merchant.driverOfferBaseUrl
    ride.bppRideId.getId
