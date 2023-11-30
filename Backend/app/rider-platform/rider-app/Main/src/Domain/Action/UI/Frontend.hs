{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Frontend
  ( GetPersonFlowStatusRes,
    FrontendEvent (..),
    NotifyEventReq (..),
    NotifyEventResp,
    getPersonFlowStatus,
    notifyEvent,
  )
where

import qualified Data.HashMap as HM
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

data GetPersonFlowStatusRes = GetPersonFlowStatusRes
  { oldStatus :: Maybe DPFS.FlowStatus,
    currentStatus :: DPFS.FlowStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrontendEvent = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype NotifyEventReq = NotifyEventReq
  { event :: FrontendEvent
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type NotifyEventResp = APISuccess

getPersonFlowStatus :: Id DP.Person -> Maybe Bool -> Flow GetPersonFlowStatusRes
getPersonFlowStatus personId mIsPolling = do
  -- should not be run in replica
  personStatus <- QPFS.getStatus personId >>= fromMaybeM (PersonNotFound personId.getId)
  case personStatus of
    DPFS.SEARCHING _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.GOT_ESTIMATE _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.WAITING_FOR_DRIVER_OFFERS _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.DRIVER_OFFERED_QUOTE _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.WAITING_FOR_DRIVER_ASSIGNMENT _ _ -> expirePersonStatusIfNeeded personStatus
    DPFS.RIDE_PICKUP {} -> handleRideTracking personId mIsPolling personStatus
    DPFS.RIDE_STARTED {} -> handleRideTracking personId mIsPolling personStatus
    DPFS.DRIVER_ARRIVED {} -> handleRideTracking personId mIsPolling personStatus
    a -> return $ GetPersonFlowStatusRes Nothing a
  where
    expirePersonStatusIfNeeded personStatus = do
      now <- getCurrentTime
      if now < personStatus.validTill
        then return $ GetPersonFlowStatusRes Nothing personStatus
        else do
          _ <- QPFS.updateStatus personId DPFS.IDLE
          return $ GetPersonFlowStatusRes (Just personStatus) DPFS.IDLE

notifyEvent :: (CacheFlow m r, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m) => Id DP.Person -> NotifyEventReq -> m NotifyEventResp
notifyEvent personId req = do
  _ <- case req.event of
    RATE_DRIVER_SKIPPED -> QPFS.updateStatus personId DPFS.IDLE
    SEARCH_CANCELLED -> do
      activeBooking <- B.runInReplica $ QB.findLatestByRiderIdAndStatus personId DRB.activeBookingStatus
      whenJust activeBooking $ \_ -> throwError (InvalidRequest "ACTIVE_BOOKING_EXISTS")
      QPFS.updateStatus personId DPFS.IDLE
  QPFS.clearCache personId
  pure APISuccess.Success

handleRideTracking ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "rideCfg" r RideConfig,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  Id DP.Person ->
  Maybe Bool ->
  DPFS.FlowStatus ->
  m GetPersonFlowStatusRes
handleRideTracking _ Nothing DPFS.RIDE_STARTED {..} = return $ GetPersonFlowStatusRes Nothing (DPFS.RIDE_ASSIGNED rideId) -- handle backward compatibility, if isPolling is Nothing means old version of API Call
handleRideTracking personId (Just isPolling) DPFS.RIDE_STARTED {..} = do
  trackUrl <- getTrackUrl rideId trackingUrl
  newDriverLocation <- if isPolling then Just . (.currPoint) <$> CallBPP.callGetDriverLocation trackUrl else return Nothing
  let updatedStatus = DPFS.RIDE_STARTED {trackingUrl = trackUrl, driverLocation = newDriverLocation, ..}
  updateStatus personId updatedStatus
  return $ GetPersonFlowStatusRes Nothing updatedStatus
handleRideTracking _ Nothing DPFS.RIDE_PICKUP {..} = return $ GetPersonFlowStatusRes Nothing (DPFS.RIDE_ASSIGNED rideId) -- handle backward compatibility, if isPolling is Nothing means old version of API Call
handleRideTracking personId (Just isPolling) DPFS.RIDE_PICKUP {..} = do
  trackUrl <- getTrackUrl rideId trackingUrl
  newDriverLocation <-
    if isPolling
      then do
        location <- CallBPP.callGetDriverLocation trackUrl
        notifyOnTheWayOrReached location
        return $ Just location.currPoint
      else return Nothing
  let updatedStatus = DPFS.RIDE_PICKUP {trackingUrl = trackUrl, driverLocation = newDriverLocation, ..}
  updateStatus personId updatedStatus
  return $ GetPersonFlowStatusRes Nothing updatedStatus
  where
    notifyOnTheWayOrReached location = do
      driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
      driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
      mbIsOnTheWayNotified <- Redis.get @() driverOnTheWay
      mbHasReachedNotified <- Redis.get @() driverHasReached

      when (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified) $ do
        let distance = highPrecMetersToMeters $ distanceBetweenInMeters fromLocation location.currPoint
        mbStartDistance <- Redis.get @Meters distanceUpdates
        case mbStartDistance of
          Nothing -> Redis.setExp distanceUpdates distance 3600
          Just startDistance -> when (startDistance - 50 > distance) $ do
            unless (isJust mbIsOnTheWayNotified) $ do
              Notify.notifyDriverOnTheWay personId
              Redis.setExp driverOnTheWay () driverOnTheWayNotifyExpiry
            when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
              Notify.notifyDriverHasReached personId otp vehicleNumber
              Redis.setExp driverHasReached () 1500
      where
        distanceUpdates = "Ride:GetDriverLoc:DriverDistance " <> rideId.getId
        driverOnTheWay = "Ride:GetDriverLoc:DriverIsOnTheWay " <> rideId.getId
        driverHasReached = "Ride:GetDriverLoc:DriverHasReached " <> rideId.getId
handleRideTracking _ Nothing DPFS.DRIVER_ARRIVED {..} = return $ GetPersonFlowStatusRes Nothing (DPFS.RIDE_ASSIGNED rideId) -- handle backward compatibility, if isPolling is Nothing means old version of API Call
handleRideTracking personId (Just isPolling) DPFS.DRIVER_ARRIVED {..} = do
  trackUrl <- getTrackUrl rideId trackingUrl
  newDriverLocation <- if isPolling then Just . (.currPoint) <$> CallBPP.callGetDriverLocation trackUrl else return Nothing
  let updatedStatus = DPFS.DRIVER_ARRIVED {trackingUrl = trackUrl, driverLocation = newDriverLocation, ..}
  updateStatus personId updatedStatus
  return $ GetPersonFlowStatusRes Nothing updatedStatus
handleRideTracking _ _ status = return $ GetPersonFlowStatusRes Nothing status

updateStatus :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id DP.Person -> DPFS.FlowStatus -> m ()
updateStatus personId updatedStatus = do
  _ <- QPFS.updateStatus personId updatedStatus
  QPFS.clearCache personId

getTrackUrl :: (Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SRide.Ride -> Maybe BaseUrl -> m (Maybe BaseUrl)
getTrackUrl rideId mTrackUrl = do
  case mTrackUrl of
    Nothing -> do
      ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      return ride.trackingUrl
    a -> return a
