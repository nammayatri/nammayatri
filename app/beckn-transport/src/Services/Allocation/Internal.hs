{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Internal where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App as BC
import Beckn.Types.Common
import Beckn.Types.ID
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common
import qualified Beckn.Utils.Logging as Log
import Data.Time
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Product.Person as Person
import qualified Product.ProductInstance as PI
import Services.Allocation.Allocation as Alloc
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.RideRequest as QRR
import Types.API.ProductInstance
import Types.API.Ride (DriverResponse (..))
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import qualified Types.Storage.RideRequest as SRR
import Utils.Notifications

getCurrentTime :: Flow UTCTime
getCurrentTime = getCurrTime

getDriverSortMode :: Flow SortMode
getDriverSortMode = asks (defaultSortMode . driverAllocationConfig)

getConfiguredNotificationTime :: Flow NominalDiffTime
getConfiguredNotificationTime = asks (driverNotificationExpiry . driverAllocationConfig)

getConfiguredAllocationTime :: Flow NominalDiffTime
getConfiguredAllocationTime = asks (rideAllocationExpiry . driverAllocationConfig)

getDriverPool :: RideId -> Flow [DriverId]
getDriverPool (RideId rideId) = do
  personIds <- Person.getDriverPool (ProductInstanceId rideId)
  let driverIds = fmap toDriverId personIds
  pure driverIds

getRequests :: Integer -> Flow [RideRequest]
getRequests = fmap (map rideRequestToRideRequest) . QRR.fetchOldest

assignDriver :: RideId -> DriverId -> Flow ()
assignDriver rideId driverId = do
  ordPi <- QPI.findById productInstanceId
  searchPi <- QPI.findById =<< fromMaybeM500 "PARENT_PI_NOT_FOUND" (ordPi ^. #_parentId)
  piList <- QPI.findAllByParentId (ordPi ^. #_parentId)
  person <- QP.findPersonById personId
  let vehicleId = person ^. #_udf1
      req = buildProdInstUpdateReq vehicleId

  PI.updateVehicleDetails piList req
  PI.assignDriver piList req
  PI.updateStatus productInstanceId req
  PI.updateInfo productInstanceId
  PI.notifyUpdateToBAP searchPi ordPi (req ^. #_status)
  where
    personId = PersonId $ driverId ^. #_getDriverId
    productInstanceId = ProductInstanceId $ rideId ^. #_getRideId
    buildProdInstUpdateReq vehicleId =
      ProdInstUpdateReq
        { _status = Just PI.TRIP_ASSIGNED,
          _personId = Just $ driverId ^. #_getDriverId,
          _vehicleId = vehicleId,
          _otpCode = Nothing
        }

rideRequestToRideRequest :: SRR.RideRequest -> Alloc.RideRequest
rideRequestToRideRequest SRR.RideRequest {..} =
  Alloc.RideRequest
    { requestHeader =
        RequestHeader
          { requestId = _id,
            rideId = _rideId,
            requestTime = _createdAt
          },
      requestData = case _type of
        SRR.ALLOCATION -> Alloc.Allocation (OrderTime _createdAt)
        SRR.CANCELLATION -> Alloc.Cancellation
    }

getCurrentNotification ::
  RideId ->
  Flow (Maybe CurrentNotification)
getCurrentNotification rideId =
  QNS.findActiveNotificationByRideId rideId
    >>= maybe (return Nothing) (fmap Just . buildCurrentNotification)
  where
    buildCurrentNotification notificationStatus = do
      notifiedAt <-
        notificationStatus ^. #_notifiedAt
          & fromMaybeM500 "NOTIFICATION_STATUS_NOT_NOTIFIED"
      let driverId = notificationStatus ^. #_driverId
      return (Alloc.CurrentNotification driverId notifiedAt)

sendNewRideNotification :: RideId -> DriverId -> Flow ()
sendNewRideNotification (RideId rideId) (DriverId driverId) = do
  prodInst <- QPI.findById (ProductInstanceId rideId)
  person <- QP.findPersonById (PersonId driverId)
  notifyDriverNewAllocation prodInst person

sendRideNotAssignedNotification :: RideId -> DriverId -> Flow ()
sendRideNotAssignedNotification (RideId rideId) (DriverId driverId) = do
  prodInst <- QPI.findById (ProductInstanceId rideId)
  person <- QP.findPersonById (PersonId driverId)
  notifyDriverUnassigned prodInst person

updateNotificationStatus :: RideId -> DriverId -> NotificationStatus -> Flow ()
updateNotificationStatus rideId driverId =
  QNS.updateStatus rideId driverId . allocNotifStatusToStorageStatus

resetLastRejectionTime :: DriverId -> Flow ()
resetLastRejectionTime = QDS.updateIdleTime

getAttemptedDrivers :: RideId -> Flow [DriverId]
getAttemptedDrivers rideId =
  QNS.fetchRefusedNotificationsByRideId rideId <&> map (^. #_driverId)

getDriversWithNotification :: Flow [DriverId]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (^. #_driverId)

getFirstDriverInTheQueue :: NonEmpty DriverId -> Flow DriverId
getFirstDriverInTheQueue = QDS.getFirstDriverInTheQueue . toList

checkAvailability :: NonEmpty DriverId -> Flow [DriverId]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map SDriverInfo._driverId driversInfo

getDriverResponse :: RideId -> DriverId -> Flow (Maybe DriverResponse)
getDriverResponse rideId driverId =
  Redis.getKeyRedis $ "beckn:" <> _getRideId rideId <> ":" <> _getDriverId driverId <> ":response"

cancelRide :: RideId -> Flow ()
cancelRide = BP.cancelRide

cleanupRide :: RideId -> Flow ()
cleanupRide rideId = do
  QRR.removeRide rideId
  QNS.removeRide rideId

resetRequestTime :: RideRequestId -> Flow ()
resetRequestTime = QRR.updateLastProcessTime

logInfo :: Text -> Text -> Flow ()
logInfo = Log.logInfo

logError :: Text -> Text -> Flow ()
logError = Log.logError

runSafely :: (FromJSON a, ToJSON a) => Flow a -> Flow (Either Text a)
runSafely = runSafeFlow

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified _ -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addNotificationStatus ::
  RideId ->
  DriverId ->
  NotificationStatus ->
  Flow ()
addNotificationStatus rideId driverId status = do
  uuid <- generateGUID
  QNS.create
    SNS.NotificationStatus
      { _id = ID uuid,
        _rideId = rideId,
        _driverId = driverId,
        _status = allocNotifStatusToStorageStatus status,
        _notifiedAt = notifiedAt
      }
  where
    notifiedAt =
      case status of
        Alloc.Notified time -> Just time
        _ -> Nothing

addAvailableDriver :: SDriverInfo.DriverInformation -> [DriverId] -> [DriverId]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo ^. #_active && not (driverInfo ^. #_onRide)
    then driverInfo ^. #_driverId : availableDriversIds
    else availableDriversIds

toDriverId :: PersonId -> DriverId
toDriverId = DriverId . _getPersonId

logEvent :: AllocationEventType -> RideId -> Flow ()
logEvent = logAllocationEvent

getRideStatus :: RideId -> Flow RideStatus
getRideStatus rideId = do
  productInstance <- QPI.findById productInstanceId
  castToRideStatus $ productInstance ^. #_status
  where
    productInstanceId = ProductInstanceId $ _getRideId rideId
    castToRideStatus = \case
      PI.CONFIRMED -> return CONFIRMED
      PI.TRIP_ASSIGNED -> return TRIP_ASSIGNED
      PI.INPROGRESS -> return INPROGRESS
      PI.COMPLETED -> return COMPLETED
      PI.CANCELLED -> return CANCELLED
      _ -> throwErrorJSON500 "UNKNOWN_STATUS"
