{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Call
  ( CallRes (..),
    CallCallbackReq,
    CallAttachments (..),
    CallCallbackRes,
    GetCustomerMobileNumberResp,
    GetDriverMobileNumberResp,
    GetCallStatusRes,
    initiateCallToCustomer,
    callStatusCallback,
    getCallStatus,
    directCallStatusCallback,
    getCustomerMobileNumber,
    getDriverMobileNumber,
    callOnClickTracker,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.CallEvent as DCE
import qualified Domain.Types.Booking as DB
import Domain.Types.CallStatus as CallStatus
import Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude (Alternative ((<|>)))
import Kernel.Beam.Functions
import Kernel.External.Call.Exotel.Types
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.External.Encryption as KE
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import Storage.Queries.Person as PSQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Call
import Tools.Error

newtype CallRes = CallRes
  { callId :: Id SCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = ExotelCallCallbackReq CallAttachments

data CallAttachments = CallAttachments
  { callStatusId :: Id SCS.CallStatus,
    entityId :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

data CallStatusAPIEntity = CallStatusAPIEntity
  { callStatusId :: Id CallStatus.CallStatus,
    entityId :: Maybe Text,
    status :: CallTypes.CallStatus
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

makeCallStatusAPIEntity :: CallStatus.CallStatus -> CallStatusAPIEntity
makeCallStatusAPIEntity CallStatus.CallStatus {..} =
  CallStatusAPIEntity
    { callStatusId = id,
      ..
    }

type CallCallbackRes = AckResponse

type GetCustomerMobileNumberResp = Text

type GetDriverMobileNumberResp = Text

type GetCallStatusRes = CallStatusAPIEntity

-- | Try to initiate a call driver -> customer
initiateCallToCustomer ::
  ( EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  Id DMOC.MerchantOperatingCity ->
  m CallRes
initiateCallToCustomer rideId merchantOpCityId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  (customerPhone, providerPhone) <- getCustomerAndDriverPhones ride booking
  callStatusId <- generateGUID
  let callReq =
        InitiateCallReq
          { fromPhoneNum = providerPhone,
            toPhoneNum = Just customerPhone,
            attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = rideId.getId},
            appletId = Nothing
          }
  exotelResponse <- initiateCall booking.providerId merchantOpCityId callReq
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from driver to customer."
  let dCallStatus = Just $ handleCallStatus exotelResponse.callStatus
  callStatus <- buildCallStatus callStatusId exotelResponse dCallStatus booking
  _ <- QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse dCallStatus booking = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            entityId = Just rideId.getId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just booking.providerId.getId,
            callService = Just Exotel,
            callAttempt = dCallStatus,
            callError = Nothing,
            createdAt = now
          }

getDriverMobileNumber :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, HasField "esqDBReplicaEnv" r EsqDBEnv, HasField "loggerEnv" r LoggerEnv) => (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> m CallRes
getDriverMobileNumber (driverId, merchantId, merchantOpCityId) rcNo = do
  vehicleRC <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  rcActiveAssociation <- DAQuery.findActiveAssociationByRC vehicleRC.id True >>= fromMaybeM ActiveRCNotFound
  callStatusId <- generateGUID
  linkedDriverNumber <- getDecryptedMobileNumberByDriverId rcActiveAssociation.driverId
  driverRequestedNumber <- getDecryptedMobileNumberByDriverId driverId
  let callReq =
        InitiateCallReq
          { fromPhoneNum = linkedDriverNumber,
            toPhoneNum = Just driverRequestedNumber,
            attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = vehicleRC.id.getId},
            appletId = Nothing
          }
  exotelResponse <- initiateCall merchantId merchantOpCityId callReq
  let dCallStatus = Just $ handleCallStatus exotelResponse.callStatus
  callStatus <- buildCallStatus callStatusId exotelResponse dCallStatus (Just vehicleRC.id.getId)
  QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse dCallStatus rcId' = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            entityId = rcId',
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just merchantId.getId,
            callError = Nothing,
            callService = Just Exotel,
            callAttempt = dCallStatus,
            createdAt = now
          }

getDecryptedMobileNumberByDriverId :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> m Text
getDecryptedMobileNumberByDriverId driverId = do
  driver <- PSQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let encMobNum = driver.mobileNumber
  case encMobNum of
    Just mobNum -> decrypt mobNum
    Nothing -> throwError $ InvalidRequest "Mobile Number not found."

callStatusCallback :: (CacheFlow m r, EsqDBFlow m r) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  _ <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  let interfaceStatus = exotelStatusToInterfaceStatus req.status
  let dCallStatus = Just $ handleCallStatus interfaceStatus
  QCallStatus.updateCallStatus req.conversationDuration (Just req.recordingUrl) interfaceStatus dCallStatus callStatusId
  return Ack

directCallStatusCallback :: (EsqDBFlow m r, EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r) => Text -> ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuratioExotel callDurationFallback = do
  let callDuration = callDuratioExotel <|> callDurationFallback
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  let newCallStatus = exotelStatusToInterfaceStatus dialCallStatus
  let dCallStatus = Just $ handleCallStatus newCallStatus
  _ <- case recordingUrl_ of
    Just recordUrl -> do
      if recordUrl == ""
        then do
          updateCallStatus callStatus.id newCallStatus Nothing callDuration dCallStatus
          throwError CallStatusDoesNotExist
        else do
          updateCallStatus callStatus.id newCallStatus (Just recordUrl) callDuration dCallStatus
    Nothing -> do
      if newCallStatus == CallTypes.COMPLETED
        then do
          updateCallStatus callStatus.id newCallStatus Nothing callDuration dCallStatus
          throwError CallStatusDoesNotExist
        else updateCallStatus callStatus.id newCallStatus Nothing callDuration dCallStatus
  DCE.sendCallDataToKafka (Just "EXOTEL") (Id <$> callStatus.entityId) (Just "ANONYMOUS_CALLER") (Just callSid) (Just (show dialCallStatus)) System Nothing
  return Ack
  where
    updateCallStatus id callStatus url callDuration callAttemptStatus = QCallStatus.updateCallStatus (fromMaybe 0 callDuration) url callStatus callAttemptStatus id

getCustomerMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, EventStreamFlow m r) => Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> m GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber_ callStatus to_ = do
  callStatusId <- generateGUID
  let callFrom = dropFirstZero callFrom_
      callTo = dropFirstZero callTo_
      to = dropFirstZero to_

  mobileNumberHash <- getDbHash callFrom
  exophone <-
    CQExophone.findByPhone to >>= \case
      Nothing -> CQExophone.findByPhone callTo >>= fromMaybeM (ExophoneDoesNotExist callTo)
      Just phone -> return phone

  (driver, dtmfNumberUsed) <-
    runInReplica (QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash exophone.merchantId Person.DRIVER) >>= \case
      Just entity -> return (entity, Nothing)
      Nothing -> do
        number <- maybe (throwError (PersonWithPhoneNotFound $ show dtmfNumber_)) pure dtmfNumber_
        let dtmfNumber = dropFirstZero $ removeQuotes number
        dtmfMobileHash <- getDbHash dtmfNumber
        person <-
          runInReplica (QPerson.findByMobileNumberAndMerchantAndRole "+91" dtmfMobileHash exophone.merchantId Person.DRIVER)
            >>= maybe (throwError (PersonWithPhoneNotFound dtmfNumber)) pure
        return (person, Just dtmfNumber)

  activeRide <- runInReplica (QRide.getActiveByDriverId driver.id) >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  ensureCallStatusExists callStatusId callSid activeRide.id callStatus dtmfNumberUsed

  activeBooking <-
    runInReplica (QRB.findById activeRide.bookingId)
      >>= maybe (throwCallError callStatusId (BookingNotFound $ getId activeRide.bookingId) (Just exophone.merchantId.getId) (Just exophone.callService)) pure

  riderId <- activeBooking.riderId & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica (QRD.findById riderId)
      >>= maybe (throwCallError callStatusId (RiderDetailsNotFound riderId.getId) (Just exophone.merchantId.getId) (Just exophone.callService)) pure

  requestorPhone <- decrypt riderDetails.mobileNumber
  QCallStatus.updateCallStatusInformation dtmfNumberUsed (Just exophone.merchantId.getId) (Just exophone.callService) callStatusId
  DCE.sendCallDataToKafka (Just "EXOTEL") (Just activeRide.id) (Just "ANONYMOUS_CALLER") (Just callSid) Nothing System (Just to)

  return requestorPhone
  where
    ensureCallStatusExists callStatusId callId rideId callStatus' dtmfNumberUsed =
      QCallStatus.findByEntityId (Just $ getId rideId) >>= \case
        Just _ -> QCallStatus.updateCallStatusCallId callId callStatusId
        Nothing -> do
          now <- getCurrentTime
          let callStatusObj =
                SCS.CallStatus
                  { id = callStatusId,
                    callId = callId,
                    entityId = Just $ getId rideId,
                    dtmfNumberUsed = dtmfNumberUsed,
                    status = exotelStatusToInterfaceStatus callStatus',
                    conversationDuration = 0,
                    recordingUrl = Nothing,
                    merchantId = Nothing,
                    callService = Nothing,
                    callError = Nothing,
                    callAttempt = Just DCallStatus.Pending,
                    createdAt = now
                  }
          void $ QCallStatus.create callStatusObj

    dropFirstZero = T.dropWhile (== '0')
    removeQuotes = T.replace "\"" ""

callOnClickTracker :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType, HasField "maxShards" r Int) => Id SRide.Ride -> m APISuccess
callOnClickTracker rideId = do
  maxShards <- asks (.maxShards)
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  transporterConfig <-
    CCT.findByMerchantOpCityId booking.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  callStatusObj <- buildCallStatus
  QCallStatus.create callStatusObj
  createJobIn @_ @'CheckExotelCallStatusAndNotifyBAP (fromIntegral transporterConfig.exotelStatusCheckSchedulerDelay) maxShards $
    CheckExotelCallStatusAndNotifyBAPJobData
      { rideId = ride.id
      }
  return Success
  where
    buildCallStatus = do
      callStatusId <- generateGUID
      callId <- generateGUID -- added random placeholder for backward compatibility
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = callId,
            entityId = Just $ getId rideId,
            dtmfNumberUsed = Nothing,
            status = CallTypes.ATTEMPTED,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Nothing,
            callService = Nothing,
            callError = Nothing,
            callAttempt = Just DCallStatus.Attempted,
            createdAt = now
          }

throwCallError ::
  ( HasCallStack,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    IsBaseException e
  ) =>
  Id DCallStatus.CallStatus ->
  e ->
  Maybe Text ->
  Maybe CallService ->
  m a
throwCallError callStatusId err merchantId callService = do
  QCallStatus.updateCallError (Just (show err)) callService merchantId callStatusId
  throwError err

getCallStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

-- | Get customer's mobile phone
getCustomerPhone :: (EncFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DB.Booking -> m Text
getCustomerPhone booking = do
  riderId <-
    booking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <- runInReplica $ QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  decMobNum <- decrypt riderDetails.mobileNumber
  return $ riderDetails.mobileCountryCode <> decMobNum

-- | Get driver's mobile phone
getDriverPhone :: (EncFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  driver <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  decMobNum <- mapM decrypt driver.mobileNumber
  let phonenum = (<>) <$> driver.mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SRide.Ride -> DB.Booking -> m (Text, Text)
getCustomerAndDriverPhones ride booking = do
  customerPhone <- getCustomerPhone booking
  driverPhone <- getDriverPhone ride
  return (customerPhone, driverPhone)

handleCallStatus :: CallTypes.CallStatus -> DCallStatus.CallAttemptStatus
handleCallStatus status
  | status `elem` failedCallStatuses = DCallStatus.Failed
  | status `elem` ignoredCallStatuses = DCallStatus.Resolved
  | status == CallTypes.COMPLETED = DCallStatus.Resolved
  | otherwise = DCallStatus.Pending
  where
    failedCallStatuses = [CallTypes.INVALID_STATUS, CallTypes.NOT_CONNECTED, CallTypes.FAILED]
    ignoredCallStatuses = [CallTypes.BUSY, CallTypes.NO_ANSWER, CallTypes.MISSED]
