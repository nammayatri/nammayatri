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
  )
where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Domain.Action.UI.CallEvent as DCE
import qualified Domain.Types.Booking as DB
import Domain.Types.CallStatus as CallStatus
import Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Exophone as DExophone
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
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBAP as CallBAP
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
import TransactionLogs.Types

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
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    CallBAPConstraints m r c
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
  when (dCallStatus == Just DCallStatus.Failed) $ do
    void $ CallBAP.sendPhoneCallRequestUpdateToBAP booking ride
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
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            callService = Just Exotel,
            callAttempt = dCallStatus,
            callError = Nothing,
            createdAt = now
          }

getDriverMobileNumber :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, HasField "esqDBReplicaEnv" r EsqDBEnv, HasField "loggerEnv" r LoggerEnv, CallBAPConstraints m r c) => (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> m CallRes
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
  isSendFCMSuccess <- sendFCMToBAPOnFailedCallStatus exotelResponse.callStatus (Left rcActiveAssociation.driverId)
  let callAttemptStatus = if isSendFCMSuccess then Just DCallStatus.Resolved else dCallStatus
  callStatus <- buildCallStatus callStatusId exotelResponse callAttemptStatus (Just vehicleRC.id.getId) merchantOpCityId
  QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse callAttemptStatus rcId' merchantOpCityId' = do
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
            merchantOperatingCityId = Just merchantOpCityId',
            callError = Nothing,
            callService = Just Exotel,
            callAttempt = callAttemptStatus,
            createdAt = now
          }

getDecryptedMobileNumberByDriverId :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> m Text
getDecryptedMobileNumberByDriverId driverId = do
  driver <- PSQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let encMobNum = driver.mobileNumber
  case encMobNum of
    Just mobNum -> decrypt mobNum
    Nothing -> throwError $ InvalidRequest "Mobile Number not found."

callStatusCallback :: (CacheFlow m r, EsqDBFlow m r, CallBAPConstraints m r c) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  callStatus <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  let interfaceStatus = exotelStatusToInterfaceStatus req.status
  let dCallStatus = Just $ handleCallStatus interfaceStatus
  isSendFCMSuccess <- sendFCMToBAPOnFailedCallStatus interfaceStatus (Right callStatus.entityId)
  let callAttemptStatus = if isSendFCMSuccess then Just DCallStatus.Resolved else dCallStatus
  QCallStatus.updateCallStatus req.conversationDuration (Just req.recordingUrl) interfaceStatus callAttemptStatus callStatusId
  return Ack

directCallStatusCallback :: (EsqDBFlow m r, EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r, CallBAPConstraints m r c) => Text -> ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuratioExotel callDurationFallback = do
  let callDuration = callDuratioExotel <|> callDurationFallback
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  let newCallStatus = exotelStatusToInterfaceStatus dialCallStatus
  let dCallStatus = Just $ handleCallStatus newCallStatus
  isSendFCMSuccess <- sendFCMToBAPOnFailedCallStatus newCallStatus (Right callStatus.entityId)
  let callAttemptStatus = if isSendFCMSuccess then Just DCallStatus.Resolved else dCallStatus
  _ <- case recordingUrl_ of
    Just recordUrl -> do
      if recordUrl == ""
        then do
          updateCallStatus callStatus.id newCallStatus Nothing callDuration callAttemptStatus
          throwError CallStatusDoesNotExist
        else do
          updateCallStatus callStatus.id newCallStatus (Just recordUrl) callDuration callAttemptStatus
    Nothing -> do
      if newCallStatus == CallTypes.COMPLETED
        then do
          updateCallStatus callStatus.id newCallStatus Nothing callDuration callAttemptStatus
          throwError CallStatusDoesNotExist
        else updateCallStatus callStatus.id newCallStatus Nothing callDuration callAttemptStatus
  DCE.sendCallDataToKafka (Just "EXOTEL") (Id <$> callStatus.entityId) (Just "ANONYMOUS_CALLER") (Just callSid) (Just (show dialCallStatus)) System Nothing
  return Ack
  where
    updateCallStatus id callStatus url callDuration callAttemptStatus = QCallStatus.updateCallStatus (fromMaybe 0 callDuration) url callStatus callAttemptStatus id

getCustomerMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, EventStreamFlow m r) => Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> m GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber_ callStatus to_ = do
  id' <- generateGUID
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
  ensureCallStatusExists id' callSid activeRide.id callStatus dtmfNumberUsed activeRide.merchantOperatingCityId

  activeBooking <-
    runInReplica (QRB.findById activeRide.bookingId)
      >>= maybe (throwCallError id' (BookingNotFound $ getId activeRide.bookingId) (Just exophone.merchantId.getId) (Just exophone.callService)) pure

  riderId <- case exophone.exophoneType of
    DExophone.CALL_DELIVERY_SENDER -> (activeBooking.senderDetails <&> (.id)) & fromMaybeM (BookingFieldNotPresent "senderDetails")
    DExophone.CALL_DELIVERY_RECEIVER -> (activeBooking.receiverDetails <&> (.id)) & fromMaybeM (BookingFieldNotPresent "receiverDetails")
    _ -> activeBooking.riderId & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica (QRD.findById riderId)
      >>= maybe (throwCallError id' (RiderDetailsNotFound riderId.getId) (Just exophone.merchantId.getId) (Just exophone.callService)) pure

  requestorPhone <- decrypt riderDetails.mobileNumber
  QCallStatus.updateCallStatusInformation dtmfNumberUsed (Just exophone.merchantId.getId) (Just exophone.callService) id'
  DCE.sendCallDataToKafka (Just "EXOTEL") (Just activeRide.id) (Just "ANONYMOUS_CALLER") (Just callSid) Nothing System (Just to)

  return requestorPhone
  where
    ensureCallStatusExists id' callId rideId callStatus' dtmfNumberUsed merchantOperatingCityId' =
      QCallStatus.findOneByEntityId (Just $ getId rideId) >>= \case
        Just cs -> QCallStatus.updateCallStatusCallId callId cs.id
        Nothing -> do
          now <- getCurrentTime
          let callStatusObj =
                SCS.CallStatus
                  { id = id',
                    callId = callId,
                    entityId = Just $ getId rideId,
                    dtmfNumberUsed = dtmfNumberUsed,
                    status = exotelStatusToInterfaceStatus callStatus',
                    conversationDuration = 0,
                    recordingUrl = Nothing,
                    merchantId = Nothing,
                    merchantOperatingCityId = Just merchantOperatingCityId',
                    callService = Nothing,
                    callError = Nothing,
                    callAttempt = Just DCallStatus.Pending,
                    createdAt = now
                  }
          void $ QCallStatus.create callStatusObj

    dropFirstZero = T.dropWhile (== '0')
    removeQuotes = T.replace "\"" ""

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
  | status `elem` successCallStatuses = DCallStatus.Resolved
  | otherwise = DCallStatus.Pending
  where
    failedCallStatuses = [CallTypes.INVALID_STATUS, CallTypes.NOT_CONNECTED, CallTypes.FAILED]
    ignoredCallStatuses = [CallTypes.BUSY, CallTypes.NO_ANSWER, CallTypes.MISSED, CallTypes.CANCELED, CallTypes.QUEUED]
    successCallStatuses = [CallTypes.COMPLETED, CallTypes.RINGING, CallTypes.IN_PROGRESS, CallTypes.CONNECTED]

sendFCMToBAPOnFailedCallStatus ::
  CallBAPConstraints m r c =>
  CallTypes.CallStatus ->
  Either (Id Person.Person) (Maybe Text) ->
  m Bool
sendFCMToBAPOnFailedCallStatus callStatus idInfo =
  if isFailureStatus callStatus
    then do
      (ride, booking) <- case idInfo of
        Left driverId -> do
          ride <- runInReplica $ QRide.getActiveByDriverId driverId >>= fromMaybeM (RideForDriverNotFound $ getId driverId)
          booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
          return (ride, booking)
        Right maybeEntityId -> do
          rideId <- fromMaybeM (CallStatusFieldNotPresent "rideId") maybeEntityId
          ride <- runInReplica $ QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
          booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
          return (ride, booking)
      void $ CallBAP.sendPhoneCallRequestUpdateToBAP booking ride
      return True
    else return False
  where
    isFailureStatus :: CallTypes.CallStatus -> Bool
    isFailureStatus status = status `elem` [CallTypes.INVALID_STATUS, CallTypes.NOT_CONNECTED, CallTypes.FAILED]

type CallBAPConstraints m r c =
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv
  )
