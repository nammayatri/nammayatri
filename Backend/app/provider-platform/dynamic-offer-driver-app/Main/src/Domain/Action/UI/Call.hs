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

import qualified Data.Text as T
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude (Alternative ((<|>)))
import Kernel.Beam.Functions
import Kernel.External.Call.Exotel.Types
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.External.Encryption as KE
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (LoggerEnv)
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import Storage.Queries.Person as PSQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
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

type CallCallbackRes = AckResponse

type GetCustomerMobileNumberResp = Text

type GetDriverMobileNumberResp = Text

type GetCallStatusRes = SCS.CallStatusAPIEntity

-- | Try to initiate a call driver -> customer
initiateCallToCustomer ::
  ( EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToCustomer rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
  callStatusId <- generateGUID
  let callReq =
        InitiateCallReq
          { fromPhoneNum = providerPhone,
            toPhoneNum = customerPhone,
            attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = rideId.getId}
          }
  exotelResponse <- initiateCall booking.providerId callReq
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from driver to customer."
  callStatus <- buildCallStatus callStatusId exotelResponse
  _ <- QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            entityId = rideId.getId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getDriverMobileNumber :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, HasField "esqDBReplicaEnv" r EsqDBEnv, HasField "loggerEnv" r LoggerEnv) => (Id Person.Person, Id DM.Merchant) -> Text -> m CallRes
getDriverMobileNumber (driverId, merchantId) rcNo = do
  vehicleRC <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  rcActiveAssociation <- DAQuery.findActiveAssociationByRC vehicleRC.id >>= fromMaybeM ActiveRCNotFound
  callStatusId <- generateGUID
  linkedDriverNumber <- getDecryptedMobileNumberByDriverId rcActiveAssociation.driverId
  driverRequestedNumber <- getDecryptedMobileNumberByDriverId driverId
  let callReq =
        InitiateCallReq
          { fromPhoneNum = linkedDriverNumber,
            toPhoneNum = driverRequestedNumber,
            attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = vehicleRC.id.getId}
          }
  exotelResponse <- initiateCall merchantId callReq
  callStatus <- buildCallStatus callStatusId exotelResponse vehicleRC.id.getId
  QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse rcId' = do
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
            createdAt = now
          }

getDecryptedMobileNumberByDriverId :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> m Text
getDecryptedMobileNumberByDriverId driverId = do
  driver <- PSQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let encMobNum = driver.mobileNumber
  case encMobNum of
    Just mobNum -> decrypt mobNum
    Nothing -> throwError $ InvalidRequest "Mobile Number not found."

callStatusCallback :: EsqDBFlow m r => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  _ <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  _ <- QCallStatus.updateCallStatus callStatusId (exotelStatusToInterfaceStatus req.status) req.conversationDuration (Just req.recordingUrl)
  return Ack

directCallStatusCallback :: EsqDBFlow m r => Text -> ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuratioExotel callDurationFallback = do
  let callDuration = callDuratioExotel <|> callDurationFallback
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  let newCallStatus = exotelStatusToInterfaceStatus dialCallStatus
  _ <- case recordingUrl_ of
    Just recordUrl -> do
      if recordUrl == ""
        then do
          _ <- updateCallStatus callStatus.id newCallStatus Nothing callDuration
          throwError CallStatusDoesNotExist
        else do
          updateCallStatus callStatus.id newCallStatus (Just recordUrl) callDuration
    Nothing -> do
      if newCallStatus == CallTypes.COMPLETED
        then do
          _ <- updateCallStatus callStatus.id newCallStatus Nothing callDuration
          throwError CallStatusDoesNotExist
        else updateCallStatus callStatus.id newCallStatus Nothing callDuration
  return Ack
  where
    updateCallStatus id callStatus url callDuration = QCallStatus.updateCallStatus id callStatus (fromMaybe 0 callDuration) url

getCustomerMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> m GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber_ callStatus to_ = do
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  let to = dropFirstZero to_
  mobileNumberHash <- getDbHash callFrom
  exophone <-
    CQExophone.findByPhone to >>= \case
      Nothing -> CQExophone.findByPhone callTo >>= fromMaybeM (ExophoneDoesNotExist callTo)
      Just phone -> return phone
  (driver, dtmfNumberUsed) <-
    runInReplica (QPerson.findByMobileNumberAndMerchant "+91" mobileNumberHash exophone.merchantId) >>= \case
      Nothing -> do
        number <- fromMaybeM (InvalidRequest "DTMF Number Not Found") dtmfNumber_
        let dtmfNumber = dropFirstZero $ removeQuotes number
        dtmfMobileHash <- getDbHash dtmfNumber
        person <- runInReplica $ QPerson.findByMobileNumberAndMerchant "+91" dtmfMobileHash exophone.merchantId >>= fromMaybeM (PersonWithPhoneNotFound dtmfNumber)
        return (person, Just dtmfNumber)
      Just entity -> return (entity, Nothing)
  activeRide <- runInReplica (QRide.getActiveByDriverId driver.id) >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  activeBooking <- runInReplica (QRB.findById activeRide.bookingId) >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  riderId <-
    activeBooking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <- runInReplica $ QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  callId <- generateGUID
  callStatusObj <- buildCallStatus activeRide.id.getId callId callSid (exotelStatusToInterfaceStatus callStatus) dtmfNumberUsed
  _ <- QCallStatus.create callStatusObj
  return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    removeQuotes = T.replace "\"" ""
    buildCallStatus rideId callId exotelCallId exoStatus dtmfNumberUsed = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            callId = exotelCallId,
            entityId = rideId,
            dtmfNumberUsed = dtmfNumberUsed,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: MonadFlow m => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

-- | Get customer's mobile phone
getCustomerPhone :: (EncFlow m r, MonadFlow m) => SRide.Ride -> m Text
getCustomerPhone ride = do
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  riderId <-
    booking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <- runInReplica $ QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  decMobNum <- decrypt riderDetails.mobileNumber
  return $ riderDetails.mobileCountryCode <> decMobNum

-- | Get driver's mobile phone
getDriverPhone :: (EncFlow m r, MonadFlow m) => SRide.Ride -> m Text
getDriverPhone ride = do
  driver <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  decMobNum <- mapM decrypt driver.mobileNumber
  let phonenum = (<>) <$> driver.mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, MonadFlow m) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  customerPhone <- getCustomerPhone ride
  driverPhone <- getDriverPhone ride
  return (customerPhone, driverPhone)
