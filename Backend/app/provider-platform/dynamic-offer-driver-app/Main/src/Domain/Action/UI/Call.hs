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
    GetCallStatusRes,
    initiateCallToCustomer,
    callStatusCallback,
    getCallStatus,
    directCallStatusCallback,
    getCustomerMobileNumber,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Ride as SRide
import Kernel.External.Call.Exotel.Types
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import Kernel.External.Call.Interface.Types
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow, runInReplica, runTransaction)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Call
import Tools.Error
import Tools.Metrics

newtype CallRes = CallRes
  { callId :: Id SCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = ExotelCallCallbackReq CallAttachments

data CallAttachments = CallAttachments
  { callStatusId :: Id SCS.CallStatus,
    rideId :: Id SRide.Ride
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackRes = AckResponse

type GetCustomerMobileNumberResp = Text

type GetCallStatusRes = SCS.CallStatusAPIEntity

-- | Try to initiate a call driver -> customer
initiateCallToCustomer ::
  ( EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    CoreMetrics m,
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
            attachments = Attachments $ CallAttachments {callStatusId = callStatusId, rideId = rideId}
          }
  exotelResponse <- initiateCall booking.providerId callReq
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from driver to customer."
  callStatus <- buildCallStatus callStatusId exotelResponse
  runTransaction $ QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = rideId,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: EsqDBFlow m r => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  _ <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus callStatusId (exotelStatusToInterfaceStatus req.status) req.conversationDuration req.recordingUrl
  return Ack

directCallStatusCallback :: EsqDBFlow m r => Text -> ExotelCallStatus -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuration = do
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id (exotelStatusToInterfaceStatus dialCallStatus) (fromMaybe 0 callDuration) recordingUrl
  return Ack

getCustomerMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> ExotelCallStatus -> m GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ callStatus = do
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  mobileNumberHash <- getDbHash callFrom
  exophone <- CQExophone.findByPhone callTo >>= fromMaybeM (ExophoneDoesNotExist callTo)
  driver <- runInReplica $ QPerson.findByMobileNumberAndMerchant "+91" mobileNumberHash exophone.merchantId >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- runInReplica $ QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  activeBooking <- runInReplica $ QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  riderId <-
    activeBooking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica $
      QRD.findById riderId
        >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  callId <- generateGUID
  callStatusObj <- buildCallStatus activeRide.id callId callSid (exotelStatusToInterfaceStatus callStatus)
  runTransaction $ QCallStatus.create callStatusObj
  return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            callId = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: EsqDBReplicaFlow m r => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

-- | Get customer's mobile phone
getCustomerPhone :: (EncFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SRide.Ride -> m Text
getCustomerPhone ride = do
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  riderId <-
    booking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica $
      QRD.findById riderId
        >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  decMobNum <- decrypt riderDetails.mobileNumber
  return $ riderDetails.mobileCountryCode <> decMobNum

-- | Get driver's mobile phone
getDriverPhone :: (EncFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  driver <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  decMobNum <- mapM decrypt driver.mobileNumber
  let phonenum = (<>) <$> driver.mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  customerPhone <- getCustomerPhone ride
  driverPhone <- getDriverPhone ride
  return (customerPhone, driverPhone)
