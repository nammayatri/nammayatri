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
    GetDriverMobileNumberResp,
    GetCallStatusRes,
    initiateCallToDriver,
    callStatusCallback,
    directCallStatusCallback,
    getCallStatus,
    getDriverMobileNumber,
  )
where

import Data.Text
import qualified Data.Text as T
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as DCS
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import qualified Kernel.External.Call.Exotel.Types as Call
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica, runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as Merchant
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Call as Call
import Tools.Error
import Tools.Metrics

newtype CallRes = CallRes
  { callId :: Id DCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallbackReq CallAttachments

data CallAttachments = CallAttachments
  { callStatusId :: Id DCS.CallStatus,
    rideId :: Id SRide.Ride
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackRes = AckResponse

type GetDriverMobileNumberResp = Text

type GetCallStatusRes = DCS.CallStatusAPIEntity

-- | Try to initiate a call customer -> driver
initiateCallToDriver ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToDriver rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
  callStatusId <- generateGUID
  let callReq =
        Call.InitiateCallReq
          { fromPhoneNum = customerPhone,
            toPhoneNum = providerPhone,
            attachments = Call.Attachments $ CallAttachments {callStatusId = callStatusId, rideId = rideId}
          }
  exotelResponse <- Call.initiateCall booking.merchantId callReq
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
  callStatus <- buildCallStatus callStatusId exotelResponse
  runTransaction $ QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse = do
      now <- getCurrentTime
      return $
        CallStatus
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

directCallStatusCallback :: EsqDBFlow m r => Text -> Call.ExotelCallStatus -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuration = do
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id (exotelStatusToInterfaceStatus dialCallStatus) (fromMaybe 0 callDuration) recordingUrl
  return Ack

getDriverMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Call.ExotelCallStatus -> m GetDriverMobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ callStatus = do
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  merchant <- Merchant.findByExoPhone callTo >>= fromMaybeM (MerchantWithExoPhoneNotFound callTo)
  mobileNumberHash <- getDbHash callFrom
  person <-
    runInReplica (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash merchant.id)
      >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  booking <- runInReplica $ QRB.findAssignedByRiderId person.id >>= fromMaybeM (BookingForRiderNotFound $ getId person.id)
  ride <- runInReplica $ QRide.findActiveByRBId booking.id >>= fromMaybeM (RideWithBookingIdNotFound $ getId booking.id)
  callId <- generateGUID
  callStatusObj <- buildCallStatus ride.id callId callSid (exotelStatusToInterfaceStatus callStatus)
  runTransaction $ QCallStatus.create callStatusObj
  return ride.driverMobileNumber
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            callId = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: EsqDBReplicaFlow m r => Id CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let personId = booking.riderId
  Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- mapM decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, EsqDBFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
