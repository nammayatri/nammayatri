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
    CallCallbackRes,
    GetCallStatusRes,
    GetCustomerMobileNumberResp,
    initiateCallToCustomer,
    callStatusCallback,
    directCallStatusCallback,
    getCustomerMobileNumber,
    getCallStatus,
  )
where

import Data.Text
import qualified Data.Text as T
import qualified Domain.Types.CallStatus as DCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Kernel.External.Call.Exotel.Types
import qualified Kernel.External.Call.Exotel.Types as Call
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
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

type GetCustomerMobileNumberResp = Text

type GetCallStatusRes = DCS.CallStatusAPIEntity

initiateCallToCustomer ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToCustomer rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <-
    QRB.findById ride.bookingId
      >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  riderId <-
    booking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    QRD.findById riderId
      >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  driverPhone <- getDriverPhone ride
  callStatusId <- generateGUID
  let callReq =
        Call.InitiateCallReq
          { fromPhoneNum = requestorPhone,
            toPhoneNum = driverPhone,
            attachments = Call.Attachments $ CallAttachments {callStatusId = callStatusId, rideId = rideId}
          }
  callResp <- Call.initiateCall booking.providerId callReq
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from driver to customer."
  callStatus <- buildCallStatus callStatusId callResp
  runTransaction $ QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId callResp = do
      now <- getCurrentTime
      return $
        DCS.CallStatus
          { id = callStatusId,
            callId = callResp.callId,
            rideId = rideId,
            status = callResp.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: (EsqDBFlow m r) => CallCallbackReq -> m CallCallbackRes
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

getCustomerMobileNumber :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> ExotelCallStatus -> m GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callStatus = do
  let callFrom = dropFirstZero callFrom_
  mobileNumberHash <- getDbHash callFrom
  driver <- runInReplica $ QPerson.findByMobileNumber "+91" mobileNumberHash >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
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
        DCS.CallStatus
          { id = callId,
            callId = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: (EsqDBReplicaFlow m r) => Id DCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> DCS.makeCallStatusAPIEntity

getDriverPhone :: (EsqDBReplicaFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  phonenum <- SP.getPersonNumber driver
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")
