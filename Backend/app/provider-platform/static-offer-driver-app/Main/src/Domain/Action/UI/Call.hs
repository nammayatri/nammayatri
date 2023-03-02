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
    MobileNumberResp,
    initiateCallToCustomer,
    callStatusCallback,
    directCallStatusCallback,
    getCustomerMobileNumber,
    getCallStatus,
  )
where

import Data.Text
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.External.Exotel.Flow (initiateCall)
import Kernel.External.Exotel.Types
import qualified Kernel.External.Exotel.Types as Call
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import Tools.Metrics

newtype CallRes = CallRes
  { callId :: Id SCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallback

type CallCallbackRes = AckResponse

type MobileNumberResp = Text

type GetCallStatusRes = SCS.CallStatusAPIEntity

initiateCallToCustomer ::
  forall m r.
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r ["exotelCfg" ::: Maybe ExotelCfg, "selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToCustomer rideId = do
  ride <-
    QRide.findById rideId (Proxy @m)
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <-
    QRB.findById ride.bookingId (Proxy @m)
      >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  riderId <-
    booking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    QRD.findById riderId (Proxy @m)
      >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  driverPhone <- getDriverPhone ride
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  exotelResponse <- initiateCall requestorPhone driverPhone callbackUrl attachments
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from driver to customer."
  let rId = strip (getId rideId)
  callStatus <- buildCallStatus (Id rId) callId exotelResponse
  runTransaction $ QCallStatus.create @m callStatus
  return $ CallRes callId
  where
    buildCallbackUrl = do
      bppUIUrl <- asks (.selfUIUrl)
      let rideid = T.unpack (strip (getId rideId))
      return $
        bppUIUrl
          { baseUrlPath = baseUrlPath bppUIUrl <> "/driver/ride/" <> rideid <> "/call/statusCallback"
          }
    buildCallStatus rideid callId exoResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exoResponse.exoCall.exoSid.getExotelCallSID,
            rideId = rideid,
            status = exoResponse.exoCall.exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: forall m r. (EsqDBFlow m r) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callId = Id req.customField.callId
  _ <- QCallStatus.findById callId (Proxy @m) >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus @m callId req.status req.conversationDuration req.recordingUrl
  return Ack

directCallStatusCallback :: forall m r. EsqDBFlow m r => Text -> Text -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  callStatus <- QCallStatus.findByCallSid callSid (Proxy @m) >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus @m callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
  return Ack

getCustomerMobileNumber :: forall m r. (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Text -> m MobileNumberResp
getCustomerMobileNumber callSid callFrom_ _ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  mobileNumberHash <- getDbHash callFrom
  driver <- runInReplica $ QPerson.findByMobileNumber "+91" mobileNumberHash (Proxy @m) >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- runInReplica $ QRide.getActiveByDriverId driver.id (Proxy @m) >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  activeBooking <- runInReplica $ QRB.findById activeRide.bookingId (Proxy @m) >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  riderId <-
    activeBooking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica $
      QRD.findById riderId (Proxy @m)
        >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  callId <- generateGUID
  callStatusObj <- buildCallStatus activeRide.id callId callSid callStatus
  runTransaction $ QCallStatus.create @m callStatusObj
  return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: forall m r. (EsqDBReplicaFlow m r) => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId (Proxy @m) >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

getDriverPhone :: forall m r. (EsqDBReplicaFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- runInReplica $ QPerson.findById driverId (Proxy @m) >>= fromMaybeM (PersonNotFound driverId.getId)
  phonenum <- SP.getPersonNumber driver
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")
