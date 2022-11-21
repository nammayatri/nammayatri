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

import Beckn.External.Encryption (decrypt)
import Beckn.External.Exotel.Flow (initiateCall)
import Beckn.External.Exotel.Types
import qualified Beckn.External.Exotel.Types as Call
import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Text
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
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
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  exotelResponse <- initiateCall requestorPhone driverPhone callbackUrl attachments
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from driver to customer."
  let rId = strip (getId rideId)
  callStatus <- buildCallStatus (Id rId) callId exotelResponse
  runTransaction $ QCallStatus.create callStatus
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

callStatusCallback :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callId = Id req.customField.callId
  _ <- QCallStatus.findById callId >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus callId req.status req.conversationDuration req.recordingUrl
  return Ack

directCallStatusCallback :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
  return Ack

getCustomerMobileNumber :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Text -> m MobileNumberResp
getCustomerMobileNumber callSid callFrom_ _ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  driver <- QPerson.findByMobileNumber "+91" callFrom >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  activeBooking <- QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  riderId <-
    activeBooking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    QRD.findById riderId
      >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  callId <- generateGUID
  callStatusObj <- buildCallStatus activeRide.id callId callSid callStatus
  runTransaction $ QCallStatus.create callStatusObj
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

getCallStatus :: (EsqDBReplicaFlow m r) => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

getDriverPhone :: (EsqDBReplicaFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  phonenum <- SP.getPersonNumber driver
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")
