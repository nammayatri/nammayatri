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
    MobileNumberResp,
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
import Data.Text.Conversions
import Domain.Types.Booking as DRB
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as DCS
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Kernel.External.Encryption
import Kernel.External.Exotel.Flow
import Kernel.External.Exotel.Types
import qualified Kernel.External.Exotel.Types as Call
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica, runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Client (BaseUrl (..))
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as Merchant
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics

newtype CallRes = CallRes
  { callId :: Id DCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallback

type CallCallbackRes = AckResponse

type MobileNumberResp = Text

type GetCallStatusRes = DCS.CallStatusAPIEntity

-- | Try to initiate a call customer -> driver
initiateCallToDriver ::
  forall m r.
  ( EncFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["exotelCfg" ::: Maybe ExotelCfg, "selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToDriver rideId = do
  (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  exotelResponse <- initiateCall customerPhone providerPhone callbackUrl attachments
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
  callStatus <- buildCallStatus rideId callId exotelResponse
  runTransaction $ QCallStatus.create @m callStatus
  return $ CallRes callId
  where
    buildCallbackUrl = do
      bapUIUrl <- asks (.selfUIUrl)
      let id = T.unpack (strip (getId rideId))
      return $
        bapUIUrl
          { baseUrlPath = baseUrlPath bapUIUrl <> "/ride/" <> id <> "/call/statusCallback"
          }
    buildCallStatus id callId exotelResponse = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            exotelCallSid = exotelResponse.exoCall.exoSid.getExotelCallSID,
            rideId = id,
            status = exotelResponse.exoCall.exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: forall m r. EsqDBFlow m r => CallCallbackReq -> m CallCallbackRes
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

getDriverMobileNumber :: forall m r. (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Text -> m MobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  merchant <- Merchant.findAllByExoPhone "+91" callTo >>= fromMaybeM (MerchantWithExoPhoneNotFound callTo)
  mobileNumberHash <- getDbHash callFrom
  person <-
    runInReplica (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash merchant.id (Proxy @m))
      >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  bookings <- runInReplica $ QRB.findByRiderIdAndStatus person.id [DRB.TRIP_ASSIGNED] (Proxy @m)
  booking <- fromMaybeM (BookingForRiderNotFound $ getId person.id) (listToMaybe bookings)
  ride <- runInReplica $ QRide.findActiveByRBId booking.id (Proxy @m) >>= fromMaybeM (RideWithBookingIdNotFound $ getId booking.id)
  callId <- generateGUID
  callStatusObj <- buildCallStatus ride.id callId callSid callStatus
  runTransaction $ QCallStatus.create @m callStatusObj
  return ride.driverMobileNumber
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            exotelCallSid = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: forall m r. EsqDBReplicaFlow m r => Id CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId (Proxy @m) >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: forall m r. (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  booking <- QRB.findById ride.bookingId (Proxy @m) >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let personId = booking.riderId
  Person.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- mapM decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: forall m r. (EncFlow m r, EsqDBFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId (Proxy @m)
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
