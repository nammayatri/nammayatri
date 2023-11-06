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
import qualified Domain.Types.Booking as BT
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as DCS
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude (Alternative ((<|>)))
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Exotel.Types as Call
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import qualified Kernel.External.Call.Interface.Types as Call
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Call as Call
import Tools.Error

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
  let merchantOperatingCityId = booking.merchantOperatingCityId
  exotelResponse <- Call.initiateCall booking.merchantId merchantOperatingCityId callReq
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
  callStatus <- buildCallStatus callStatusId exotelResponse
  QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: (CacheFlow m r, EsqDBFlow m r) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  _ <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  void $ QCallStatus.updateCallStatus callStatusId (exotelStatusToInterfaceStatus req.status) req.conversationDuration (Just req.recordingUrl)
  return Ack

directCallStatusCallback :: (CacheFlow m r, EsqDBFlow m r) => Text -> Call.ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuratioExotel callDurationFallback = do
  let callDuration = callDuratioExotel <|> callDurationFallback
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  let newCallStatus = exotelStatusToInterfaceStatus dialCallStatus
  _ <- case recordingUrl_ of
    Just recordUrl -> do
      if recordUrl == ""
        then do
          void $ updateCallStatus callStatus.id newCallStatus Nothing callDuration
          throwError CallStatusDoesNotExist
        else do
          updateCallStatus callStatus.id newCallStatus (Just recordUrl) callDuration
    Nothing -> do
      if newCallStatus == CallTypes.COMPLETED
        then do
          void $ updateCallStatus callStatus.id newCallStatus Nothing callDuration
          throwError CallStatusDoesNotExist
        else updateCallStatus callStatus.id newCallStatus Nothing callDuration
  return Ack
  where
    updateCallStatus id callStatus url callDuration = QCallStatus.updateCallStatus id callStatus (fromMaybe 0 callDuration) url

getDriverMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Maybe Text -> Call.ExotelCallStatus -> Text -> m GetDriverMobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ dtmfNumber_ callStatus to_ = do
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  let to = dropFirstZero to_
  exophone <-
    CQExophone.findByPhone to >>= \case
      Nothing -> CQExophone.findByPhone callTo >>= fromMaybeM (ExophoneDoesNotExist callTo)
      Just phone -> return phone
  merchantId <- CQMOC.findById exophone.merchantOperatingCityId >>= fmap (.merchantId) . fromMaybeM (MerchantOperatingCityDoesNotExist exophone.merchantOperatingCityId.getId)
  mobileNumberHash <- getDbHash callFrom
  (dtmfNumberUsed, booking) <-
    runInReplica (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash merchantId) >>= \case
      -- (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash exophone.merchantId) >>= \case
      Nothing -> getDtmfFlow dtmfNumber_ merchantId
      Just person ->
        (QRB.findAssignedByRiderId person.id) >>= \case
          Nothing -> getDtmfFlow dtmfNumber_ merchantId
          Just activeBooking -> return (Nothing, activeBooking)
  ride <- runInReplica $ QRide.findActiveByRBId booking.id >>= fromMaybeM (RideWithBookingIdNotFound $ getId booking.id)
  callId <- generateGUID
  callStatusObj <- buildCallStatus ride.id callId callSid (exotelStatusToInterfaceStatus callStatus) dtmfNumberUsed
  QCallStatus.create callStatusObj
  return ride.driverMobileNumber
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus dtmfNumberUsed = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            callId = exotelCallId,
            rideId = rideId,
            dtmfNumberUsed = dtmfNumberUsed,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getDtmfFlow :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Maybe Text -> Id Merchant -> m (Maybe Text, BT.Booking)
getDtmfFlow dtmfNumber_ merchantId = do
  number <- fromMaybeM (InvalidRequest "DTMF Number Not Found") dtmfNumber_
  let dtmfNumber = dropFirstZero $ removeQuotes number
  dtmfMobileHash <- getDbHash dtmfNumber
  person <- runInReplica $ Person.findByRoleAndMobileNumberAndMerchantId USER "+91" dtmfMobileHash merchantId >>= fromMaybeM (PersonWithPhoneNotFound dtmfNumber)
  booking <- runInReplica $ QRB.findAssignedByRiderId person.id >>= fromMaybeM (BookingForRiderNotFound $ getId person.id)
  return (Just dtmfNumber, booking)
  where
    dropFirstZero = T.dropWhile (== '0')
    removeQuotes = T.replace "\"" ""

getCallStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (EsqDBFlow m r, CacheFlow m r, EncFlow m r) => SRide.Ride -> m Person
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
getCustomerAndDriverPhones :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
