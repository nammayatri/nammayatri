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
import qualified Domain.Action.UI.CallEvent as DCE
import qualified Domain.Types.Booking as BT
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as DCS
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
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
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

type GetCallStatusRes = CallStatusAPIEntity

data CallStatusAPIEntity = CallStatusAPIEntity
  { callStatusId :: Id CallStatus,
    rideId :: Maybe (Id SRide.Ride),
    status :: CallTypes.CallStatus
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

makeCallStatusAPIEntity :: CallStatus -> CallStatusAPIEntity
makeCallStatusAPIEntity CallStatus {..} =
  CallStatusAPIEntity
    { callStatusId = id,
      ..
    }

-- | Try to initiate a call customer -> driver
initiateCallToDriver ::
  ( EncFlow m r,
    KvDbFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToDriver rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  (customerPhone, providerPhone) <- getCustomerAndDriverPhones ride booking
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
  callStatus <- buildCallStatus callStatusId exotelResponse booking
  QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse booking = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = Just rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just booking.merchantId.getId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now
          }

callStatusCallback :: KvDbFlow m r => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  _ <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  void $ QCallStatus.updateCallStatus req.conversationDuration (Just req.recordingUrl) (exotelStatusToInterfaceStatus req.status) callStatusId
  return Ack

directCallStatusCallback :: (KvDbFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r) => Text -> Call.ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuratioExotel callDurationFallback = do
  let callDuration = callDuratioExotel <|> callDurationFallback
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  let newCallStatus = exotelStatusToInterfaceStatus dialCallStatus
  _ <- case recordingUrl_ of
    Just recordUrl -> do
      if recordUrl == ""
        then do
          void $ updateCallStatus callDuration Nothing newCallStatus callStatus.id
          throwError CallStatusDoesNotExist
        else do
          updateCallStatus callDuration (Just recordUrl) newCallStatus callStatus.id
    Nothing -> do
      if newCallStatus == CallTypes.COMPLETED
        then do
          void $ updateCallStatus callDuration Nothing newCallStatus callStatus.id
          throwError CallStatusDoesNotExist
        else updateCallStatus callDuration Nothing newCallStatus callStatus.id
  DCE.sendCallDataToKafka (Just "EXOTEL") callStatus.rideId (Just "ANONYMOUS_CALLER") (Just callSid) (Just (show dialCallStatus)) System Nothing
  return Ack
  where
    updateCallStatus callDuration = QCallStatus.updateCallStatus (fromMaybe 0 callDuration)

getDriverMobileNumber :: (KvDbFlow m r, EsqDBReplicaFlow m r, EncFlow m r, EventStreamFlow m r) => Text -> Text -> Text -> Maybe Text -> Call.ExotelCallStatus -> Text -> m GetDriverMobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ _dtmfNumber callStatus to_ = do
  callId <- generateGUID
  callStatusObj <- buildCallStatus callId callSid (exotelStatusToInterfaceStatus callStatus)
  QCallStatus.create callStatusObj
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  let to = dropFirstZero to_
  exophone <-
    CQExophone.findByPhone to >>= \case
      Nothing -> CQExophone.findByPhone callTo >>= maybe (throwCallError callSid (ExophoneDoesNotExist callTo) Nothing Nothing) pure
      Just phone -> return phone
  merchantId <- CQMOC.findById exophone.merchantOperatingCityId >>= fmap (.merchantId) . maybe (throwCallError callSid (MerchantOperatingCityDoesNotExist exophone.merchantOperatingCityId.getId) (Just exophone.merchantId.getId) (Just exophone.callService)) pure
  mobileNumberHash <- getDbHash callFrom
  mbRiderDetails <-
    runInReplica (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash merchantId) >>= \case
      -- (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash exophone.merchantId) >>= \case
      Nothing -> pure Nothing
      Just person ->
        (QRB.findAssignedByRiderId person.id) >>= \case
          Nothing -> pure Nothing
          Just activeBooking -> return $ Just (Nothing, activeBooking)
  (resNumber, ride, callType, dtmfNumberUsed) <- do
    case mbRiderDetails of
      Just (dtmfNumberUsed, booking) -> do
        ride <- runInReplica $ QRide.findActiveByRBId booking.id >>= maybe (throwCallError callSid (RideWithBookingIdNotFound $ getId booking.id) (Just exophone.merchantId.getId) (Just exophone.callService)) pure
        return (ride.driverMobileNumber, ride, "ANONYMOUS_CALLER", dtmfNumberUsed)
      Nothing -> do
        mbRide <- runInReplica $ QRide.findLatestByDriverPhoneNumber callFrom
        case mbRide of
          Nothing -> do
            throwCallError callSid (PersonWithPhoneNotFound callFrom) (Just exophone.merchantId.getId) (Just exophone.callService)
          Just ride -> do
            booking <- runInReplica $ QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
            isValueAddNP <- CQVAN.isValueAddNP booking.providerId
            when isValueAddNP $ do
              throwCallError callSid (PersonWithPhoneNotFound callFrom) (Just exophone.merchantId.getId) (Just exophone.callService)
            rider <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
            decRider <- decrypt rider
            riderMobileNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
            return (riderMobileNumber, ride, "DRIVER", Nothing)
  QCallStatus.updateCallStatusInformation (Just merchantId.getId) (Just ride.id) (Just exophone.callService) dtmfNumberUsed callId
  DCE.sendCallDataToKafka (Just "EXOTEL") (Just ride.id) (Just callType) (Just callSid) Nothing System (Just to)
  return resNumber
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            callId = exotelCallId,
            rideId = Nothing,
            dtmfNumberUsed = Nothing,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Nothing,
            callService = Nothing,
            callError = Nothing,
            createdAt = now,
            updatedAt = now
          }

-- getDtmfFlow :: (KvDbFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Maybe Text -> Id Merchant -> Text -> Exophone -> m (Maybe (Maybe Text, BT.Booking))
-- getDtmfFlow dtmfNumber_ merchantId callSid exophone = do
--   number <- maybe (throwCallError callSid (PersonWithPhoneNotFound $ show dtmfNumber_) (Just exophone.merchantId.getId) (Just exophone.callService)) pure dtmfNumber_
--   let dtmfNumber = dropFirstZero $ removeQuotes number
--   dtmfMobileHash <- getDbHash dtmfNumber
--   person <- runInReplica $ Person.findByRoleAndMobileNumberAndMerchantId USER "+91" dtmfMobileHash merchantId >>= maybe (throwCallError callSid (PersonWithPhoneNotFound dtmfNumber) (Just exophone.merchantId.getId) (Just exophone.callService)) pure
--   booking <- runInReplica $ QRB.findAssignedByRiderId person.id >>= maybe (throwCallError callSid (BookingForRiderNotFound $ getId person.id) (Just exophone.merchantId.getId) (Just exophone.callService)) pure
--   return (Just dtmfNumber, booking)
--   where
--     dropFirstZero = T.dropWhile (== '0')
--     removeQuotes = T.replace "\"" ""

getCallStatus :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (KvDbFlow m r, EncFlow m r) => BT.Booking -> m Person
getPerson booking = do
  let personId = booking.riderId
  Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- mapM decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Returns phones pair
getCustomerAndDriverPhones :: (EncFlow m r, KvDbFlow m r) => SRide.Ride -> BT.Booking -> m (Text, Text)
getCustomerAndDriverPhones ride booking = do
  person <- getPerson booking
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)

throwCallError ::
  ( HasCallStack,
    KvDbFlow m r,
    IsBaseException e
  ) =>
  Text ->
  e ->
  Maybe Text ->
  Maybe Call.CallService ->
  m a
throwCallError callSid err merchantId callService = do
  let callSId :: Id DCS.CallStatus = Id callSid
  QCallStatus.updateCallError (Just (show err)) callService merchantId callSId
  throwError err
