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
    DriverNumberType (..),
    getCallTwillioAccessToken,
    getCallTwillioConnectedEntityTwiml,
    EntityType (..),
  )
where

import Data.Aeson (object, (.=))
import Data.Char as DC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text hiding (elem)
import qualified Data.Text as T
import qualified Data.Text.Lazy as DTL
import Data.Text.Lazy.Builder (fromString, toLazyText)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.CallEvent as DCE
import qualified Domain.Types.Booking as BT
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as DCS
import qualified Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment (Flow)
import EulerHS.Prelude (Alternative ((<|>)))
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Exotel.Types as Call
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import qualified Kernel.External.Call.Interface.Types as Call
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.External.Call.Twillio.Config
import qualified Kernel.External.Call.Types as CallTypes
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Common
import Kernel.Types.Id as ID
import Kernel.Types.Version (DeviceType (..))
import Kernel.Utils.Common
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event
import Servant (FromHttpApiData (..))
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.CallStatus as QCallStatus
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Text.XML as XML
import qualified Tools.Call as Call
import Tools.Error
import Web.JWT (Algorithm (HS256), ClaimsMap (..), JOSEHeader (..), JWTClaimsSet (..), encodeSigned, hmacSecret, numericDate, stringOrURI)

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

data DriverNumberType = AlternateNumber | PrimaryNumber | BothNumber
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

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
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasKafkaProducer r
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
            toPhoneNum = Just providerPhone,
            attachments = Call.Attachments $ CallAttachments {callStatusId = callStatusId, rideId = rideId},
            appletId = Nothing
          }
  let merchantOperatingCityId = booking.merchantOperatingCityId
  exotelResponse <- Call.initiateCall booking.merchantId merchantOperatingCityId callReq
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
  let dCallStatus = Just $ handleCallStatus exotelResponse.callStatus
  when (dCallStatus == Just DCallStatus.Failed) $ do
    merchant <- SMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
    void $ CallBPPInternal.callCustomerFCM merchant.driverOfferApiKey merchant.driverOfferBaseUrl (getId ride.bppRideId)
  callStatus <- buildCallStatus callStatusId exotelResponse dCallStatus booking
  QCallStatus.create callStatus
  return $ CallRes callStatusId
  where
    buildCallStatus callStatusId exotelResponse dCallStatus booking = do
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
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            callService = Just Call.Exotel,
            callError = Nothing,
            callAttempt = dCallStatus,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }

callStatusCallback :: (CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], EventStreamFlow m r) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callStatusId = req.customField.callStatusId
  callStatus <- QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist
  let interfaceStatus = exotelStatusToInterfaceStatus req.status
  let dCallStatus = Just $ handleCallStatus (interfaceStatus)
  isSendFCMSuccess <- sendFCMToDriverOnCallFailure interfaceStatus (Just req.customField.rideId) callStatus.merchantId
  let callAttemptStatus = if isSendFCMSuccess then Just DCallStatus.Resolved else dCallStatus
  QCallStatus.updateCallStatus req.conversationDuration (Just req.recordingUrl) interfaceStatus callAttemptStatus callStatusId
  return Ack

directCallStatusCallback :: (EsqDBFlow m r, EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Text -> Call.ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuratioExotel callDurationFallback = do
  let callDuration = callDuratioExotel <|> callDurationFallback
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  let newCallStatus = exotelStatusToInterfaceStatus dialCallStatus
  let dCallStatus = Just $ handleCallStatus newCallStatus
  isSendFCMSuccess <- sendFCMToDriverOnCallFailure newCallStatus callStatus.rideId callStatus.merchantId
  let callAttemptStatus = if isSendFCMSuccess then Just DCallStatus.Resolved else dCallStatus
  _ <- case recordingUrl_ of
    Just recordUrl -> do
      if recordUrl == ""
        then do
          updateCallStatus callStatus.id newCallStatus Nothing callAttemptStatus callDuration
          throwError CallStatusDoesNotExist
        else do
          updateCallStatus callStatus.id newCallStatus (Just recordUrl) callAttemptStatus callDuration
    Nothing -> do
      if newCallStatus == CallTypes.COMPLETED
        then do
          updateCallStatus callStatus.id newCallStatus Nothing callAttemptStatus callDuration
          throwError CallStatusDoesNotExist
        else updateCallStatus callStatus.id newCallStatus Nothing callAttemptStatus callDuration
  DCE.sendCallDataToKafka (Just "EXOTEL") callStatus.rideId (Just "ANONYMOUS_CALLER") (Just callSid) (Just (show dialCallStatus)) System Nothing
  return Ack
  where
    updateCallStatus id callStatus url callAttemptStatus callDuration = QCallStatus.updateCallStatus (fromMaybe 0 callDuration) url callStatus callAttemptStatus id

getDriverMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, EventStreamFlow m r, HasField "maxShards" r Int, HasField "schedulerSetName" r Text, HasField "schedulerType" r SchedulerType, HasField "jobInfoMap" r (M.Map Text Bool)) => DriverNumberType -> Text -> Text -> Text -> Maybe Text -> Call.ExotelCallStatus -> Text -> m GetDriverMobileNumberResp
getDriverMobileNumber driverNumberType callSid callFrom_ callTo_ _dtmfNumber callStatus to_ = do
  id <- generateGUID

  let callFrom = dropFirstZero callFrom_
      callTo = dropFirstZero callTo_
      to = dropFirstZero to_

  exophone <-
    CQExophone.findByPhone to >>= \case
      Nothing -> CQExophone.findByPhone callTo >>= fromMaybeM (ExophoneDoesNotExist callTo)
      Just phone -> return phone
  merchantId <- CQMOC.findById exophone.merchantOperatingCityId >>= fmap (.merchantId) . fromMaybeM (MerchantOperatingCityDoesNotExist exophone.merchantOperatingCityId.getId)

  mobileNumberHash <- getDbHash callFrom
  mbRiderDetails <- findRiderDetails merchantId mobileNumberHash
  let driverNumberType' = if driverNumberType == BothNumber then (if exophone.callService == CallTypes.Exotel && exophone.enableAlternateNumber == Just True then BothNumber else PrimaryNumber) else driverNumberType

  (resNumber, ride, callType, dtmfNumberUsed) <-
    case mbRiderDetails of
      Just (dtmfNumberUsed, booking) -> do
        ride <- runInReplica $ QRide.findActiveByRBId booking.id >>= fromMaybeM (RideWithBookingIdNotFound $ getId booking.id)
        ensureCallStatusExists id callSid ride.id callStatus booking.merchantOperatingCityId booking.merchantId
        decRide <- decrypt ride
        number <- getNumberBasedOnType driverNumberType' decRide
        return (number, ride, "ANONYMOUS_CALLER", dtmfNumberUsed)
      Nothing -> do
        ride <- runInReplica $ QRide.findLatestByDriverPhoneNumber callFrom >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
        booking <- runInReplica $ QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
        ensureCallStatusExists id callSid ride.id callStatus booking.merchantOperatingCityId booking.merchantId
        isValueAddNP <- CQVAN.isValueAddNP booking.providerId
        when isValueAddNP $
          throwCallError id (PersonWithPhoneNotFound callFrom) (Just exophone.merchantId.getId) (Just exophone.callService)
        rider <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
        decRider <- decrypt rider
        riderMobileNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
        return (riderMobileNumber, ride, "DRIVER", Nothing)

  QCallStatus.updateCallStatusInformation (Just merchantId.getId) (Just exophone.callService) dtmfNumberUsed id
  DCE.sendCallDataToKafka (Just "EXOTEL") (Just ride.id) (Just callType) (Just callSid) Nothing System (Just to)

  return resNumber
  where
    dropFirstZero = T.dropWhile (== '0')

    findRiderDetails merchantId mobileNumberHash =
      runInReplica (Person.findByRoleAndMobileNumberAndMerchantId USER "+91" mobileNumberHash merchantId) >>= \case
        Nothing -> pure Nothing
        Just person -> do
          activeBooking <-
            QB.findAssignedByRiderId person.id >>= \case
              bk@(Just _) -> pure bk
              Nothing ->
                QBPL.findOneActivePartyByRiderId person.id >>= \case
                  Nothing -> pure Nothing
                  Just party -> QRB.findById party.bookingId
          case activeBooking of
            Nothing -> pure Nothing
            Just activeBooking_ -> return $ Just (Nothing, activeBooking_)

    ensureCallStatusExists id callId rideId callStatus' merchantOperatingCityId merchantId =
      QCallStatus.findOneByRideId (Just $ getId rideId) >>= \case
        Nothing -> do
          callStatusObj <- buildCallStatus id callId (Just rideId) (exotelStatusToInterfaceStatus callStatus') merchantOperatingCityId merchantId
          void $ QCallStatus.create callStatusObj
        Just cs -> QCallStatus.updateCallStatusCallId callId cs.id

    buildCallStatus id exotelCallId rideId exoStatus merchantOperatingCityId merchantId = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = id,
            callId = exotelCallId,
            rideId = rideId,
            dtmfNumberUsed = Nothing,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just merchantId.getId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            callService = Nothing,
            customerIvrResponse = Nothing,
            callAttempt = Just Pending,
            callError = Nothing,
            createdAt = now,
            updatedAt = now
          }

    getNumberBasedOnType numberType ride =
      case numberType of
        PrimaryNumber -> return $ fromMaybe ride.driverMobileNumber ride.driverPhoneNumber
        AlternateNumber ->
          maybe (throwError $ RideFieldNotPresent "driverAlternateNumber") pure ride.driverAlternateNumber
        BothNumber -> do
          let primaryNumber = fromMaybe ride.driverMobileNumber ride.driverPhoneNumber
          let alternateNumber = maybe "" ("," <>) ride.driverAlternateNumber
          return $ primaryNumber <> alternateNumber

-- getDtmfFlow :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Maybe Text -> Id Merchant -> Text -> Exophone -> m (Maybe (Maybe Text, BT.Booking))
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

getCallStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  runInReplica $ QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (EsqDBFlow m r, CacheFlow m r, EncFlow m r) => BT.Booking -> m Person
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
getCustomerAndDriverPhones :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => SRide.Ride -> BT.Booking -> m (Text, Text)
getCustomerAndDriverPhones ride booking = do
  person <- getPerson booking
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)

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
  Maybe Call.CallService ->
  m a
throwCallError callStatusId err merchantId callService = do
  QCallStatus.updateCallError (Just (show err)) callService merchantId callStatusId
  throwError err

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

sendFCMToDriverOnCallFailure ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r
  ) =>
  CallTypes.CallStatus ->
  Maybe (Id SRide.Ride) ->
  Maybe Text ->
  m Bool
sendFCMToDriverOnCallFailure callStatus mbRideId mbMerchantId =
  if isFailureStatus callStatus
    then do
      deploymentVersion <- asks (.version)
      rideId <- fromMaybeM (CallStatusFieldNotPresent "rideId") mbRideId
      ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
      booking <- runInReplica $ QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      merchantId <- fromMaybeM (RideFieldNotPresent "merchantId") mbMerchantId
      merchant <- runInReplica $ SMerchant.findById (ID.Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
      fork "updating in prometheus" $ incrementCounter (getId booking.merchantOperatingCityId) "call_attempt" deploymentVersion.getDeploymentVersion
      void $ CallBPPInternal.callCustomerFCM merchant.driverOfferApiKey merchant.driverOfferBaseUrl (getId ride.bppRideId)
      return True
    else return False
  where
    isFailureStatus :: CallTypes.CallStatus -> Bool
    isFailureStatus status = status `elem` [CallTypes.INVALID_STATUS, CallTypes.NOT_CONNECTED, CallTypes.FAILED]

data EntityType = CUSTOMER | DRIVER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

data Response = Response
  { say :: Maybe String,
    dial :: Maybe Dial
  }
  deriving stock (Generic, Show)

newtype Dial = Dial
  { client :: Maybe String
  }
  deriving stock (Generic, Show)

instance FromHttpApiData EntityType where
  parseUrlPiece txt = case T.toLower txt of
    "customer" -> Right Domain.Action.UI.Call.CUSTOMER
    "driver" -> Right Domain.Action.UI.Call.DRIVER
    _ -> Left "Invalid EntityType"

getCallTwillioAccessToken ::
  ( Id SRide.Ride ->
    EntityType ->
    DeviceType ->
    Flow Text
  )
getCallTwillioAccessToken rideId entity deviceType = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  entityId <- case entity of
    Domain.Action.UI.Call.CUSTOMER -> do
      return booking.transactionId
    Domain.Action.UI.Call.DRIVER -> do
      return ride.bppRideId.getId
  getAccessToken entityId (fromMaybe booking.merchantOperatingCityId ride.merchantOperatingCityId) (fromMaybe booking.merchantId ride.merchantId)
  where
    getAccessToken id cityId merId = do
      twillioCallConfig :: TwillioCallCfg <- getCfg cityId merId
      createJWT id twillioCallConfig deviceType
    getCfg cityId mercId = do
      merchantServConfig <- QMSC.findByMerchantOpCityIdAndService mercId cityId (DMSC.CallService Call.TwillioCall) >>= fromMaybeM (MerchantServiceConfigNotFound cityId.getId "Call" "TwillioCall")
      case merchantServConfig.serviceConfig of
        DMSC.CallServiceConfig config ->
          case config of
            Call.TwillioCallConfig cfg -> return cfg
            _ -> throwError $ InternalError "Twillio configs not foung"
        _ -> throwError $ InternalError "Twillio configs not foung"

getCallTwillioConnectedEntityTwiml ::
  ( Id SRide.Ride ->
    EntityType ->
    Flow Text
  )
getCallTwillioConnectedEntityTwiml rideId entity = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let say = Just "Please wait while we are making the call"
  dial <- case entity of
    Domain.Action.UI.Call.CUSTOMER -> do
      let dial = Dial (Just (T.unpack ride.bppRideId.getId))
      return $ Just dial
    Domain.Action.UI.Call.DRIVER -> do
      let dial = Dial (Just (T.unpack booking.transactionId))
      return $ Just dial
  let twimlResp = responseToXML (Response say dial)
  logDebug $ DTL.toStrict $ renderText def twimlResp
  return $ DTL.toStrict $ renderText def twimlResp

createJWT :: Text -> TwillioCallCfg -> DeviceType -> Flow Text
createJWT id cfg deviceType = do
  decApiKey <- decrypt cfg.apiKey
  decApiKeySecret <- decrypt cfg.apiKeySecret
  let applicationSid = cfg.applicationSid
      accountSid = cfg.accountSid
      pushCredentialSid = if deviceType == IOS then cfg.pushCredentialSidIos else cfg.pushCredentialSidAndroid
  now <- getCurrentTime
  let grants =
        object
          [ "identity" .= id,
            "voice"
              .= object
                [ "incoming" .= object ["allow" .= True],
                  "outgoing" .= object ["application_sid" .= (applicationSid :: Text)],
                  "push_credential_sid" .= (pushCredentialSid :: Text)
                ]
          ]
  let jtiText = decApiKey <> "-" <> posixTimeToText (utcTimeToPOSIXSeconds now)
  let claims =
        JWTClaimsSet
          { iss = stringOrURI decApiKey,
            sub = stringOrURI accountSid,
            iat = numericDate $ utcTimeToPOSIXSeconds now,
            exp = numericDate $ utcTimeToPOSIXSeconds $ addUTCTime (secondsToNominalDiffTime 1800) now,
            jti = stringOrURI jtiText,
            unregisteredClaims = ClaimsMap (M.fromList [("grants", grants)]),
            aud = Nothing,
            nbf = Nothing
          }
  let header =
        JOSEHeader
          { typ = Just "JWT",
            alg = Just HS256,
            cty = Just "twilio-fpa;v=1",
            kid = Nothing
          }
  let key = hmacSecret decApiKeySecret
  let jwt = encodeSigned key header claims
  return jwt
  where
    posixTimeToText = DTL.toStrict . toLazyText . Data.Text.Lazy.Builder.fromString . show . (round :: POSIXTime -> Int)

capitalizeText :: T.Text -> T.Text
capitalizeText txt =
  case T.uncons txt of
    Nothing -> txt
    Just (x, xs) -> T.cons (DC.toUpper x) xs

mkElement :: T.Text -> Maybe String -> Maybe XML.Node
mkElement tagName = fmap (\tval -> NodeElement $ XML.Element (Name (capitalizeText tagName) Nothing Nothing) mempty [NodeContent (T.pack tval)])

-- Function to convert a Dial data type to XML elements
dialToXML :: Dial -> XML.Element
dialToXML Dial {..} = XML.Element (Name "Dial" Nothing Nothing) mempty elements
  where
    elements =
      catMaybes
        [ mkElement "client" client
        ]

-- Function to convert a Response data type to an XML Document
responseToXML :: Response -> Document
responseToXML Response {..} =
  Document (Prologue [] Nothing []) root []
  where
    elements =
      catMaybes
        [ mkElement "say" say,
          NodeElement <$> fmap dialToXML dial
        ]
    root = XML.Element (Name "Response" Nothing Nothing) mempty elements
