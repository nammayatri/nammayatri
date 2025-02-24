{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module SharedLogic.CallBAP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
    sendDriverArrivalUpdateToBAP,
    sendStopArrivalUpdateToBAP,
    sendDestinationArrivalUpdateToBAP,
    sendEstimateRepetitionUpdateToBAP,
    sendRideEstimatedEndTimeRangeUpdateToBAP,
    sendQuoteRepetitionUpdateToBAP,
    sendTollCrossedUpdateToBAP,
    sendUpdateEditDestToBAP,
    sendUpdateEditDestErrToBAP,
    sendNewMessageToBAP,
    sendDriverOffer,
    callOnConfirmV2,
    callOnStatusV2,
    buildBppUrl,
    sendSafetyAlertToBAP,
    sendPhoneCallRequestUpdateToBAP,
    sendPhoneCallCompletedUpdateToBAP,
    mkTxnIdKey,
    sendOnConfirmToBAP,
    notfyDeliveryImageUploadedToBAP,
  )
where

import qualified AWS.S3 as S3
import qualified Beckn.ACL.OnCancel as ACL
import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.OnDemand.Transformer.OnUpdate as TFOU
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnCancel as API
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnStatus as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Aeson as A
import Data.Default.Class
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as DL
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time hiding (getCurrentTime)
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.DocumentVerificationConfig as DIT
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.DriverStats as DDriverStats
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.OnUpdate as DOU
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRFD
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified EulerHS.Types as Euler
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth
import Network.URI (parseURI, uriQuery)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import Storage.Beam.IssueManagement ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.ValueAddNP as CValueAddNP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.IdfyVerification as QIV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRD
import Storage.Queries.RiderDriverCorrelation as SQR
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify
import TransactionLogs.PushLogs
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

callOnSelectV2 ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DSRFD.SearchRequestForDriver ->
  DST.SearchTry ->
  Spec.OnSelectReqMessage ->
  m ()
callOnSelectV2 transporter searchRequest srfd searchTry content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
      bppSubscriberId = getShortId $ transporter.subscriberId
  bppUri <- buildBppUrl (transporter.id)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)

  msgId <- getMsgIdByTxnId searchRequest.transactionId
  let vehicleCategory = Utils.mapServiceTierToCategory srfd.vehicleServiceTier
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle transporter.id "MOBILITY" vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- bppConfig.onSelectTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.ON_SELECT Context.MOBILITY msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry) (Just ttl)
  logDebug $ "on_selectV2 request bpp: " <> show content
  void $ withShortRetry $ callBecknAPIWithSignature' transporter.id bppSubscriberId (show Context.ON_SELECT) API.onSelectAPIV2 bapUri internalEndPointHashMap (Spec.OnSelectReq context Nothing (Just content))
  where
    getMsgIdByTxnId :: CacheFlow m r => Text -> m Text
    getMsgIdByTxnId txnId = do
      Hedis.safeGet (mkTxnIdKey txnId) >>= \case
        Nothing -> pure (fromMaybe searchTry.estimateId srfd.estimateId)
        Just a -> pure a

mkTxnIdKey :: Text -> Text
mkTxnIdKey txnId = "driver-offer:CachedQueries:Select:transactionId-" <> txnId

callOnUpdateV2 ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    HasHttpClientOptions r c,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig]
  ) =>
  Spec.OnUpdateReq ->
  RetryCfg ->
  Id Merchant.Merchant ->
  m ()
callOnUpdateV2 req retryConfig merchantId = do
  bapUri' <- req.onUpdateReqContext.contextBapUri & fromMaybeM (InternalError "BAP URI is not present in Ride Assigned request context.")
  bapUri <- parseBaseUrl bapUri'
  bppSubscriberId <- req.onUpdateReqContext.contextBppId & fromMaybeM (InternalError "BPP ID is not present in Ride Assigned request context.")
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ withRetryConfig retryConfig $ callBecknAPIWithSignature' merchantId bppSubscriberId (show Context.ON_UPDATE) API.onUpdateAPIV2 bapUri internalEndPointHashMap req

callOnStatusV2 ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    HasHttpClientOptions r c,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig]
  ) =>
  Spec.OnStatusReq ->
  RetryCfg ->
  Id Merchant.Merchant ->
  m ()
callOnStatusV2 req retryConfig merchantId = do
  bapUri' <- req.onStatusReqContext.contextBapUri & fromMaybeM (InternalError "BAP URI is not present in Ride Assigned request context.")
  bapUri <- parseBaseUrl bapUri'
  bppSubscriberId <- req.onStatusReqContext.contextBppId & fromMaybeM (InternalError "BPP ID is not present in Ride Assigned request context.")
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ withRetryConfig retryConfig $ callBecknAPIWithSignature' merchantId bppSubscriberId (show Context.ON_STATUS) API.onStatusAPIV2 bapUri internalEndPointHashMap req

callOnCancelV2 ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    HasHttpClientOptions r c,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig]
  ) =>
  Spec.OnCancelReq ->
  RetryCfg ->
  Id Merchant.Merchant ->
  m ()
callOnCancelV2 req retryConfig merchantId = do
  bapUri' <- req.onCancelReqContext.contextBapUri & fromMaybeM (InternalError "BAP URI is not present in Ride Assigned request context.")
  bapUri <- parseBaseUrl bapUri'
  bppSubscriberId <- req.onCancelReqContext.contextBppId & fromMaybeM (InternalError "BPP ID is not present in Ride Assigned request context.")
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ withRetryConfig retryConfig $ callBecknAPIWithSignature' merchantId bppSubscriberId (show Context.ON_CANCEL) API.onCancelAPIV2 bapUri internalEndPointHashMap req

callOnConfirmV2 ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DM.Merchant ->
  Spec.Context ->
  Spec.ConfirmReqMessage ->
  DBC.BecknConfig ->
  m ()
callOnConfirmV2 transporter context content bppConfig = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
  bapUri <- Utils.getContextBapUri context
  bapId <- Utils.getContextBapId context
  msgId <- Utils.getMessageId context
  city <- Utils.getContextCity context
  country <- Utils.getContextCountry context
  bppUri <- buildBppUrl transporter.id
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  txnId <- Utils.getTransactionId context
  ttl <- bppConfig.onConfirmTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context_ <- ContextV2.buildContextV2 Context.ON_CONFIRM Context.MOBILITY msgId (Just txnId) bapId bapUri (Just bppSubscriberId) (Just bppUri) city country (Just ttl)
  void $ withShortRetry $ callBecknAPIWithSignature' transporter.id bppSubscriberId (show Context.ON_CONFIRM) API.onConfirmAPIV2 bapUri internalEndPointHashMap (Spec.OnConfirmReq {onConfirmReqContext = context_, onConfirmReqError = Nothing, onConfirmReqMessage = Just content})

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id DM.Merchant ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

getQueryParam :: String -> String -> Maybe String
getQueryParam paramName url = do
  uri <- parseURI url
  let query = uriQuery uri
  let params = parseQueryParams query
  DL.lookup paramName params

parseQueryParams :: String -> [(String, String)]
parseQueryParams qs = DL.map parseParam (DL.filter (/= "") (splitOn '&' (DL.drop 1 qs)))
  where
    parseParam p = let (k, v) = DL.break (== '=') p in (k, DL.drop 1 v)

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delim str =
  let (firstV, remainder) = DL.break (== delim) str
   in firstV : case remainder of
        [] -> []
        _ -> splitOn delim (DL.tail remainder)

rideAssignedCommon ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  m DOU.OnUpdateBuildReq
rideAssignedCommon booking ride driver veh = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  let estimateId = booking.estimateId <&> getId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  mbTransporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) -- these two lines just for backfilling driver vehicleModel from idfy TODO: remove later
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
  let paymentUrl = Nothing
  vehicle <-
    case mbTransporterConfig of
      Just transporterConfig ->
        if transporterConfig.refillVehicleModel
          then do
            reffiledVeh <- refillVehicleModel
            pure $
              case reffiledVeh of
                Right reffiledVeh' -> reffiledVeh'
                Left _ -> veh
          else pure veh
      Nothing -> pure veh
  riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
  riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
  rideDetails <- runInReplica $ QRideDetails.findById ride.id >>= fromMaybeM (RideNotFound ride.id.getId)
  let bookingDetails = ACL.BookingDetails {..}
  -- resp <- try @_ @SomeException (fetchAndCacheAadhaarImage driver driverInfo)
  image <- forM driver.faceImageId $ \mediaId -> do
    mediaEntry <- runInReplica $ MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist ("Driver image does not exist for ride:" <> driver.id.getId))
    imagePath <- fromMaybeM (FileDoNotExist ("Driver image does not exist for ride:" <> driver.id.getId)) (getQueryParam "filePath" (Text.unpack mediaEntry.url))
    pure $ Text.pack imagePath

  -- let image = join (eitherToMaybe resp)
  isDriverBirthDay <- maybe (return False) (checkIsDriverBirthDay mbTransporterConfig) driverInfo.driverDob
  let isFreeRide = False
  isAlreadyFav <- do
    isAlreadyFav' <- SQR.checkRiderFavDriver (fromMaybe "" booking.riderId) ride.driverId True
    case isAlreadyFav' of
      Just _ -> pure True
      Nothing -> pure False
  let favCount = driverStats.favRiderCount
  driverAccountId <-
    if ride.onlinePayment && isValueAddNP
      then do
        mDriverBankAccount <- runInReplica $ QDBA.findByPrimaryKey ride.driverId
        return $ (.accountId) <$> mDriverBankAccount
      else pure Nothing
  pure $ (if ride.status == SRide.UPCOMING then ACL.ScheduledRideAssignedBuildReq else ACL.RideAssignedBuildReq) ACL.DRideAssignedReq {vehicleAge = rideDetails.vehicleAge, ..}
  where
    refillKey = "REFILLED_" <> ride.driverId.getId
    updateVehicle DVeh.Vehicle {..} newModel = DVeh.Vehicle {model = newModel, ..}
    refillVehicleModel = try @_ @SomeException do
      -- TODO: remove later
      mbIsRefilledToday :: Maybe Bool <- Hedis.get refillKey
      case mbIsRefilledToday of
        Just True -> Hedis.expire refillKey 86400 $> veh
        _ -> do
          driverVehicleIdfyResponse <-
            find
              ( \a ->
                  maybe False ((==) veh.registrationNo) $
                    (.registration_number)
                      =<< (.extraction_output)
                      =<< (.result)
                      =<< (((A.decode . TLE.encodeUtf8 . TL.fromStrict) =<< (a.idfyResponse)) :: Maybe Idfy.VerificationResponse)
              )
              <$> QIV.findAllByDriverIdAndDocType ride.driverId DIT.VehicleRegistrationCertificate
          newVehicle <-
            (flip $ maybe (pure veh)) ((.manufacturer_model) =<< (.extraction_output) =<< (.result) =<< (((A.decode . TLE.encodeUtf8 . TL.fromStrict) =<< (.idfyResponse) =<< driverVehicleIdfyResponse) :: Maybe Idfy.VerificationResponse)) $ \newModel -> do
              modelNamesHashMap <- asks (.modelNamesHashMap)
              let modelValueToUpdate = fromMaybe "" $ HMS.lookup newModel modelNamesHashMap
              if modelValueToUpdate == veh.model
                then pure veh
                else QVeh.updateVehicleModel modelValueToUpdate ride.driverId $> updateVehicle veh modelValueToUpdate
          Hedis.setExp refillKey True 86400
          pure newVehicle

    checkIsDriverBirthDay mbTransporterConfig driverBirthDate = do
      case mbTransporterConfig of
        Just tc -> do
          let (_, birthMonth, birthDay) = toGregorian $ utctDay driverBirthDate
          currentLocalTime <- getLocalCurrentTime tc.timeDiffFromUtc
          let (_, curMonth, curDay) = toGregorian $ utctDay currentLocalTime
          return (birthMonth == curMonth && birthDay == curDay)
        Nothing -> return False

buildOnConfirmMessage ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  m Spec.ConfirmReqMessage
buildOnConfirmMessage booking ride driver veh = do
  rideAssignedBuildReq <- rideAssignedCommon booking ride driver veh
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle booking.providerId "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  farePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  onConfirmMessage <- TFOU.mkOnUpdateMessageV2 rideAssignedBuildReq farePolicy becknConfig
  let generatedMsg = A.encode onConfirmMessage
  logDebug $ "ride assigned on_confirm request bppv2: " <> T.pack (show generatedMsg)
  pure . fromJust $ onConfirmMessage

sendOnConfirmToBAP ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  DM.Merchant ->
  Spec.Context ->
  m ()
sendOnConfirmToBAP booking ride driver veh transporter context = do
  transactionId <- Utils.getTransactionId context
  let bppId = context.contextBppId
      txnId = Just transactionId
  bapId <- Utils.getContextBapId context
  callbackUrl <- Utils.getContextBapUri context
  bppUri <- Utils.getContextBppUri context
  msgId <- Utils.getMessageId context
  city <- Utils.getContextCity context
  country <- Utils.getContextCountry context
  context' <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country (Just "PT2M")
  let vehicleCategory = Utils.mapServiceTierToCategory booking.vehicleServiceTier
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle transporter.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
  onConfirmMessage <- buildOnConfirmMessage booking ride driver veh
  void $ callOnConfirmV2 transporter context' onConfirmMessage becknConfig

sendRideAssignedUpdateToBAP ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  Bool ->
  m ()
sendRideAssignedUpdateToBAP booking ride driver veh isScheduledRideAssignment = do
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  retryConfig <- asks (.shortDurationRetryCfg)
  rideAssignedBuildReq <- rideAssignedCommon booking ride driver veh
  rideAssignedMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing rideAssignedBuildReq
  let generatedMsg = A.encode rideAssignedMsgV2
  logDebug $ "ride assigned on_update request bppv2: " <> T.pack (show generatedMsg)
  when isScheduledRideAssignment $ Notify.notifyDriverWithProviders booking.merchantOperatingCityId notificationType notificationTitle (message booking) driver driver.deviceToken EmptyDynamicParam
  void $ callOnUpdateV2 rideAssignedMsgV2 retryConfig merchant.id
  where
    notificationType = Notification.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message uBooking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst uBooking.startTime) <> ".",
            "Check the app for more details."
          ]

sendRideStartedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Maybe Maps.LatLong ->
  m ()
sendRideStartedUpdateToBAP booking ride tripStartLocation = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
  riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
      paymentUrl = Nothing
      bookingDetails = ACL.BookingDetails {..}
      estimateId = booking.estimateId <&> (.getId)
      rideStartedBuildReq = ACL.RideStartedReq ACL.DRideStartedReq {..}
  retryConfig <- asks (.longDurationRetryCfg)
  rideStartedMsgV2 <- ACL.buildOnStatusReqV2 merchant booking rideStartedBuildReq Nothing
  void $ callOnStatusV2 rideStartedMsgV2 retryConfig merchant.id

sendRideEstimatedEndTimeRangeUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideEstimatedEndTimeRangeUpdateToBAP booking ride = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
  riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
      paymentUrl = Nothing
      bookingDetails = ACL.BookingDetails {..}
      endEstimateTimeRangeBuildReq = DOU.RideEstimatedEndTimeRangeBuildReq DOU.DRideEstimatedEndTimeRangeReq {..}
  retryConfig <- asks (.longDurationRetryCfg)
  endEstimateTimeRangeMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing endEstimateTimeRangeBuildReq
  void $ callOnUpdateV2 endEstimateTimeRangeMsgV2 retryConfig merchant.id

sendRideCompletedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  Maybe DMPM.PaymentMethodInfo ->
  Maybe Text ->
  Maybe Maps.LatLong ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams paymentMethodInfo paymentUrl tripEndLocation = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
  riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
  let bookingDetails = ACL.BookingDetails {..}
      estimateId = booking.estimateId <&> (.getId)
      rideCompletedBuildReq = ACL.RideCompletedBuildReq ACL.DRideCompletedReq {..}
  retryConfig <- asks (.longDurationRetryCfg)
  rideCompletedMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing rideCompletedBuildReq
  void $ callOnUpdateV2 rideCompletedMsgV2 retryConfig merchant.id

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  DM.Merchant ->
  SRBCR.CancellationSource ->
  Maybe PriceAPIEntity ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource cancellationFee = do
  let bookingCancelledBuildReqV2 = ACL.BookingCancelledBuildReqV2 ACL.DBookingCancelledReqV2 {..}
  retryConfig <- asks (.longDurationRetryCfg)
  bookingCancelledMsgV2 <- ACL.buildOnCancelMessageV2 transporter booking.bapCity booking.bapCountry (show Enums.CANCELLED) bookingCancelledBuildReqV2 Nothing
  void $ callOnCancelV2 bookingCancelledMsgV2 retryConfig transporter.id

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasPrettyLogger m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DSRFD.SearchRequestForDriver ->
  DST.SearchTry ->
  DDQ.DriverQuote ->
  m ()
sendDriverOffer transporter searchReq srfd searchTry driverQuote = do
  logDebug $ "on_select ttl request driver:-" <> show driverQuote.validTill
  isValueAddNP <- CValueAddNP.isValueAddNP searchReq.bapId
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle transporter.id "MOBILITY" (Utils.mapServiceTierToCategory driverQuote.vehicleServiceTier) >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId:-" <> show transporter.id.getId <> ",domain:-MOBILITY,vehicleVariant:-" <> show (Utils.mapServiceTierToCategory driverQuote.vehicleServiceTier))
  farePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback driverQuote.id.getId
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId driverQuote.vehicleServiceTier searchTry.merchantOperatingCityId >>= fromMaybeM (VehicleServiceTierNotFound $ show driverQuote.vehicleServiceTier)
  callOnSelectV2 transporter searchReq srfd searchTry =<< (buildOnSelectReq transporter vehicleServiceTierItem searchReq driverQuote isValueAddNP <&> ACL.mkOnSelectMessageV2 isValueAddNP bppConfig transporter farePolicy)
  where
    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      DM.Merchant ->
      DVST.VehicleServiceTier ->
      DSR.SearchRequest ->
      DDQ.DriverQuote ->
      Bool ->
      m ACL.DOnSelectReq
    buildOnSelectReq org vehicleServiceTierItem searchRequest quotes isValueAddNP = do
      now <- getCurrentTime
      logDebug $ "on_select: searchRequest " <> show searchRequest
      logDebug $ "on_select: quotes " <> show quotes
      let transporterInfo =
            ACL.TransporterInfo
              { merchantShortId = org.shortId,
                name = org.name,
                contacts = fromMaybe "" org.mobileNumber,
                ridesInProgress = 0, -- FIXME
                ridesCompleted = 0, -- FIXME
                ridesConfirmed = 0 -- FIXME
              }
      pure $
        ACL.DOnSelectReq
          { transporterInfo,
            vehicleServiceTierItem,
            driverQuote,
            taggings = getTags isValueAddNP,
            now,
            searchRequest
          }

    getTags :: Bool -> Tags.Taggings
    getTags isValueAddNP = do
      def{Tags.itemTags =
            [ (Tags.DISTANCE_TO_NEAREST_DRIVER_METER, Just $ show driverQuote.distanceToPickup.getMeters),
              (Tags.ETA_TO_NEAREST_DRIVER_MIN, Just . show $ driverQuote.durationToPickup.getSeconds `div` 60),
              (Tags.UPGRADE_TO_CAB, show <$> srfd.upgradeCabRequest)
            ]
              <> if isJust driverQuote.specialLocationTag && isValueAddNP then [(Tags.SPECIAL_LOCATION_TAG, driverQuote.specialLocationTag)] else []
         }

sendDriverArrivalUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Maybe UTCTime ->
  m ()
sendDriverArrivalUpdateToBAP booking ride arrivalTime = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
  riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
      paymentUrl = Nothing
      bookingDetails = ACL.BookingDetails {..}
      estimateId = booking.estimateId <&> (.getId)
      driverArrivedBuildReq = ACL.DriverArrivedBuildReq ACL.DDriverArrivedReq {..}
  retryConfig <- asks (.shortDurationRetryCfg)
  driverArrivedMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing driverArrivedBuildReq
  void $ callOnUpdateV2 driverArrivedMsgV2 retryConfig merchant.id

sendPhoneCallRequestUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendPhoneCallRequestUpdateToBAP booking ride = do
  mbBookingDetails <- buildBookingDetails booking ride
  whenJust mbBookingDetails $ \bookingDetails -> do
    let phoneCallRequestReq = ACL.PhoneCallRequestBuildReq ACL.DPhoneCallRequestReq {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    phoneCallRequestMsgV2 <- ACL.buildOnUpdateMessageV2 bookingDetails.merchant booking Nothing phoneCallRequestReq
    void $ callOnUpdateV2 phoneCallRequestMsgV2 retryConfig bookingDetails.merchant.id

sendPhoneCallCompletedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendPhoneCallCompletedUpdateToBAP booking ride = do
  mbBookingDetails <- buildBookingDetails booking ride
  whenJust mbBookingDetails $ \bookingDetails -> do
    let phoneCallCompletedReq = ACL.PhoneCallCompletedBuildReq ACL.DPhoneCallCompletedReq {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    phoneCallRequestMsgV2 <- ACL.buildOnUpdateMessageV2 bookingDetails.merchant booking Nothing phoneCallCompletedReq
    void $ callOnUpdateV2 phoneCallRequestMsgV2 retryConfig bookingDetails.merchant.id

buildBookingDetails ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m (Maybe ACL.BookingDetails)
buildBookingDetails booking ride = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  if isValueAddNP
    then do
      merchant <-
        CQM.findById booking.providerId
          >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
      driver <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      driverStats <- runInReplica $ QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
      vehicle <- runInReplica $ QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
      bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
      mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
        CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
          >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
      let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
      let paymentUrl = Nothing
      riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
      riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
      return $ Just ACL.BookingDetails {..}
    else return Nothing

sendStopArrivalUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  m ()
sendStopArrivalUpdateToBAP booking ride driver vehicle = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
  let paymentUrl = Nothing
  when isValueAddNP $ do
    merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    driverStats <- QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
    riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
    let bookingDetails = ACL.BookingDetails {..}
        stopArrivedBuildReq = ACL.StopArrivedBuildReq ACL.DStopArrivedBuildReq {..}
    stopArrivedMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing stopArrivedBuildReq
    retryConfig <- asks (.shortDurationRetryCfg)
    void $ callOnUpdateV2 stopArrivedMsgV2 retryConfig merchant.id

sendNewMessageToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  T.Text ->
  m ()
sendNewMessageToBAP booking ride message = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
    vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    let paymentUrl = Nothing
    riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
    riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
    let bookingDetails = ACL.BookingDetails {..}
        newMessageBuildReq = ACL.NewMessageBuildReq ACL.DNewMessageReq {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    newMessageMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing newMessageBuildReq
    void $ callOnUpdateV2 newMessageMsgV2 retryConfig merchant.id

sendUpdateEditDestToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DBUR.BookingUpdateRequest ->
  Maybe DLoc.Location ->
  Maybe Maps.LatLong ->
  DOU.UpdateType ->
  m ()
sendUpdateEditDestToBAP booking ride bookingUpdateReqDetails newDestination currentLocation updateType = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
    vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    let paymentUrl = Nothing
    riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
    riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
    let bookingDetails = ACL.BookingDetails {..}
        sUpdateEditDestToBAPReq = ACL.EditDestinationUpdate ACL.DEditDestinationUpdateReq {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    sUpdateEditDestToBAP <- ACL.buildOnUpdateMessageV2 merchant booking (Just bookingUpdateReqDetails.bapBookingUpdateRequestId) sUpdateEditDestToBAPReq
    void $ callOnUpdateV2 sUpdateEditDestToBAP retryConfig merchant.id

sendUpdateEditDestErrToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  Text ->
  Text ->
  Text ->
  m ()
sendUpdateEditDestErrToBAP booking bapBookingUpdateRequestId errorCode errorMessage = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    let errorObject = DOU.DErrorObject {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    updateEditDestErrToBAP <- ACL.buildOnUpdateError merchant booking (Just bapBookingUpdateRequestId) errorObject
    void $ callOnUpdateV2 updateEditDestErrToBAP retryConfig merchant.id

sendSafetyAlertToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  T.Text ->
  DP.Person ->
  DVeh.Vehicle ->
  m ()
sendSafetyAlertToBAP booking ride reason driver vehicle = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    driverStats <- QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    let paymentUrl = Nothing
    riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
    riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
    let bookingDetails = ACL.BookingDetails {..}
        safetyAlertBuildReq = ACL.SafetyAlertBuildReq ACL.DSafetyAlertReq {..}

    retryConfig <- asks (.shortDurationRetryCfg)
    safetyAlertMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing safetyAlertBuildReq
    void $ callOnUpdateV2 safetyAlertMsgV2 retryConfig merchant.id

sendEstimateRepetitionUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Id DEst.Estimate ->
  SRBCR.CancellationSource ->
  DP.Person ->
  DVeh.Vehicle ->
  m ()
sendEstimateRepetitionUpdateToBAP booking ride estimateId cancellationSource driver vehicle = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    driverStats <- QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    let paymentUrl = Nothing
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
    riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
    let bookingDetails = ACL.BookingDetails {..}
        estimateRepetitionBuildReq = ACL.EstimateRepetitionBuildReq ACL.DEstimateRepetitionReq {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    estimateRepMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing estimateRepetitionBuildReq
    void $ callOnUpdateV2 estimateRepMsgV2 retryConfig merchant.id

sendQuoteRepetitionUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Id DRB.Booking ->
  SRBCR.CancellationSource ->
  DP.Person ->
  DVeh.Vehicle ->
  m ()
sendQuoteRepetitionUpdateToBAP booking ride newBookingId cancellationSource driver vehicle = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    driverStats <- QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    let paymentUrl = Nothing
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
    riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
    let bookingDetails = ACL.BookingDetails {..}
        quoteRepetitionBuildReq = ACL.QuoteRepetitionBuildReq ACL.DQuoteRepetitionReq {..}
    retryConfig <- asks (.shortDurationRetryCfg)
    quoteRepMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing quoteRepetitionBuildReq
    void $ callOnUpdateV2 quoteRepMsgV2 retryConfig merchant.id

sendTollCrossedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Maybe DRB.Booking ->
  Maybe SRide.Ride ->
  DP.Person ->
  DDriverStats.DriverStats ->
  DVeh.Vehicle ->
  m ()
sendTollCrossedUpdateToBAP (Just booking) (Just ride) driver driverStats vehicle = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  when isValueAddNP $ do
    merchant <-
      CQM.findById booking.providerId
        >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    let paymentUrl = Nothing
        riderPhone = Nothing
    bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
    let bookingDetails = ACL.BookingDetails {..}
        tollCrossedUpdateBuildReq = ACL.TollCrossedBuildReq ACL.DTollCrossedBuildReq {..}
    tollCrossedMsg <- ACL.buildOnUpdateMessageV2 merchant booking Nothing tollCrossedUpdateBuildReq
    retryConfig <- asks (.shortDurationRetryCfg)
    void $ callOnUpdateV2 tollCrossedMsg retryConfig merchant.id
sendTollCrossedUpdateToBAP _ _ _ _ _ = do
  logTagError "on_update_req" "on_update_err - Could not send toll crossed update to BPP : booking or ride not found"
  pure ()

sendDestinationArrivalUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Maybe UTCTime ->
  m ()
sendDestinationArrivalUpdateToBAP booking ride destinationArrivalTime = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  let riderPhone = Nothing
      paymentMethodInfo = Nothing
      paymentUrl = Nothing
      bookingDetails = ACL.BookingDetails {..}
      driverReachedDestBuildReq = ACL.DriverReachedDestinationBuildReq ACL.DDriverReachedDestinationReq {..}
  retryConfig <- asks (.shortDurationRetryCfg)
  driverReachedDestMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing driverReachedDestBuildReq
  void $ callOnUpdateV2 driverReachedDestMsgV2 retryConfig merchant.id

notfyDeliveryImageUploadedToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
notfyDeliveryImageUploadedToBAP booking ride = do
  isValueAddNP <- CValueAddNP.isValueAddNP booking.bapId
  merchant <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  let riderPhone = Nothing
      paymentMethodInfo = Nothing
      paymentUrl = Nothing
      bookingDetails = ACL.BookingDetails {..}
      deliveryImageUploadReq = ACL.ParcelImageUploadedBuildReq ACL.DParcelImageUploadedReq {..}
  retryConfig <- asks (.shortDurationRetryCfg)
  deliveryImageUploadMsgV2 <- ACL.buildOnUpdateMessageV2 merchant booking Nothing deliveryImageUploadReq
  void $ callOnUpdateV2 deliveryImageUploadMsgV2 retryConfig merchant.id

callBecknAPIWithSignature' ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    ToJSON req,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  Id Merchant.Merchant ->
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HMS.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature' merchantId a b c d e req' = do
  fork ("sending " <> show b <> ", pushing ondc logs") do
    void $ pushLogs b (toJSON req') merchantId.getId "MOBILITY"
  Beckn.callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing b c d e req'
