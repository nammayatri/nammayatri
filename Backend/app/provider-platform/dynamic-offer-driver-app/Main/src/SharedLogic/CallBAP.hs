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
    sendEstimateRepetitionUpdateToBAP,
    sendNewMessageToBAP,
    sendDriverOffer,
    callOnConfirm,
    -- callOnConfirmV2,
    buildBppUrl,
    sendAlertToBAP,
  )
where

import qualified AWS.S3 as S3
import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Aeson as A
import Data.Either.Extra (eitherToMaybe)
import qualified Data.HashMap as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Domain.Action.UI.DriverOnboarding.AadhaarVerification
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.DriverOnboarding.Image as DIT
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as V
import qualified EulerHS.Types as ET
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as QIV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Metrics (CoreMetrics)

callOnSelect ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  OnSelect.OnSelectMessage ->
  m ()
callOnSelect transporter searchRequest searchTry content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  let msgId = searchTry.estimateId.getId
  context <- buildTaxiContext Context.ON_SELECT msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry) False
  logDebug $ "on_select request bpp: " <> show content
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_SELECT) API.onSelectAPIV1 bapUri internalEndPointHashMap (BecknCallbackReq context $ Right content)

callOnSelectV2 ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  Spec.OnSelectReqMessage ->
  m ()
callOnSelectV2 transporter searchRequest searchTry content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  let msgId = searchTry.estimateId.getId
  context <- ContextV2.buildContextV2 Context.ON_SELECT Context.MOBILITY msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry)
  logDebug $ "on_selectV2 request bpp: " <> show content
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_SELECT) API.onSelectAPIV2 bapUri internalEndPointHashMap (Spec.OnSelectReq context Nothing (Just content))

callOnUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c
  ) =>
  DM.Merchant ->
  Text ->
  BaseUrl ->
  Maybe Context.City ->
  Maybe Context.Country ->
  Text ->
  OnUpdate.OnUpdateMessage ->
  RetryCfg ->
  m ()
callOnUpdate transporter bapId bapUri bapCity bapCountry transactionId content retryConfig = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId (Just transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city bapCity) (fromMaybe Context.India bapCountry) False
  void $ withRetryConfig retryConfig $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPIV1 bapUri internalEndPointHashMap (BecknCallbackReq context $ Right content)

-- _callOnUpdateV2 ::
--   ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
--     HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
--     CoreMetrics m,
--     HasHttpClientOptions r c
--   ) =>
--   DM.Merchant ->
--   Text ->
--   BaseUrl ->
--   Maybe Context.City ->
--   Maybe Context.Country ->
--   Text ->
--   -- OnUpdate.OnUpdateMessageV2 ->
--   Spec.ConfirmReqMessage ->
--   RetryCfg ->
--   m ()
-- _callOnUpdateV2 transporter bapId bapUri bapCity bapCountry transactionId content retryConfig = do
--   let bppSubscriberId = getShortId $ transporter.subscriberId
--       authKey = getHttpManagerKey bppSubscriberId
--   bppUri <- buildBppUrl (transporter.id)
--   msgId <- generateGUID
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   context <- buildTaxiContextV2 Context.ON_UPDATE msgId (Just transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city bapCity) (fromMaybe Context.India bapCountry)
--   void $ withRetryConfig retryConfig $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPIV2 bapUri internalEndPointHashMap (BecknCallbackReqV2 context $ Right content)

callOnConfirm ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  DM.Merchant ->
  Context.Context ->
  OnConfirm.OnConfirmMessage ->
  m ()
callOnConfirm transporter contextFromConfirm content = do
  let bapUri = contextFromConfirm.bap_uri
      bapId = contextFromConfirm.bap_id
      msgId = contextFromConfirm.message_id
      city = contextFromConfirm.city
      country = contextFromConfirm.country
      bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl transporter.id
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  context_ <- buildTaxiContext Context.ON_CONFIRM msgId contextFromConfirm.transaction_id bapId bapUri (Just bppSubscriberId) (Just bppUri) city country False
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPIV1 bapUri internalEndPointHashMap (BecknCallbackReq context_ $ Right content)

-- callOnConfirmV2 ::
--   ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
--     HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
--     HasHttpClientOptions r c,
--     HasShortDurationRetryCfg r c,
--     CoreMetrics m
--   ) =>
--   DM.Merchant ->
--   BaseUrl ->
--   Text ->
--   Text ->
--   Context.City ->
--   Context.Country ->
--   Maybe Text ->
--   -- OnConfirm.OnConfirmMessageV2 ->
--   Spec.ConfirmReqMessage ->
--   m ()
-- callOnConfirmV2 transporter bapUri bapId msgId city country txnId content = do
--   let bppSubscriberId = getShortId $ transporter.subscriberId
--       authKey = getHttpManagerKey bppSubscriberId
--   bppUri <- buildBppUrl transporter.id
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   context_ <- buildTaxiContextV2 Context.ON_CONFIRM msgId txnId bapId bapUri (Just bppSubscriberId) (Just bppUri) city country
--   void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPIV2 bapUri internalEndPointHashMap (BecknCallbackReqV2 context_ $ Right content)

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id DM.Merchant ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

sendRideAssignedUpdateToBAP ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HM.Map Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    HasField "isBecknSpecVersion2" r Bool,
    LT.HasLocationService m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP booking ride = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  veh <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
  mbTransporterConfig <- CQTC.findByMerchantOpCityId booking.merchantOperatingCityId -- these two lines just for backfilling driver vehicleModel from idfy TODO: remove later
  vehicle <-
    case mbTransporterConfig of
      Just transporterConfig ->
        if transporterConfig.refillVehicleModel
          then do
            reffiledVeh <- refillVehicleModel veh
            pure $
              case reffiledVeh of
                Right reffiledVeh' -> reffiledVeh'
                Left _ -> veh
          else pure veh
      Nothing -> pure veh
  resp <- try @_ @SomeException (fetchAndCacheAadhaarImage driver driverInfo)
  let image = join (eitherToMaybe resp)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq ACL.DRideAssignedReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
  rideAssignedMsgV2 <- ACL.buildOnUpdateMessageV2 transporter rideAssignedBuildReq
  let generatedMsg = A.encode rideAssignedMsgV2
  logDebug $ "on_update request bppv2: " <> T.pack (show generatedMsg)

  retryConfig <- asks (.shortDurationRetryCfg)
  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideAssignedMsg retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideAssignedMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideAssignedMsg retryConfig
  where
    refillKey = "REFILLED_" <> ride.driverId.getId
    updateVehicle V.Vehicle {..} newModel = V.Vehicle {model = newModel, ..}
    refillVehicleModel veh = try @_ @SomeException do
      -- TODO: remove later
      mbIsRefilledToday :: Maybe Bool <- Redis.get refillKey
      case mbIsRefilledToday of
        Just True -> Redis.expire refillKey 86400 $> veh
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
              let modelValueToUpdate = fromMaybe "" $ HM.lookup newModel modelNamesHashMap
              if modelValueToUpdate == veh.model
                then pure veh
                else QVeh.updateVehicleModel modelValueToUpdate ride.driverId $> updateVehicle veh modelValueToUpdate
          Redis.setExp refillKey True 86400
          pure newVehicle

sendRideStartedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
  -- rideStartedMsgV2 <- ACL.buildOnUpdateMessage rideStartedBuildReq -- shrey00 : implement buildOnUpdateMessageV2
  retryConfig <- asks (.longDurationRetryCfg)

  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideStartedMsgV2 retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideStartedMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideStartedMsg retryConfig

sendRideCompletedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  Maybe DMPM.PaymentMethodInfo ->
  Maybe Text ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams paymentMethodInfo paymentUrl = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {..}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
  -- rideCompletedMsgV2 <- ACL.buildOnUpdateMessage rideCompletedBuildReq -- shrey00 : implement buildOnUpdateMessageV2
  retryConfig <- asks (.longDurationRetryCfg)
  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideCompletedMsgV2 retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideCompletedMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideCompletedMsg retryConfig

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  DRB.Booking ->
  DM.Merchant ->
  SRBCR.CancellationSource ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  -- bookingCancelledMsgV2 <- ACL.buildOnUpdateMessage bookingCancelledBuildReq -- shrey00 : implement buildOnUpdateMessageV2
  retryConfig <- asks (.longDurationRetryCfg)
  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId bookingCancelledMsgV2 retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId bookingCancelledMsg retryConfig
  callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId bookingCancelledMsg retryConfig

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  DDQ.DriverQuote ->
  m ()
sendDriverOffer transporter searchReq searchTry driverQuote = do
  logDebug $ "on_select ttl request driver: " <> show driverQuote.validTill
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then callOnSelectV2 transporter searchReq searchTry =<< (buildOnSelectReq transporter searchReq driverQuote <&> ACL.mkOnSelectMessageV2) -- shrey00 : implement mkOnSelectMessageV2 -- shrey00 : implement callOnSelectV2
    else callOnSelect transporter searchReq searchTry =<< (buildOnSelectReq transporter searchReq driverQuote <&> ACL.mkOnSelectMessage)
  where
    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      DM.Merchant ->
      DSR.SearchRequest ->
      DDQ.DriverQuote ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime
      logPretty DEBUG "on_select: searchRequest" searchRequest
      logPretty DEBUG "on_select: quotes" quotes
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
            driverQuote,
            now,
            searchRequest
          }

sendDriverArrivalUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Maybe UTCTime ->
  m ()
sendDriverArrivalUpdateToBAP booking ride arrivalTime = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let driverArrivedBuildReq = ACL.DriverArrivedBuildReq {..}
  driverArrivedMsg <- ACL.buildOnUpdateMessage driverArrivedBuildReq
  -- driverArrivedMsgV2 <- ACL.buildOnUpdateMessage driverArrivedBuildReq -- shrey00 : implement buildOnUpdateMessageV2

  retryConfig <- asks (.shortDurationRetryCfg)
  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId driverArrivedMsgV2 retryConfig shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId driverArrivedMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId driverArrivedMsg retryConfig

sendNewMessageToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  T.Text ->
  m ()
sendNewMessageToBAP booking ride message = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let newMessageBuildReq = ACL.NewMessageBuildReq {..}
  newMessageMsg <- ACL.buildOnUpdateMessage newMessageBuildReq
  -- newMessageMsgV2 <- ACL.buildOnUpdateMessage newMessageBuildReq -- shrey00 : implement buildOnUpdateMessageV2
  retryConfig <- asks (.shortDurationRetryCfg)

  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId newMessageMsgV2 retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId newMessageMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId newMessageMsg retryConfig

sendAlertToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  T.Text ->
  m ()
sendAlertToBAP booking ride reason = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let safetyAlertBuildReq = ACL.SafetyAlertBuildReq {ride, reason}
  safetyAlertMsg <- ACL.buildOnUpdateMessage safetyAlertBuildReq
  -- safetyAlertMsgV2 <- ACL.buildOnUpdateMessage safetyAlertBuildReq -- shrey00 : implement buildOnUpdateMessageV2

  retryConfig <- asks (.shortDurationRetryCfg)
  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId safetyAlertMsgV2 retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId safetyAlertMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId safetyAlertMsg retryConfig

sendEstimateRepetitionUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Id DEst.Estimate ->
  SRBCR.CancellationSource ->
  m ()
sendEstimateRepetitionUpdateToBAP booking ride estimateId cancellationSource = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let estimateRepetitionBuildReq = ACL.EstimateRepetitionBuildReq {cancellationSource, booking, estimateId, ride}
  estimateRepMsg <- ACL.buildOnUpdateMessage estimateRepetitionBuildReq
  -- estimateRepMsgV2 <- ACL.buildOnUpdateMessage estimateRepetitionBuildReq -- shrey00 : implement buildOnUpdateMessageV2

  retryConfig <- asks (.shortDurationRetryCfg)
  _isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)

  -- if isBecknSpecVersion2
  --   then void $ callOnUpdateV2 transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId estimateRepMsgV2 retryConfig -- shrey00 : implement callOnUpdateV2
  --   else void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId estimateRepMsg retryConfig
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId estimateRepMsg retryConfig
