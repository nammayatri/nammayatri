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
    callOnConfirmV2,
    buildBppUrl,
    sendSafetyAlertToBAP,
  )
where

import qualified AWS.S3 as S3
import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
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
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time hiding (getCurrentTime)
import Domain.Action.UI.DriverOnboarding.AadhaarVerification
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.DriverOnboarding.Image as DIT
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as DVeh
import qualified EulerHS.Types as ET
import Kernel.External.Maps.Types as Maps
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
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
  let msgId = searchTry.estimateId
  context <- buildTaxiContext Context.ON_SELECT msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry) False
  logDebug $ "on_select request bpp: " <> show content
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_SELECT) API.onSelectAPIV1 bapUri internalEndPointHashMap (BecknCallbackReq context $ Right content)

callOnSelectV2 ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
  let msgId = searchTry.estimateId
  context <- ContextV2.buildContextV2 Context.ON_SELECT Context.MOBILITY msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry)
  logDebug $ "on_selectV2 request bpp: " <> show content
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_SELECT) API.onSelectAPIV2 bapUri internalEndPointHashMap (Spec.OnSelectReq context Nothing (Just content))

callOnUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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

callOnUpdateV2 ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    HasHttpClientOptions r c
  ) =>
  Spec.OnUpdateReq ->
  RetryCfg ->
  m ()
callOnUpdateV2 req retryConfig = do
  bapUri' <- req.onUpdateReqContext.contextBapUri & fromMaybeM (InternalError "BAP URI is not present in Ride Assigned request context.")
  bapUri <- parseBaseUrl bapUri'
  bppSubscriberId <- req.onUpdateReqContext.contextBppId & fromMaybeM (InternalError "BPP ID is not present in Ride Assigned request context.")
  let authKey = getHttpManagerKey bppSubscriberId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ withRetryConfig retryConfig $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPIV2 bapUri internalEndPointHashMap req

callOnConfirm ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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

callOnConfirmV2 ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  DM.Merchant ->
  Spec.Context ->
  -- OnConfirm.OnConfirmMessageV2 ->
  Spec.ConfirmReqMessage ->
  m ()
callOnConfirmV2 transporter context content = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bapUri <- Utils.getContextBapUri context
  bapId <- Utils.getContextBapId context
  msgId <- Utils.getMessageId context
  city <- Utils.getContextCity context
  country <- Utils.getContextCountry context
  bppUri <- buildBppUrl transporter.id
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  txnId <- Utils.getTransactionId context
  context_ <- ContextV2.buildContextV2 Context.ON_CONFIRM Context.MOBILITY msgId (Just txnId) bapId bapUri (Just bppSubscriberId) (Just bppUri) city country
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPIV2 bapUri internalEndPointHashMap (Spec.OnConfirmReq {onConfirmReqContext = context_, onConfirmReqError = Nothing, onConfirmReqMessage = Just content})

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
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  m ()
sendRideAssignedUpdateToBAP booking ride driver veh = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
  mbTransporterConfig <- CQTC.findByMerchantOpCityId booking.merchantOperatingCityId -- these two lines just for backfilling driver vehicleModel from idfy TODO: remove later
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
  resp <- try @_ @SomeException (fetchAndCacheAadhaarImage driver driverInfo)
  let image = join (eitherToMaybe resp)
  isDriverBirthDay <- maybe (return False) (checkIsDriverBirthDay mbTransporterConfig) driverInfo.driverDob
  isFreeRide <- maybe (return False) (checkIfRideBySpecialDriver ride.driverId) mbTransporterConfig
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq ACL.DRideAssignedReq {..}
  retryConfig <- asks (.shortDurationRetryCfg)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      rideAssignedMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry rideAssignedBuildReq
      let generatedMsg = A.encode rideAssignedMsgV2
      logDebug $ "ride assigned on_update request bppv2: " <> T.pack (show generatedMsg)
      void $ callOnUpdateV2 rideAssignedMsgV2 retryConfig
    else do
      rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideAssignedMsg retryConfig
  where
    refillKey = "REFILLED_" <> ride.driverId.getId
    updateVehicle DVeh.Vehicle {..} newModel = DVeh.Vehicle {model = newModel, ..}
    refillVehicleModel = try @_ @SomeException do
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

    checkIsDriverBirthDay mbTransporterConfig driverBirthDate = do
      case mbTransporterConfig of
        Just tc -> do
          let (_, birthMonth, birthDay) = toGregorian $ utctDay driverBirthDate
          currentLocalTime <- getLocalCurrentTime tc.timeDiffFromUtc
          let (_, curMonth, curDay) = toGregorian $ utctDay currentLocalTime
          return (birthMonth == curMonth && birthDay == curDay)
        Nothing -> return False

    checkIfRideBySpecialDriver driverId transporterConfig = do
      let specialDrivers = transporterConfig.specialDrivers
      if null specialDrivers
        then return False
        else return $ (getId driverId) `elem` specialDrivers

sendRideStartedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Maybe Maps.LatLong ->
  m ()
sendRideStartedUpdateToBAP booking ride tripStartLocation = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId) -- shrey00 : are these 2 lines needed?
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId) -- shrey00 : are these 2 lines needed?
  let rideStartedBuildReq = ACL.RideStartedBuildReq ACL.DRideStartedReq {..}
  retryConfig <- asks (.longDurationRetryCfg)

  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      rideStartedMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry rideStartedBuildReq
      void $ callOnUpdateV2 rideStartedMsgV2 retryConfig
    else do
      rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideStartedMsg retryConfig

sendRideCompletedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  Maybe DMPM.PaymentMethodInfo ->
  Maybe Text ->
  Maybe Maps.LatLong ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams paymentMethodInfo paymentUrl tripEndLocation = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId) -- shrey00 : are these 2 lines needed?
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId) -- shrey00 : are these 2 lines needed?
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq ACL.DRideCompletedReq {..}
  retryConfig <- asks (.longDurationRetryCfg)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      rideCompletedMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry rideCompletedBuildReq
      void $ callOnUpdateV2 rideCompletedMsgV2 retryConfig
    else do
      rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideCompletedMsg retryConfig

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq ACL.DBookingCancelledReq {..}
  retryConfig <- asks (.longDurationRetryCfg)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      bookingCancelledMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry bookingCancelledBuildReq
      void $ callOnUpdateV2 bookingCancelledMsgV2 retryConfig
    else do
      bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId bookingCancelledMsg retryConfig

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
  let driverArrivedBuildReq = ACL.DriverArrivedBuildReq ACL.DDriverArrivedReq {..}
  retryConfig <- asks (.shortDurationRetryCfg)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      driverArrivedMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry driverArrivedBuildReq
      void $ callOnUpdateV2 driverArrivedMsgV2 retryConfig
    else do
      driverArrivedMsg <- ACL.buildOnUpdateMessage driverArrivedBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId driverArrivedMsg retryConfig

sendNewMessageToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
  let newMessageBuildReq = ACL.NewMessageBuildReq ACL.DNewMessageReq {..}
  retryConfig <- asks (.shortDurationRetryCfg)

  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      newMessageMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry newMessageBuildReq -- shrey00 : implement buildOnUpdateMessageV2
      void $ callOnUpdateV2 newMessageMsgV2 retryConfig
    else do
      newMessageMsg <- ACL.buildOnUpdateMessage newMessageBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId newMessageMsg retryConfig

sendSafetyAlertToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasField "isBecknSpecVersion2" r Bool
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  T.Text ->
  T.Text ->
  m ()
sendSafetyAlertToBAP booking ride code reason = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let safetyAlertBuildReq = ACL.SafetyAlertBuildReq ACL.DSafetyAlertReq {..}

  retryConfig <- asks (.shortDurationRetryCfg)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      safetyAlertMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry safetyAlertBuildReq
      void $ callOnUpdateV2 safetyAlertMsgV2 retryConfig
    else do
      safetyAlertMsg <- ACL.buildOnUpdateMessage safetyAlertBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId safetyAlertMsg retryConfig

sendEstimateRepetitionUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
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
  let estimateRepetitionBuildReq = ACL.EstimateRepetitionBuildReq ACL.DEstimateRepetitionReq {cancellationSource, booking, estimateId, ride}

  retryConfig <- asks (.shortDurationRetryCfg)
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)

  if isBecknSpecVersion2
    then do
      estimateRepMsgV2 <- ACL.buildOnUpdateMessageV2 transporter booking.bapCity booking.bapCountry estimateRepetitionBuildReq
      void $ callOnUpdateV2 estimateRepMsgV2 retryConfig
    else do
      estimateRepMsg <- ACL.buildOnUpdateMessage estimateRepetitionBuildReq
      void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId estimateRepMsg retryConfig
