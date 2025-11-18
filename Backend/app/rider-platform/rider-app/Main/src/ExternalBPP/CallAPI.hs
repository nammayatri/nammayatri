module ExternalBPP.CallAPI where

import qualified API.Types.UI.FRFSTicketService
import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Select as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station as DStation
import Environment
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError (..))
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error
import qualified Tools.Metrics as Metrics

type FRFSSearchFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r
  )

type FRFSConfirmFlow m r =
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

type FRFSSelectFlow m r c =
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    HasShortDurationRetryCfg r c,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

discoverySearch :: FRFSSearchFlow m r => Merchant -> BecknConfig -> IntegratedBPPConfig -> API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq -> m ()
discoverySearch merchant bapConfig integratedBPPConfig req = do
  transactionId <- generateGUID
  bknSearchReq <- ACL.buildSearchReq transactionId req.vehicleType bapConfig Nothing Nothing req.city
  logDebug $ "FRFS Discovery SearchReq " <> encodeToText bknSearchReq
  case integratedBPPConfig.providerConfig of
    ONDC ONDCBecknConfig {networkHostUrl} -> do
      let providerUrl = fromMaybe bapConfig.gatewayUrl networkHostUrl
      void $ CallFRFSBPP.search providerUrl bknSearchReq merchant.id
    _ -> do
      void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id

search :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> Maybe HighPrecMoney -> [FRFSRouteDetails] -> IntegratedBPPConfig -> m ()
search merchant merchantOperatingCity bapConfig searchReq mbFare routeDetails integratedBPPConfig = do
  Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
  case integratedBPPConfig.providerConfig of
    ONDC ONDCBecknConfig {networkHostUrl, networkId, fareCachingAllowed} -> do
      fork ("FRFS ONDC SearchReq for " <> show bapConfig.vehicleCategory) $ do
        case (fareCachingAllowed, networkHostUrl, networkId, mbFare) of
          (Just True, Just _, Just _, Just _) ->
            withTryCatch "callExternalBPP:searchFlow" (Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig networkHostUrl networkId searchReq routeDetails)
              >>= \case
                Left err -> do
                  logError $ "Error in calling ONDC Search: " <> show err
                  callOndcSearch networkHostUrl
                Right onSearchReq ->
                  if null onSearchReq.quotes
                    then callOndcSearch networkHostUrl
                    else processOnSearch onSearchReq
          _ -> callOndcSearch networkHostUrl
    _ -> do
      onSearchReq <- Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig Nothing Nothing searchReq routeDetails
      processOnSearch onSearchReq
  where
    processOnSearch :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => DOnSearch.DOnSearch -> m ()
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

    callOndcSearch :: Maybe BaseUrl -> (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => m ()
    callOndcSearch networkHostUrl = do
      let providerUrl = fromMaybe bapConfig.gatewayUrl networkHostUrl
      fromStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.fromStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound searchReq.fromStationCode)
      toStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.toStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound searchReq.toStationCode)
      routeStopMappingFromStation <- OTPRest.getRouteStopMappingByStopCode searchReq.fromStationCode integratedBPPConfig
      routeStopMappingToStation <- OTPRest.getRouteStopMappingByStopCode searchReq.toStationCode integratedBPPConfig
      let fromStationProviderCode = fromMaybe searchReq.fromStationCode (listToMaybe routeStopMappingFromStation <&> (.providerCode))
          toStationProviderCode = fromMaybe searchReq.toStationCode (listToMaybe routeStopMappingToStation <&> (.providerCode))
      bknSearchReq <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just $ fromStation {DStation.code = fromStationProviderCode}) (Just $ toStation {DStation.code = toStationProviderCode}) merchantOperatingCity.city
      logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
      void $ CallFRFSBPP.search providerUrl bknSearchReq merchant.id

select ::
  FRFSSelectFlow m r c =>
  (DOnSelect -> Maybe Bool -> Maybe Bool -> m ()) ->
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  DQuote.FRFSQuote ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  Maybe Bool ->
  Maybe Bool ->
  m ()
select processOnSelectHandler merchant merchantOperatingCity bapConfig quote quoteCategories isSingleMode mbEnableOffer = do
  Metrics.startMetrics Metrics.SELECT_FRFS merchant.name quote.searchId.getId merchantOperatingCity.id.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork ("FRFS ONDC SelectReq for " <> show bapConfig.vehicleCategory) $ do
        let categories =
              mapMaybe
                ( \category -> do
                    selectedQuantity <- FRFSUtils.nonZeroQuantity category.selectedQuantity
                    return $ DCategorySelect {bppItemId = category.bppItemId, quantity = selectedQuantity, category = category.category, price = category.price}
                )
                quoteCategories
        providerUrl <- quote.bppSubscriberUrl & parseBaseUrl
        bknSelectReq <- ACL.buildSelectReq quote bapConfig Utils.BppData {bppId = quote.bppSubscriberId, bppUri = quote.bppSubscriberUrl} merchantOperatingCity.city categories
        logDebug $ "FRFS SelectReq " <> encodeToText bknSelectReq
        void $ CallFRFSBPP.select providerUrl bknSelectReq merchant.id
    _ -> do
      onSelectReq <- Flow.select merchant merchantOperatingCity integratedBPPConfig bapConfig quote quoteCategories
      processOnSelectHandler onSelectReq isSingleMode mbEnableOffer

init :: (FRFSConfirmFlow m r) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> Maybe Bool -> m ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbEnableOffer = do
  Metrics.startMetrics Metrics.INIT_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      let categories =
            mapMaybe
              ( \category -> do
                  selectedQuantity <- FRFSUtils.nonZeroQuantity category.selectedQuantity
                  return $ DCategorySelect {bppItemId = category.bppItemId, quantity = selectedQuantity, category = category.category, price = category.offeredPrice}
              )
              quoteCategories
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city categories
      logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id
    _ -> do
      onInitReq <- Flow.init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories
      processOnInit onInitReq
  where
    processOnInit :: (FRFSConfirmFlow m r) => DOnInit.DOnInit -> m ()
    processOnInit onInitReq = do
      (merchant', booking', quoteCategories') <- DOnInit.validateRequest onInitReq
      DOnInit.onInit onInitReq merchant' booking' quoteCategories' mbEnableOffer

cancel ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    Metrics.HasBAPMetrics m r
  ) =>
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  Spec.CancellationType ->
  DBooking.FRFSTicketBooking ->
  m ()
cancel merchant merchantOperatingCity bapConfig cancellationType booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  when (cancellationType == Spec.SOFT_CANCEL) $
    unless (booking.status == DBooking.CONFIRMED) $ throwError (InvalidRequest $ "Cancellation during incorrect status: " <> show booking.status)
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Cancel Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl")
        messageId <- generateGUID
        when (cancellationType == Spec.CONFIRM_CANCEL) $ Redis.setExp (FRFSUtils.makecancelledTtlKey booking.id) messageId ttl
        bknCancelReq <- ACL.buildCancelReq messageId booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} frfsConfig.cancellationReasonId cancellationType merchantOperatingCity.city
        logDebug $ "FRFS CancelReq " <> encodeToText bknCancelReq
        void $ CallFRFSBPP.cancel providerUrl bknCancelReq merchant.id
    _ -> return ()

confirm ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    Metrics.HasBAPMetrics m r
  ) =>
  (DOrder -> m ()) ->
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  (Maybe Text, Maybe Text) ->
  DBooking.FRFSTicketBooking ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  m ()
confirm onConfirmHandler merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking quoteCategories = do
  Metrics.startMetrics Metrics.CONFIRM_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Confirm Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        let filteredDCategories :: [DCategorySelect] =
              mapMaybe
                ( \category -> do
                    selectedQuantity <- FRFSUtils.nonZeroQuantity category.selectedQuantity
                    return $ DCategorySelect {bppItemId = category.bppItemId, quantity = selectedQuantity, category = category.category, price = fromMaybe category.offeredPrice category.finalPrice}
                )
                quoteCategories
        bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) booking bapConfig booking.searchId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city filteredDCategories
        logDebug $ "FRFS ConfirmReq " <> encodeToText bknConfirmReq
        void $ CallFRFSBPP.confirm providerUrl bknConfirmReq merchant.id
    _ -> do
      fork "FRFS External Confirm Req" $ do
        result <- withTryCatch "callExternalBPP:confirmFlow" $ do
          frfsConfig <-
            CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
              >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
          onConfirmReq <- Flow.confirm merchant merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories
          onConfirmHandler onConfirmReq
        case result of
          Left err -> do
            case fromException err :: Maybe CRISError of
              Just crisError -> void $ QFRFSTicketBooking.updateFailureReasonById (Just crisError.errorMessage) booking.id
              Nothing -> logError $ "FRFS External Confirm failed with error: " <> show err
            void $ QFRFSTicketBooking.updateStatusById DBooking.FAILED booking.id
            throwM err
          Right _ -> return ()

status ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String]
  ) =>
  Id Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  DBooking.FRFSTicketBooking ->
  m ()
status merchantId merchantOperatingCity bapConfig booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      Redis.setExp (Utils.mkCheckInprogressKey booking.searchId.getId) False 86400 -- 24hours
      void $ CallFRFSBPP.callBPPStatus booking bapConfig merchantOperatingCity.city merchantId
    _ -> do
      onStatusReq <- Flow.status merchantId merchantOperatingCity integratedBPPConfig bapConfig booking
      void $ processOnStatus onStatusReq
  where
    processOnStatus ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        MonadFlow m,
        EncFlow m r,
        SchedulerFlow r,
        EsqDBReplicaFlow m r,
        HasLongDurationRetryCfg r c,
        HasShortDurationRetryCfg r c,
        CallFRFSBPP.BecknAPICallFlow m r,
        HasFlowEnv m r '["googleSAPrivateKey" ::: String]
      ) =>
      DOrder ->
      m DOnStatus.DOnStatusResp
    processOnStatus onStatusReq = do
      let bookingOnStatusReq = DOnStatus.Booking onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest bookingOnStatusReq
      DOnStatus.onStatus merchant' booking' bookingOnStatusReq

verifyTicket :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Flow DFRFSTicket.FRFSTicket
verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory encryptedQrData platformType = do
  -- TODO: Add support for multiple integratedBPPConfigs, when we have multiple integratedBPPConfigs integrating verifyTicket API
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) platformType
  onStatusReq <- Flow.verifyTicket merchantId merchantOperatingCity integratedBPPConfig bapConfig encryptedQrData
  processOnStatus onStatusReq >>= \case
    DOnStatus.TicketVerificationSync ticket -> return ticket
    _ -> throwError $ InvalidRequest "Unsupported OnStatus Response."
  where
    processOnStatus onStatusReq = do
      let verificationOnStatusReq = DOnStatus.TicketVerification onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest verificationOnStatusReq
      void $ status merchantId merchantOperatingCity bapConfig booking' -- Doing one status call to sync the Ticket Status & Expiry Before Verifying
      DOnStatus.onStatus merchant' booking' verificationOnStatusReq
