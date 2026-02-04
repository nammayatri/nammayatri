module ExternalBPP.CallAPI.Confirm where

import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError (..))
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.PTCircuitBreaker as CB
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error
import qualified Tools.Metrics as Metrics
import qualified UrlShortner.Common as UrlShortner

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
    Metrics.HasBAPMetrics m r,
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  (Maybe Text, Maybe Text) ->
  DBooking.FRFSTicketBooking ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  Maybe Bool ->
  m ()
confirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbIsSingleMode = do
  Metrics.startMetrics Metrics.CONFIRM_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Confirm Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        let filteredDCategories :: [DCategorySelect] =
              mapMaybe
                ( \category -> do
                    if category.selectedQuantity > 0
                      then Just $ DCategorySelect {bppItemId = category.bppItemId, quantity = category.selectedQuantity, category = category.category, price = fromMaybe category.offeredPrice category.finalPrice}
                      else Nothing
                )
                quoteCategories
        let requestCity = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
        bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) booking bapConfig booking.searchId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} requestCity filteredDCategories
        logDebug $ "FRFS ConfirmReq " <> encodeToText bknConfirmReq
        void $ CallFRFSBPP.confirm providerUrl bknConfirmReq merchant.id
    _ -> do
      fork "FRFS External Confirm Req" $ do
        let ptMode = CB.vehicleCategoryToPTMode booking.vehicleType
        mRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, txnId = Nothing})
        let circuitOpen = CB.isCircuitOpen ptMode CB.BookingAPI mRiderConfig
        let cbConfig = CB.parseCircuitBreakerConfig (mRiderConfig >>= (.ptCircuitBreakerConfig))

        result <- withTryCatch "callExternalBPP:confirmFlow" $ do
          frfsConfig <-
            CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
              >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
          onConfirmReq <- Flow.confirm merchant merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbIsSingleMode
          processOnConfirm onConfirmReq
        case result of
          Left err -> do
            CB.recordFailure ptMode CB.BookingAPI merchantOperatingCity.id
            CB.checkAndDisableIfNeeded ptMode CB.BookingAPI merchantOperatingCity.id cbConfig
            case fromException err :: Maybe CRISError of
              Just crisError -> void $ QFRFSTicketBooking.updateFailureReasonById (Just crisError.errorMessage) booking.id
              Nothing -> logError $ "FRFS External Confirm failed with error: " <> show err
            void $ QFRFSTicketBooking.updateStatusById DBooking.FAILED booking.id
            throwM err
          Right _ -> do
            when circuitOpen $ CB.reEnableCircuit ptMode CB.BookingAPI merchantOperatingCity.id
            CB.recordSuccess ptMode CB.BookingAPI merchantOperatingCity.id
            return ()
  where
    processOnConfirm ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        MonadFlow m,
        EncFlow m r,
        SchedulerFlow r,
        EsqDBReplicaFlow m r,
        HasLongDurationRetryCfg r c,
        HasShortDurationRetryCfg r c,
        CallFRFSBPP.BecknAPICallFlow m r,
        Metrics.HasBAPMetrics m r,
        HasField "ltsHedisEnv" r Redis.HedisEnv,
        HasFlowEnv m r '["smsCfg" ::: SmsConfig],
        HasFlowEnv m r '["googleSAPrivateKey" ::: String],
        HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
        HasField "isMetroTestTransaction" r Bool,
        HasField "blackListedJobs" r [Text]
      ) =>
      DOrder ->
      m ()
    processOnConfirm onConfirmReq = do
      (merchant', booking'', quoteCategories') <- DACFOC.validateRequest onConfirmReq
      DACFOC.onConfirm merchant' booking'' quoteCategories' onConfirmReq
