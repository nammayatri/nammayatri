module ExternalBPP.CallAPI.Cancel where

import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.Beckn.FRFS.OnCancel.Core as OnCancelCore
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified ExternalBPP.Flow as Flow
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.CachedQueries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import Storage.ConfigPilot.Config.FRFSConfig (FRFSConfigDimensions (..))
import Tools.Error
import qualified Tools.Metrics as Metrics
import qualified UrlShortner.Common as UrlShortner

data CancellationInitiator = UserInitiated | Technical
  deriving (Eq, Show)

-- Caller should handle sideEffectData and call cancelJourney based on the cancellationType
cancel ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Finance.HasActorInfo m r,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text],
    HasMasterCloudForwarder r
  ) =>
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  Spec.CancellationType ->
  CancellationInitiator ->
  DBooking.FRFSTicketBooking ->
  m (Maybe (Maybe Text, Maybe Text, FRFSUtils.FRFSFareParameters, DBooking.FRFSTicketBooking))
cancel merchant merchantOperatingCity bapConfig cancellationType initiator booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  frfsConfig <-
    getConfig (FRFSConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId}) Nothing
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
  let (userCancellationAllowed, technicalCancellationAllowed) = SIBC.frfsCancellationFlags integratedBPPConfig
      providerCancellationAllowed = case initiator of
        UserInitiated -> userCancellationAllowed
        Technical -> technicalCancellationAllowed
  unless (frfsConfig.isCancellationAllowed && providerCancellationAllowed) $ throwError CancellationNotSupported
  let mbServiceTierType = FRFSUtils.getServiceTierTypeFromRouteStationsJson booking.routeStationsJson
  whenJust mbServiceTierType $ \serviceTierType -> do
    mbVst <- QFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTierType merchantOperatingCity.id integratedBPPConfig.id
    unless (fromMaybe True (mbVst >>= (.isCancellable))) $ throwError CancellationNotSupported
    -- Cap how often a rider can cancel. Only a rider driven confirm cancel is refused here, so a
    -- soft cancel quote never trips the cap. The counter itself is incremented once the booking
    -- reaches CANCELLED in FRFSCancel.handleCancelledStatus, which is also reached by technical
    -- and operator cancellations, so those consume the rider's allowance too.
    when (initiator == UserInitiated && cancellationType == Spec.CONFIRM_CANCEL) $
      whenJust ((,) <$> (mbVst >>= (.maxCancellationCount)) <*> (mbVst >>= (.cancellationWindowSeconds))) $ \(cancellationLimit, windowSeconds) -> do
        cancellationCount <- FRFSUtils.getCancellationCountInWindow booking windowSeconds
        when (cancellationCount >= cancellationLimit) $ do
          logInfo $ "FRFS cancellation quota exhausted for riderId-" <> booking.riderId.getId <> " count: " <> show cancellationCount <> " limit: " <> show cancellationLimit
          retryAfterSeconds <- FRFSUtils.getCancellationRetryAfterSeconds booking windowSeconds (cancellationCount - cancellationLimit)
          throwError $ FRFSCancellationLimitReached retryAfterSeconds
  when (cancellationType == Spec.SOFT_CANCEL) $
    unless (booking.status == DFRFSTicketBooking.CONFIRMED) $ throwError (InvalidRequest $ "Cancellation during incorrect status: " <> show booking.status)
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Cancel Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl")
        messageId <- generateGUID
        when (cancellationType == Spec.CONFIRM_CANCEL) $ Redis.setExp (FRFSUtils.makecancelledTtlKey booking.id) messageId ttl
        let requestCity = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
        bknCancelReq <- ACL.buildCancelReq messageId booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} frfsConfig.cancellationReasonId cancellationType requestCity
        logDebug $ "FRFS CancelReq " <> encodeToText bknCancelReq
        void $ CallFRFSBPP.cancel providerUrl bknCancelReq merchant.id
      return Nothing
    _ -> do
      onCancelReq <- Flow.cancel merchant merchantOperatingCity integratedBPPConfig bapConfig cancellationType booking
      mbSideEffectData <- OnCancelCore.onCancelCore merchant booking onCancelReq
      let updatedBooking = booking {DBooking.bppOrderId = Just onCancelReq.bppOrderId}
      return $ fmap (\(a, b, c) -> (a, b, c, updatedBooking)) mbSideEffectData
