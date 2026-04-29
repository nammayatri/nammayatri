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
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.CachedQueries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import Storage.ConfigPilot.Config.FRFSConfig (FRFSConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import Tools.Error
import qualified Tools.Metrics as Metrics
import qualified UrlShortner.Common as UrlShortner

-- Caller should handle sideEffectData and call cancelJourney based on the cancellationType
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
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  Spec.CancellationType ->
  DBooking.FRFSTicketBooking ->
  m (Maybe (Maybe Text, Maybe Text, FRFSUtils.FRFSFareParameters, DBooking.FRFSTicketBooking))
cancel merchant merchantOperatingCity bapConfig cancellationType booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  frfsConfig <-
    getConfig (FRFSConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId})
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  let mbServiceTierType = FRFSUtils.getServiceTierTypeFromRouteStationsJson booking.routeStationsJson
  whenJust mbServiceTierType $ \serviceTierType -> do
    mbVst <- QFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTierType merchantOperatingCity.id integratedBPPConfig.id
    unless (fromMaybe True (mbVst >>= (.isCancellable))) $ throwError CancellationNotSupported
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
