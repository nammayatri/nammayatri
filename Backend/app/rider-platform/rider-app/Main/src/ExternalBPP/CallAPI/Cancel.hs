module ExternalBPP.CallAPI.Cancel where

import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Tools.Error
import qualified Tools.Metrics as Metrics

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
        let requestCity = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
        bknCancelReq <- ACL.buildCancelReq messageId booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} frfsConfig.cancellationReasonId cancellationType requestCity
        logDebug $ "FRFS CancelReq " <> encodeToText bknCancelReq
        void $ CallFRFSBPP.cancel providerUrl bknCancelReq merchant.id
    _ -> return ()
