module ExternalBPP.CallAPI.Status where

import qualified Beckn.ACL.FRFS.Utils as Utils
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC

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
      let requestCity = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
      void $ CallFRFSBPP.callBPPStatus booking bapConfig requestCity merchantId
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
