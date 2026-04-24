module Domain.Action.Beckn.FRFS.OnCancel.Core (onCancelCore) where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.Beckn.FRFS.OnCancel
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBookingStatus as FTBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Domain.Types.Merchant as Merchant
import Kernel.External.Encryption
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.FRFSCancel as FRFSCancel
import SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Tools.Metrics as Metrics
import qualified UrlShortner.Common as UrlShortner

onCancelCore ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    SchedulerFlow r,
    CallFRFSBPP.BecknAPICallFlow m r,
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text],
    HasShortDurationRetryCfg r c,
    HasLongDurationRetryCfg r c
  ) =>
  Merchant ->
  Booking.FRFSTicketBooking ->
  DOnCancel ->
  m (Maybe (Maybe Text, Maybe Text, FRFSUtils.FRFSFareParameters))
onCancelCore merchant booking' dOnCancel = do
  let booking = booking' {Booking.bppOrderId = Just dOnCancel.bppOrderId}
  let refundAmount = fromMaybe (negate dOnCancel.baseFare) dOnCancel.refundAmount
  let cancellationCharges = fromMaybe 0 dOnCancel.cancellationCharges
  case dOnCancel.orderStatus of
    Spec.SOFT_CANCELLED -> do
      void $ QTBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just refundAmount) (Just cancellationCharges) (Just True) booking.id
      return Nothing
    Spec.CANCELLED -> do
      sideEffectData <- FRFSCancel.handleCancelledStatus merchant booking refundAmount cancellationCharges dOnCancel.messageId False
      return (Just sideEffectData)
    Spec.CANCEL_INITIATED -> do
      void $ QTBooking.updateStatusById FTBooking.CANCEL_INITIATED booking.id
      void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCEL_INITIATED) booking.id
      return Nothing
    _ -> throwError $ InvalidRequest "Unexpected orderStatus received"
