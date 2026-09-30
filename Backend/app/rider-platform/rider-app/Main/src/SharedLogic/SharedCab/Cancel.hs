-- | R54: every shared-cab cancel, rider's or driver's, passes here. Under the booking lock it decides from a fresh read,
-- hands the refund to `ExternalBPP.Flow.Common.cancel`, runs the cancel and drops the allocation state.
module SharedLogic.SharedCab.Cancel
  ( CancelStage (..),
    withSharedCabCancel,
    guardRiderCancel,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.FRFSUtils (isPayOnBoard)
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Types (AllocationState (..))
import SharedLogic.SharedCab.Booking (readRiderFix, recordCancelReason, shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LegState (CancelReason (DRIVER, RIDER))
import SharedLogic.SharedCab.RefundDecision (Refund (..), clearRefundDecision, gatePayOnBoard, isSharedCabBooking, setRefundDecision)
import SharedLogic.SharedCab.RefundPolicy
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

-- | A soft cancel only quotes the refund; the confirm is what ends the booking.
data CancelStage = SoftCancel | ConfirmCancel
  deriving (Eq)

-- | `checkFresh` sees the booking as read under the lock (the driver's cancel checks it is still on their cab).
-- `cancelAction` must not take the booking lock itself.
withSharedCabCancel :: CancelBy -> CancelStage -> Maybe Text -> (DFTB.FRFSTicketBooking -> Flow ()) -> DFTB.FRFSTicketBooking -> Flow () -> Flow ()
withSharedCabCancel by stage mbReason checkFresh booking cancelAction = do
  refund <- withBookingLock booking.id $ do
    fresh <- QFRFSTicketBooking.findById booking.id >>= fromMaybeM (InvalidRequest "Booking not found")
    -- a canceller that lost the lock to another cancel (a rider's tap racing the finding timeout) must not refund again
    unless (cancellableStatus fresh.status) $ throwError (InvalidRequest "Booking is already cancelled")
    checkFresh fresh
    refund <- decide by fresh
    setRefundDecision booking.id refund
    (cancelAction >> when (stage == ConfirmCancel) (Allocation.clearAllocationKeys booking.id)) `finally` clearRefundDecision booking.id
    pure refund
  when (stage == ConfirmCancel) $ do
    -- R55: the leg-state surface shows why; the R54 no-show cap writes its own reason from the tick. Swallowed:
    -- the booking is cancelled by now, so a Redis hiccup here must not error a finished cancel.
    withTryCatch "sharedCab:cancel:recordCancelReason" (recordCancelReason booking.id (reasonFor by))
      >>= either (\err -> logError $ "shared-cab cancel-reason not recorded for booking " <> booking.id.getId <> ": " <> show err) pure
    whenJust mbReason $ \reason -> logInfo $ "shared-cab booking " <> booking.id.getId <> " cancelled by " <> byText by <> ": " <> T.take 200 reason
    Events.forBooking (Events.BookingCancelled (byText by) (refundText refund) mbReason) booking
  Invariants.checkBooking booking.id

-- | The rider's cancel of a booking through any endpoint: a shared-cab one goes through the policy, others run as is.
guardRiderCancel :: CancelStage -> DFTB.FRFSTicketBooking -> Flow () -> Flow ()
guardRiderCancel stage booking cancelAction
  | isSharedCabBooking booking = withSharedCabCancel ByRider stage Nothing (const $ pure ()) booking cancelAction
  | otherwise = cancelAction

decide :: CancelBy -> DFTB.FRFSTicketBooking -> Flow Refund
decide by booking = do
  tickets <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId booking.id
  now <- getCurrentTime
  mbAlloc <- shared $ Redis.safeGet @AllocationState (Allocation.allocKey booking.id.getId)
  tunables <- Config.getTunables booking.merchantOperatingCityId
  riderFix <- readRiderFix booking.id
  let state = cancelState now booking.vehicleNumber (mbAlloc >>= (.expiresAt))
      near = riderNearStop tunables.boardProximityM tunables.ltsMaxAgeSec now booking.fromStationPoint riderFix
  case decideCancel by state booking.sharedCabNoShows tickets near of
    Rejected err -> throwError err
    Allowed refund -> (\payOnBoard -> gatePayOnBoard payOnBoard refund) <$> isPayOnBoard booking

byText :: CancelBy -> Text
byText = \case
  ByRider -> "rider"
  ByDriver -> "driver"

reasonFor :: CancelBy -> CancelReason
reasonFor = \case
  ByRider -> RIDER
  ByDriver -> DRIVER

refundText :: Refund -> Text
refundText = \case
  FullRefund -> "full"
  NoRefund -> "none"
  NothingPaid -> "none"
