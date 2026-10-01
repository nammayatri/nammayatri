module Domain.Action.UI.TollChargeApproval
  ( TollChargeApprovalDecisionReq (..),
    TollChargeApprovalRequestRes (..),
    getTollChargeApproval,
    getPendingTollChargeApproval,
    acknowledgeSurfacedTollChargeApproval,
    tollChargeApprovalDecision,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema)
import qualified Domain.Action.Internal.TollChargeApproval as Internal
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Extra.Ride (TollChargeApprovalRequestRes (..))
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as SMerchant
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics (CoreMetrics)

data TollChargeApprovalDecisionReq = TollChargeApprovalDecisionReq
  { approved :: Bool,
    amount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- The rider app reads this when it opens the prompt from the push or comes back to the app.
getTollChargeApproval :: Id DP.Person -> Id DRide.Ride -> Flow (Maybe TollChargeApprovalRequestRes)
getTollChargeApproval personId rideId = do
  ride <- findOwnedRide personId rideId
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  mbPending <- getPendingTollChargeApproval rideId
  whenJust mbPending $ \_ -> acknowledgeSurfacedTollChargeApproval booking rideId
  pure mbPending

-- Tells the driver side the prompt was shown. A failure is logged and retried on the next read.
-- Polymorphic (not pinned to Flow) so a non-Flow-typed response builder like buildRideAPIEntity can call it too.
acknowledgeSurfacedTollChargeApproval ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  DBooking.Booking ->
  Id DRide.Ride ->
  m ()
acknowledgeSurfacedTollChargeApproval booking rideId = do
  now <- getCurrentTime
  mbPending :: Maybe Internal.PendingTollChargeApproval <- Redis.get' (Internal.pendingTollChargeApprovalKey rideId) (pure ())
  case mbPending of
    Just pending
      | Just requestId <- pending.requestId,
        pending.deliveryAcked /= Just True,
        addUTCTime (fromIntegral pending.approvalTimeoutSeconds) pending.requestedAt > now -> do
        ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
        merchant <- SMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
        ackResult <- withTryCatch "ackTollChargeApproval" $ CallBPPInternal.ackTollChargeApproval merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId requestId
        case ackResult of
          Right _ -> Redis.setExp (Internal.pendingTollChargeApprovalKey rideId) pending {Internal.deliveryAcked = Just True} Internal.pendingTollChargeApprovalRedisTtlSec
          Left err -> logWarning $ "Could not acknowledge toll charge approval for ride " <> rideId.getId <> ": " <> show err
    _ -> pure ()

-- Shared with the already-continuous ride-status polls, so none of them need a second poll just
-- for this. Ownership of the ride is the caller's concern.
getPendingTollChargeApproval :: (MonadFlow m, CacheFlow m r) => Id DRide.Ride -> m (Maybe TollChargeApprovalRequestRes)
getPendingTollChargeApproval rideId = do
  now <- getCurrentTime
  mbPending :: Maybe Internal.PendingTollChargeApproval <- Redis.safeGet (Internal.pendingTollChargeApprovalKey rideId)
  pure $ do
    pending <- mbPending
    let expiresAt = addUTCTime (fromIntegral pending.approvalTimeoutSeconds) pending.requestedAt
    guard (expiresAt > now)
    pure
      TollChargeApprovalRequestRes
        { tollNames = pending.tollNames,
          amount = pending.amount,
          currency = pending.currency,
          requestedAt = pending.requestedAt,
          expiresAt = expiresAt
        }

-- The BPP owns the ride and holds the decision, so this only checks the rider and the request and
-- passes the decision on.
tollChargeApprovalDecision :: Id DP.Person -> Id DRide.Ride -> TollChargeApprovalDecisionReq -> Flow APISuccess
tollChargeApprovalDecision personId rideId req = do
  ride <- findOwnedRide personId rideId
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  now <- getCurrentTime
  pending :: Internal.PendingTollChargeApproval <- Redis.safeGet (Internal.pendingTollChargeApprovalKey rideId) >>= fromMaybeM (InvalidRequest "There is no toll charge waiting for your approval")
  unless (pending.amount == req.amount && addUTCTime (fromIntegral pending.approvalTimeoutSeconds) pending.requestedAt > now) $
    throwError $ InvalidRequest "This toll charge request is no longer valid"
  requestId <- fromMaybeM (InvalidRequest "This toll charge request is no longer valid") pending.requestId
  merchant <- SMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  void $ CallBPPInternal.submitTollChargeApprovalDecision merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId requestId req.approved req.amount
  Redis.del (Internal.pendingTollChargeApprovalKey rideId)
  pure Success

findOwnedRide :: Id DP.Person -> Id DRide.Ride -> Flow DRide.Ride
findOwnedRide personId rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
  pure ride
