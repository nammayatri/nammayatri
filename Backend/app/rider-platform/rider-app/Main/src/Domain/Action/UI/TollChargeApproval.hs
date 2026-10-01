module Domain.Action.UI.TollChargeApproval
  ( TollChargeApprovalDecisionReq (..),
    TollChargeApprovalRequestRes (..),
    getTollChargeApproval,
    getPendingTollChargeApproval,
    tollChargeApprovalDecision,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Action.Internal.TollChargeApproval as Internal
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

data TollChargeApprovalDecisionReq = TollChargeApprovalDecisionReq
  { approved :: Bool,
    amount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- The rider app reads this when it opens the prompt from the push or comes back to the app.
getTollChargeApproval :: Id DP.Person -> Id DRide.Ride -> Flow (Maybe TollChargeApprovalRequestRes)
getTollChargeApproval personId rideId = do
  _ <- findOwnedRide personId rideId
  getPendingTollChargeApproval rideId

-- Shared with the booking-status poll, which already runs continuously for the ride's whole
-- duration: this lets that poll surface a pending toll approval too, instead of the rider app
-- needing a second, separate poll just for this. Ownership of the ride is the caller's concern.
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
  merchant <- SMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  void $ CallBPPInternal.submitTollChargeApprovalDecision merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId req.approved req.amount
  Redis.del (Internal.pendingTollChargeApprovalKey rideId)
  pure Success

findOwnedRide :: Id DP.Person -> Id DRide.Ride -> Flow DRide.Ride
findOwnedRide personId rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
  pure ride
