module Domain.Action.UI.SharedCab
  ( postSharedCabBookingSkip,
    skipReason,
  )
where

import qualified API.Types.UI.SharedCab as API
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (CONFIRMED))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation as Allocation
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import SharedLogic.SharedCab.Allocation.Types (SkipReason (..), usesRiderSkip)
import qualified SharedLogic.SharedCab.Approach as Approach
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.LegState (SkipVerdict (..), decideRiderSkip)
import SharedLogic.SharedCab.RateLimit (RiderAction (RiderSkip), riderActionsLeft)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error

skipReason :: API.SharedCabSkipReason -> SkipReason
skipReason = \case
  API.FULL -> SkipFull
  API.OTHER -> SkipOther

-- | "Skip this cab": up to maxSkipsAndCancels per search, only while the allocated cab is still coming and further than skipMinEtaSec
-- from the board stop (never after boarding). The booking goes back to FINDING, that cab isn't offered to it again, and nobody is charged.
postSharedCabBookingSkip :: (Maybe (Id DP.Person), Id DM.Merchant) -> Id DFTB.FRFSTicketBooking -> API.SharedCabSkipReq -> Environment.Flow APISuccess
postSharedCabBookingSkip (mbPersonId, _) bookingId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Booking not found")
  unless (booking.riderId == personId && isSharedCabBooking booking && booking.status == CONFIRMED) $
    throwError $ InvalidRequest "Booking not found"
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  let reason = skipReason req.reason
  cfg <- Config.getTunables booking.merchantOperatingCityId
  -- the skip limit and distance rules are for the rider's own choice; "it arrived full" is the cab's fault and is never limited
  when (usesRiderSkip reason) $ do
    unlessM (riderActionsLeft cfg.maxSkipsAndCancels RiderSkip booking.searchId.getId) $ throwError SharedCabSkipUsed
    when (DFRFSTicket.INPROGRESS `elem` map (.status) tickets) $ throwError SharedCabSkipTooLate
  plate <- Select.allocatedPlate (Select.LiveBooking booking (map (.status) tickets)) & fromMaybeM (InvalidRequest "This booking has no cab to skip")
  when (usesRiderSkip reason) $ do
    now <- getCurrentTime
    mbEta <- maybe (pure Nothing) (\routeCode -> Approach.etaToStopSec now cfg booking.integratedBppConfigId routeCode plate booking.fromStationCode) booking.routeCode
    when (decideRiderSkip cfg.skipMinEtaSec False False mbEta /= SkipOk) $ throwError SharedCabSkipTooLate
  skipped <- Allocation.skipSharedCabAllocation cfg booking.id booking.searchId.getId plate reason
  unless skipped $ throwError $ InvalidRequest "This booking has no cab to skip"
  pure Success
