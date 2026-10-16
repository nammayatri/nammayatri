-- | The tick's time-driven closes: expired or lost alloc-key timers, silent cabs, and the time-based board-any fallback push.
module SharedLogic.SharedCab.Allocation.Timers
  ( expireTimers,
    pushTimeFallbacks,
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow, claimFallbackPush, warnAllocKeyTtlOnce)
import SharedLogic.SharedCab.Allocation.Reallocate (afterClose, closeLocked)
import SharedLogic.SharedCab.Allocation.Select (FindingBooking (..), allocatedPlate)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Keys (allocKey)
import SharedLogic.SharedCab.LegState (fallbackTimeElapsed)
import qualified SharedLogic.SharedCab.Notify as Notify

-- | A timed-out or key-less ALLOCATED booking goes back to FINDING, decided under the booking lock so a claim still writing its key is not mistaken for lost.
expireTimers ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  UTCTime ->
  (Text -> Text -> Bool) -> -- plate moving on route, per fresh LTS positions
  (Text -> Text -> Bool) -> -- plate silent on route
  [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])] ->
  m ()
expireTimers cfg now movingOn silentOn live =
  forM_ [(b, plate) | entry@(b, _) <- live, Just plate <- [allocatedPlate entry]] $ \(b, plate) -> do
    result <- withBookingLock b.id $ do
      mbState <- shared $ Redis.safeGet (allocKey b.id)
      case timerExpiry now mbState <|> (CabSilent <$ guard (maybe False (silentOn plate) b.routeCode)) of
        Just outcome -> fmap (outcome,) <$> closeLocked cfg b.id plate outcome
        Nothing -> do
          whenJust mbState $ \st ->
            when (clearOnMove st.timer /= st.timer && maybe False (movingOn plate) b.routeCode) $ do
              warnAllocKeyTtlOnce cfg "timer-clear"
              shared $ Redis.setExp (allocKey b.id) st {timer = clearOnMove st.timer} (allocKeyTtl cfg)
          pure Nothing
    whenJust result $ \(outcome, closedInfo) -> afterClose cfg b.id plate outcome (Just closedInfo)

pushTimeFallbacks :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [(DFTB.FRFSTicketBooking, FindingBooking)] -> m ()
pushTimeFallbacks cfg now findings =
  forM_ [b | (b, fb) <- findings, fallbackTimeElapsed now fb.findingSince cfg.fallbackAfterSec] $ \b ->
    whenM (claimFallbackPush cfg b.id) $ Notify.notifyBoardAny b
