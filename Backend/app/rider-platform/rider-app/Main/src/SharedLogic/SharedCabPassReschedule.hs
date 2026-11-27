-- | Shuttle subscription: ride another slot on one day of a term (e.g. the 9:30 instead of the usual 7:30), when that
-- slot has a free seat.
--
-- The change lives on the term's payment row ('slotChanges'); the ticket issue job books that day on the new slot, and
-- the change cutoff, skip and the OTP timing all follow it. Seats are counted from the terms riding the slot that day
-- (its own riders and those who moved onto it, less skipped days) against the capacity ops set for the slot.
module SharedLogic.SharedCabPassReschedule
  ( reschedule,
    slotAvailability,
  )
where

import qualified API.Types.UI.Pass as PassAPI
import qualified Data.Time as DT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Environment (Flow)
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.RoutePassPricing as RoutePassPricing
import qualified SharedLogic.SharedCabPass as SharedCabPass
import qualified SharedLogic.SharedCabPassRules as Rules
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPPP
import qualified Storage.Queries.PurchasedPassPaymentExtra as QPPPExtra
import Tools.Error

-- | Moves the day onto the slot. Refused when the day is already ticketed, when either slot is past its change cutoff,
-- when the slot does not run from the rider's pickup to their drop, or when it has no free seat. A skipped day is
-- un-skipped by the move: the rider asked to ride it, and it is the way back onto a day whose own slot filled up.
reschedule :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> DT.Day -> Text -> Flow ()
reschedule personId merchantId purchasedPassId day newSlot = do
  (timeDiff, term) <- termOnDay personId merchantId purchasedPassId day
  if Rules.slotOn term day == Just newSlot && day `notElem` map (.day) term.skipDays
    then pure ()
    else do
      validateSlot term newSlot
      -- Slot lock first (it guards the seat count), then the term lock skip/unskip and the issue job take.
      -- Both are try-locks: a busy one fails the request, never waits, so the two orders elsewhere cannot deadlock.
      withLock (SharedCabPass.slotLockKey term.merchantOperatingCityId newSlot day) $
        withLock (SharedCabPass.termLockKey term.id) $ do
          -- Under the locks: a fresh read of the term and a fresh clock, since another change may have landed and the
          -- cutoff may have passed while the checks above ran.
          fresh <- QPPP.findByPrimaryKey term.id >>= fromMaybeM (InvalidRequest $ "Term not found: " <> term.id.getId)
          -- The term may have ended or been cut short (e.g. terminated from the dashboard) since it was first read.
          unless (fresh.status `elem` [DPurchasedPass.Active, DPurchasedPass.PreBooked] && fresh.startDate <= day && day <= fresh.endDate) $
            throwError (InvalidRequest $ "No shuttle term of this pass covers " <> show day)
          let wasSkipped = day `elem` map (.day) fresh.skipDays
          unless (Rules.slotOn fresh day == Just newSlot && not wasSkipped) $ do
            currentDeparture <- SharedCabPass.slotDepartureOn timeDiff fresh day
            newDeparture <-
              SharedCabPass.slotDepartureFor timeDiff fresh newSlot day
                >>= fromMaybeM (InvalidRequest "This shuttle does not stop at your pickup")
            now <- getCurrentTime
            unless (Rules.rescheduleOpen now (Rules.localDay timeDiff now) currentDeparture newDeparture day) $
              throwError (InvalidRequest $ "Changes for " <> show day <> " close 5 hours before departure")
            tickets <- SharedCabPass.ticketsForDay timeDiff fresh day currentDeparture
            unless (null tickets) $
              throwError (InvalidRequest $ "A ticket is already issued for " <> show day <> "; cancel it instead")
            -- Capacity is read under the slot lock too, so the seat check uses the value in force while the lock is held.
            capacity <-
              SharedCabPass.slotCapacity fresh.merchantOperatingCityId newSlot
                >>= fromMaybeM (InvalidRequest "Seats for this shuttle are not set up yet, please choose another")
            booked <- SharedCabPass.ridersOnSlot fresh.merchantOperatingCityId newSlot day
            unless (Rules.hasFreeSeat capacity booked) $
              throwError (InvalidRequest "No seats left on this shuttle, please choose another")
            let slotChanges = Rules.setSlotChange fresh.routeId fresh.slotChanges day newSlot
            if wasSkipped
              then QPPPExtra.updateSlotChangesAndSkipDaysById slotChanges (Rules.removeSkipDays fresh.skipDays [day]) fresh.id
              else QPPP.updateSlotChangesById slotChanges fresh.id
            logInfo $ "SharedCabPass: rescheduled term=" <> fresh.id.getId <> " day=" <> show day <> " slot=" <> newSlot <> " seats=" <> show (booked + 1) <> "/" <> show capacity

-- | What the app shows before the rider moves a day: the slot's departure from their pickup, its seats, and whether the
-- move would be accepted now. A read without locks, so a seat shown free can still be taken before the reschedule.
slotAvailability :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> DT.Day -> Text -> Flow PassAPI.PassSlotAvailabilityResp
slotAvailability personId merchantId purchasedPassId day slot = do
  (timeDiff, term) <- termOnDay personId merchantId purchasedPassId day
  validateSlot term slot
  mbDeparture <- SharedCabPass.slotDepartureFor timeDiff term slot day
  currentDeparture <- SharedCabPass.slotDepartureOn timeDiff term day
  capacity <- SharedCabPass.slotCapacity term.merchantOperatingCityId slot
  booked <- SharedCabPass.ridersOnSlot term.merchantOperatingCityId slot day
  now <- getCurrentTime
  let availableSeats = (\c -> max 0 (c - booked)) <$> capacity
      open = maybe False (\departure -> Rules.rescheduleOpen now (Rules.localDay timeDiff now) currentDeparture departure day) mbDeparture
      alreadyRiding = Rules.slotOn term day == Just slot && day `notElem` map (.day) term.skipDays
      canReschedule = open && not alreadyRiding && maybe False (> 0) availableSeats
  pure
    PassAPI.PassSlotAvailabilityResp
      { routeId = slot,
        day,
        departureTime = mbDeparture,
        capacity,
        booked,
        availableSeats,
        canReschedule
      }

-- | The caller's term with a slot that covers the day (today or later), and the city's offset from UTC.
termOnDay :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> DT.Day -> Flow (Seconds, DPPP.PurchasedPassPayment)
termOnDay personId merchantId purchasedPassId day = do
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId && purchasedPass.merchantId == merchantId) $ throwError AccessDenied
  timeDiff <- SharedCabPass.cityTimeDiff purchasedPass.merchantOperatingCityId
  now <- getCurrentTime
  let today = Rules.localDay timeDiff now
  when (day < today) $ throwError (InvalidRequest $ "Past days cannot be changed: " <> show day)
  terms <-
    filter (isJust . (.routeId))
      <$> QPPP.findAllByPurchasedPassIdAndStatus Nothing Nothing purchasedPassId [DPurchasedPass.Active, DPurchasedPass.PreBooked] today
  term <-
    find (\t -> t.startDate <= day && day <= t.endDate) terms
      & fromMaybeM (InvalidRequest $ "No shuttle term of this pass covers " <> show day)
  pure (timeDiff, term)

-- | The slot must run from the term's pickup to its drop, as at purchase.
validateSlot :: DPPP.PurchasedPassPayment -> Text -> Flow ()
validateSlot term slot = do
  sourceStopCode <- term.sourceStopCode & fromMaybeM (InvalidRequest "Term has no source stop")
  destinationStopCode <- term.destinationStopCode & fromMaybeM (InvalidRequest "Term has no destination stop")
  integratedBPPConfig <- SharedCabPass.termBppConfig term >>= fromMaybeM (InvalidRequest "Pass not found for this term")
  RoutePassPricing.validateSlotRoute integratedBPPConfig sourceStopCode destinationStopCode slot

-- | 60s: far longer than the reads and the one write it covers; released as soon as they finish.
withLock :: Text -> Flow () -> Flow ()
withLock key action =
  Redis.whenWithLockRedisAndReturnValue key 60 action >>= \case
    Left _ -> throwError (InvalidRequest "This pass is being updated, please try again")
    Right () -> pure ()
