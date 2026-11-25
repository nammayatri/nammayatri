-- | Shuttle subscription: the rider marks days of a term they will not travel (skip / pause), or undoes that.
--
-- The days live on the term's payment row ('skipDays'); the ticket issue job does not issue a ticket for them.
-- The pass trip counter is not touched: it holds the trips still unused and drops only when a ticket is
-- booked, so a skipped day (no ticket) already keeps its trip.
module SharedLogic.ShuttlePassSkipDays
  ( skipDays,
    unskipDays,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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
import qualified SharedLogic.ShuttlePass as ShuttlePass
import qualified SharedLogic.ShuttlePassRules as Rules
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPPP
import Tools.Error

-- | Mark days as skipped. Each day must still be open (before its change cutoff), inside a live term of this
-- pass that has a slot, and not already have a ticket (cancel that ticket instead).
skipDays :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> [DT.Day] -> Maybe Text -> Flow ()
skipDays personId merchantId purchasedPassId days reason = do
  whenJust reason $ \r ->
    when (T.length r > Rules.maxReasonLength) $
      throwError (InvalidRequest $ "reason must be at most " <> show Rules.maxReasonLength <> " characters")
  forEachTerm personId merchantId purchasedPassId days $ \timeDiff payment termDays -> do
    forM_ termDays $ \(day, mbDeparture) -> do
      tickets <- ShuttlePass.ticketsForDay timeDiff payment day mbDeparture
      unless (null tickets) $
        throwError (InvalidRequest $ "A ticket is already issued for " <> show day <> "; cancel it instead")
    let updated = Rules.addSkipDays payment.skipDays (map fst termDays) reason
    when (length updated /= length payment.skipDays) $
      QPPP.updateSkipDaysById updated payment.id

-- | Undo skips, under the same cutoff. Days that were not skipped are ignored.
--
-- A skipped day's seat may since have gone to a rider who moved onto that slot, so where ops set the slot's seats the
-- day comes back only while it still has room, checked under the slot's lock as a reschedule is. When it is full the
-- rider can move the day to another slot instead (which also un-skips it).
unskipDays :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> [DT.Day] -> Flow ()
unskipDays personId merchantId purchasedPassId days =
  forEachTerm personId merchantId purchasedPassId days $ \_ payment termDays -> do
    let skipped = map (.day) payment.skipDays
        unskipping = [day | (day, _) <- termDays, day `elem` skipped]
        updated = Rules.removeSkipDays payment.skipDays (map fst termDays)
        cityId = payment.merchantOperatingCityId
        slotDays = [(slot, day) | day <- unskipping, Just slot <- [Rules.slotOn payment day]]
        -- Capacity is read under the slot locks, so the seat check uses the value in force while they are held.
        -- A slot with no capacity set is not gated, as before.
        checkAndWrite = do
          forM_ slotDays $ \(slot, day) ->
            ShuttlePass.slotCapacity cityId slot >>= \case
              Nothing -> pure ()
              Just capacity -> do
                booked <- ShuttlePass.ridersOnSlot cityId slot day
                unless (Rules.hasFreeSeat capacity booked) $
                  throwError (InvalidRequest $ "No seat left on your shuttle for " <> show day <> "; choose another shuttle for that day")
          when (length updated /= length payment.skipDays) $
            QPPP.updateSkipDaysById updated payment.id
    foldr (\(slot, day) inner -> lockOrBusy (ShuttlePass.slotLockKey cityId slot day) inner) checkAndWrite slotDays

-- | Shared checks, then runs the update once per term, under that term's lock and on a fresh read of it.
-- Each day is passed with its slot departure when the timetable knows it.
forEachTerm ::
  Id DP.Person ->
  Id DM.Merchant ->
  Id DPurchasedPass.PurchasedPass ->
  [DT.Day] ->
  (Seconds -> DPPP.PurchasedPassPayment -> [(DT.Day, Maybe UTCTime)] -> Flow ()) ->
  Flow ()
forEachTerm personId merchantId purchasedPassId requestedDays update = do
  let days = nub requestedDays
  when (null days) $ throwError (InvalidRequest "days must not be empty")
  when (length days > Rules.maxDaysPerRequest) $
    throwError (InvalidRequest $ "At most " <> show Rules.maxDaysPerRequest <> " days per request")
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId && purchasedPass.merchantId == merchantId) $ throwError AccessDenied
  timeDiff <- ShuttlePass.cityTimeDiff purchasedPass.merchantOperatingCityId
  now <- getCurrentTime
  let today = Rules.localDay timeDiff now
  whenJust (find (< today) days) $ \day ->
    throwError (InvalidRequest $ "Past days cannot be changed: " <> show day)
  terms <-
    filter (isJust . (.slotTripId))
      <$> QPPP.findAllByPurchasedPassIdAndStatus Nothing Nothing purchasedPassId [DPurchasedPass.Active, DPurchasedPass.PreBooked] today
  termByDay <- forM days $ \day ->
    case find (\term -> term.startDate <= day && day <= term.endDate) terms of
      Just term -> do
        mbDeparture <- ShuttlePass.slotDepartureOn timeDiff term day
        unless (Rules.dayOpenForChange now today mbDeparture day) $
          throwError (InvalidRequest $ "Changes for " <> show day <> " closed 5 hours before departure")
        pure (term.id, [(day, mbDeparture)])
      Nothing -> throwError (InvalidRequest $ "No shuttle term of this pass covers " <> show day)
  -- 60s: far longer than the fresh reads and one write it covers, so it cannot lapse mid-update and let the issue job
  -- in. It is released as soon as the update finishes, so the length only matters if this process dies holding it.
  forM_ (Map.toList $ Map.fromListWith (<>) termByDay) $ \(termId, termDays) ->
    lockOrBusy (ShuttlePass.termLockKey termId) (updateFresh timeDiff termId termDays)
  where
    -- Under the lock: a fresh clock (the departure lookups above take time, and the cutoff may have passed
    -- meanwhile) and a fresh read of the term.
    updateFresh timeDiff termId termDays = do
      now <- getCurrentTime
      let today = Rules.localDay timeDiff now
      forM_ termDays $ \(day, mbDeparture) ->
        unless (Rules.dayOpenForChange now today mbDeparture day) $
          throwError (InvalidRequest $ "Changes for " <> show day <> " closed 5 hours before departure")
      term <- QPPP.findByPrimaryKey termId >>= fromMaybeM (InvalidRequest $ "Term not found: " <> termId.getId)
      update timeDiff term termDays

-- | Try-lock for 60s (see above); a busy lock fails the request rather than waiting.
lockOrBusy :: Text -> Flow () -> Flow ()
lockOrBusy key action =
  Redis.whenWithLockRedisAndReturnValue key 60 action >>= \case
    Left _ -> throwError (InvalidRequest "This pass is being updated, please try again")
    Right () -> pure ()
