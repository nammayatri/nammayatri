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
unskipDays :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> [DT.Day] -> Flow ()
unskipDays personId merchantId purchasedPassId days =
  forEachTerm personId merchantId purchasedPassId days $ \_ payment termDays -> do
    let updated = Rules.removeSkipDays payment.skipDays (map fst termDays)
    when (length updated /= length payment.skipDays) $
      QPPP.updateSkipDaysById updated payment.id

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
  forM_ (Map.toList $ Map.fromListWith (<>) termByDay) $ \(termId, termDays) ->
    Redis.whenWithLockRedisAndReturnValue (lockKey termId) 10 (updateFresh timeDiff termId termDays) >>= \case
      Left _ -> throwError (InvalidRequest "This pass is being updated, please try again")
      Right () -> pure ()
  where
    updateFresh timeDiff termId termDays = do
      term <- QPPP.findByPrimaryKey termId >>= fromMaybeM (InvalidRequest $ "Term not found: " <> termId.getId)
      update timeDiff term termDays

lockKey :: Id DPPP.PurchasedPassPayment -> Text
lockKey termId = "ShuttlePassSkipDays:PurchasedPassPaymentId:" <> termId.getId
