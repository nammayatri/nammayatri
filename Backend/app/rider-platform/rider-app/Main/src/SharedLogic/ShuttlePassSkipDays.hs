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
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPPP
import Tools.Error

-- | Mark days as skipped. Each day must still be open (more than 5 hours before the slot departs), inside a
-- live term of this pass that has a slot, and not already have a ticket (cancel that ticket instead).
skipDays :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> [DT.Day] -> Maybe Text -> Flow ()
skipDays personId merchantId purchasedPassId days reason =
  forEachTerm personId merchantId purchasedPassId days $ \payment termDays -> do
    forM_ termDays $ \(day, mbDeparture) ->
      whenJust mbDeparture $ \departure -> do
        issued <- ShuttlePass.hasLiveTicket payment departure
        when issued $
          throwError (InvalidRequest $ "A ticket is already issued for " <> show day <> "; cancel it instead")
    let alreadySkipped = map (.day) payment.skipDays
        added = [DPPP.SkipDay {day, reason} | (day, _) <- termDays, day `notElem` alreadySkipped]
    unless (null added) $
      QPPP.updateSkipDaysById (payment.skipDays <> added) payment.id

-- | Undo skips, under the same cutoff. Days that were not skipped are ignored.
unskipDays :: Id DP.Person -> Id DM.Merchant -> Id DPurchasedPass.PurchasedPass -> [DT.Day] -> Flow ()
unskipDays personId merchantId purchasedPassId days =
  forEachTerm personId merchantId purchasedPassId days $ \payment termDays -> do
    let removed = map fst termDays
        kept = filter ((`notElem` removed) . (.day)) payment.skipDays
    when (length kept /= length payment.skipDays) $
      QPPP.updateSkipDaysById kept payment.id

-- | Shared checks, then runs the update once per term, under that term's lock and on a fresh read of it.
-- Each day is passed with its slot departure when the timetable knows it.
forEachTerm ::
  Id DP.Person ->
  Id DM.Merchant ->
  Id DPurchasedPass.PurchasedPass ->
  [DT.Day] ->
  (DPPP.PurchasedPassPayment -> [(DT.Day, Maybe UTCTime)] -> Flow ()) ->
  Flow ()
forEachTerm personId merchantId purchasedPassId requestedDays update = do
  let days = nub requestedDays
  when (null days) $ throwError (InvalidRequest "days must not be empty")
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId && purchasedPass.merchantId == merchantId) $ throwError AccessDenied
  timeDiff <- ShuttlePass.cityTimeDiff purchasedPass.merchantOperatingCityId
  now <- getCurrentTime
  let today = ShuttlePass.localDay timeDiff now
  whenJust (find (< today) days) $ \day ->
    throwError (InvalidRequest $ "Past days cannot be changed: " <> show day)
  terms <-
    filter (isJust . (.slotTripId))
      <$> QPPP.findAllByPurchasedPassIdAndStatus Nothing Nothing purchasedPassId [DPurchasedPass.Active, DPurchasedPass.PreBooked] today
  termByDay <- forM days $ \day ->
    case find (\term -> term.startDate <= day && day <= term.endDate) terms of
      Just term -> do
        mbDeparture <- ShuttlePass.slotDepartureOn timeDiff term day
        let open = case mbDeparture of
              Just departure -> now < ShuttlePass.issueTimeOn departure
              Nothing -> day > today
        unless open $
          throwError (InvalidRequest $ "Changes for " <> show day <> " closed 5 hours before departure")
        pure (term.id, [(day, mbDeparture)])
      Nothing -> throwError (InvalidRequest $ "No shuttle term of this pass covers " <> show day)
  forM_ (Map.toList $ Map.fromListWith (<>) termByDay) $ \(termId, termDays) ->
    Redis.whenWithLockRedisAndReturnValue (lockKey termId) 10 (updateFresh termId termDays) >>= \case
      Left _ -> throwError (InvalidRequest "This pass is being updated, please try again")
      Right () -> pure ()
  where
    updateFresh termId termDays = do
      term <- QPPP.findByPrimaryKey termId >>= fromMaybeM (InvalidRequest $ "Term not found: " <> termId.getId)
      update term termDays

lockKey :: Id DPPP.PurchasedPassPayment -> Text
lockKey termId = "ShuttlePassSkipDays:PurchasedPassPaymentId:" <> termId.getId
