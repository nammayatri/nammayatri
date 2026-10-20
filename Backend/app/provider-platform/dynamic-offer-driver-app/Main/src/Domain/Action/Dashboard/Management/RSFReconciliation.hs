{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Domain.Action.Dashboard.Management.RSFReconciliation
  ( getRSFReconciliationRsfOrders,
    getRSFReconciliationRsfSummary,
    getRSFReconciliationRsfUtrs,
    getRSFReconciliationRsfUtr,
    postRSFReconciliationRsfUtrBankVerify,
    postRSFReconciliationRsfUtrAdjust,
    postRSFReconciliationRsfAutoAllocation,
    postRSFReconciliationRsfOrdersConfirm,
    postRSFReconciliationRsfSend,
    getRSFReconciliationRsfReconUnmatched,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation as Res
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Time
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as M
import qualified Domain.Types.Ride as DRide
import qualified Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import qualified Lib.Finance.Domain.Types.RsfUtrState as DUtrState
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.RsfUtrState as QUtrState
import qualified SharedLogic.CallRSF as CallRSF
import qualified SharedLogic.RSFLedger as RSFLedger
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RsfLedgerAggregates as QAgg

-- What an order row is folded from, grouped once per request: standing claims and allocations
-- per order, the rides (fare) and, per order, the message its last answer was for. Two ledger-side reads
-- (claims+allocations by order id, rides) and one projection read.
data OrderView = OrderView
  { claimsByOrder :: Map.Map Text [RSFLedger.Entry],
    allocationsByOrder :: Map.Map Text [RSFLedger.Entry],
    rideByOrderId :: Map.Map Text DRide.Ride,
    lastReportedMessageByOrder :: Map.Map Text Text
  }

-- What a UTR summary is folded from: the UTR's ledger rows (claims, receipt, allocations),
-- its standing claims, and its projection row. One ledger read and one projection read.
data UtrView = UtrView
  { entriesByUtr :: Map.Map Text [RSFLedger.Entry],
    claimsByUtr :: Map.Map Text [RSFLedger.Entry],
    utrStates :: Map.Map Text DUtrState.RsfUtrState
  }

groupBy :: (RSFLedger.Entry -> Maybe Text) -> [RSFLedger.Entry] -> Map.Map Text [RSFLedger.Entry]
groupBy key = Map.fromListWith (flip (<>)) . mapMaybe (\e -> (,[e]) <$> key e)

loadOrderView :: Text -> [Text] -> Environment.Flow OrderView
loadOrderView merchantId orderIds = do
  orderEntries <- QLedger.findAllByMerchantAndOrderIds merchantId orderIds
  rideByOrderId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
  orderStates <- QOrderState.findAllByMerchantAndOrderIds merchantId orderIds
  pure
    OrderView
      { -- latest claim per (order, UTR) of any status: a refused revision is still the row to show
        claimsByOrder = Map.map (sortOn (.createdAt)) $ groupBy (.orderId) (Map.elems (RSFLedger.latestClaims orderEntries)),
        allocationsByOrder = groupBy (.orderId) (RSFLedger.ofType L.BANK_ALLOCATION orderEntries),
        rideByOrderId = rideByOrderId,
        lastReportedMessageByOrder = Map.fromList [(st.orderId, m) | st <- orderStates, Just m <- [st.lastReportedMessageId]]
      }

loadUtrView :: Text -> [Text] -> Environment.Flow UtrView
loadUtrView merchantId utrs = do
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId utrs
  utrStates <- QUtrState.findAllByMerchantAndUtrs merchantId utrs
  pure
    UtrView
      { entriesByUtr = groupBy (.utr) utrEntries,
        claimsByUtr = Map.map (sortOn (.createdAt)) $ groupBy (.utr) (Map.elems (RSFLedger.latestClaims utrEntries)),
        utrStates = Map.fromList [(s.utr, s) | s <- utrStates]
      }

findMerchantId :: ShortId M.Merchant -> Environment.Flow Text
findMerchantId merchantShortId = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  pure merchant.id.getId

dayRange :: Maybe Data.Time.Day -> Environment.Flow (UTCTime, UTCTime)
dayRange mbDay = RSFLedger.istDayRange <$> fromMaybeM (InvalidRequest "Missing 'date'") mbDay

-- The ledger actor for a money-changing call: the authenticated dashboard user the auth layer put in
-- the request context (withDashboardUserActorInfo), never a name supplied in the body.
operatorId :: Environment.Flow Text
operatorId = do
  actor <- asks (.actorInfo)
  case (actor.actorType, actor.actorId) of
    (Finance.DASHBOARD_PERSON, Just personId) -> pure personId
    _ -> throwError $ InvalidRequest "No authenticated dashboard user in the request context"

getRSFReconciliationRsfOrders :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Res.OrderListRes
getRSFReconciliationRsfOrders merchantShortId _opCity mbBapId mbDate mbLimit mbOffset = do
  merchantId <- findMerchantId merchantShortId
  (from, to) <- dayRange mbDate
  let limit = fromMaybe 50 mbLimit
      offset = fromMaybe 0 mbOffset
  -- page over rsf_order_state by the day of each order's latest claim; only the page is folded
  page <- QOrderState.findPageByMerchantAndLastClaimedAtRange merchantId from to mbBapId limit offset
  let orderIds = map (.orderId) page
  view <- loadOrderView merchantId orderIds
  pure $ Res.OrderListRes {totalItems = offset + length page, orders = mapMaybe (toOrderRow view) orderIds}

-- The day's tiles, folded from the same rows the two lists show.
getRSFReconciliationRsfSummary :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.Flow Res.DaySummaryRes
getRSFReconciliationRsfSummary merchantShortId _opCity mbDate = do
  merchantId <- findMerchantId merchantShortId
  (from, to) <- dayRange mbDate
  orderIds <- map (.orderId) <$> QOrderState.findAllByMerchantAndLastClaimedAtRange merchantId from to
  utrs <- map (.utr) <$> QUtrState.findAllByMerchantAndLastClaimedAtRange merchantId from to
  orderView <- loadOrderView merchantId orderIds
  utrView <- loadUtrView merchantId utrs
  let orders = mapMaybe (toOrderRow orderView) orderIds
      utrRows = mapMaybe (toUtrSummary utrView) utrs
      verified = filter (isJust . (.bankVerifiedAmount)) utrRows
      unverified = filter (isNothing . (.bankVerifiedAmount)) utrRows
      rejected = filter ((== Res.REJECTED) . (.orderVerdict)) orders
      mismatched = filter ((`elem` [Res.UNDERPAID, Res.OVERPAID]) . (.orderVerdict)) orders
      unsent = filter (not . (.allSent)) orders
      attention = nub (map (.orderId) (rejected <> mismatched <> unsent))
  pure
    Res.DaySummaryRes
      { claimedByCollector = sum (map (.claimedTotalAmount) orders),
        orderCount = length orders,
        utrCount = length utrRows,
        bankReceived = sum (mapMaybe (.bankVerifiedAmount) verified),
        utrsVerified = length verified,
        allocatedToOrders = sum (map (.receivedTotal) orders),
        unallocated = sum (map (.unallocatedAmount) verified),
        unallocatedUtrCount = length (filter ((> 0) . (.unallocatedAmount)) verified),
        awaitingBankFigure = sum (map (.claimedTotalAmount) unverified),
        unverifiedUtrCount = length unverified,
        needsAttention = length attention,
        rejectedOrders = length rejected,
        mismatchedOrders = length mismatched,
        unsentOrders = length unsent
      }

getRSFReconciliationRsfUtrs :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Res.UtrListRes
getRSFReconciliationRsfUtrs merchantShortId _opCity mbBapId mbDate mbLimit mbOffset = do
  merchantId <- findMerchantId merchantShortId
  (from, to) <- dayRange mbDate
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
  page <- QUtrState.findPageByMerchantAndLastClaimedAtRange merchantId from to mbBapId limit offset
  let utrs = map (.utr) page
  view <- loadUtrView merchantId utrs
  pure $ Res.UtrListRes {totalItems = offset + length page, utrs = mapMaybe (toUtrSummary view) utrs}

getRSFReconciliationRsfUtr :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow Res.UtrDetailRes
getRSFReconciliationRsfUtr merchantShortId _opCity utr = do
  merchantId <- findMerchantId merchantShortId
  utrView <- loadUtrView merchantId [utr]
  summary <- fromMaybeM (InvalidRequest "UTR not found") (toUtrSummary utrView utr)
  let orderIds = nub $ mapMaybe (.orderId) (Map.findWithDefault [] utr utrView.claimsByUtr)
  orderView <- loadOrderView merchantId orderIds
  pure $ Res.UtrDetailRes {utr = summary, orders = mapMaybe (toOrderRow orderView) orderIds}

-- Phase 3 (step 10): BANK_RECEIPT only; the UTR is locked from here.
postRSFReconciliationRsfUtrBankVerify :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Res.BankVerifyReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfUtrBankVerify merchantShortId _opCity utr req = do
  merchantId <- findMerchantId merchantShortId
  actorId <- operatorId
  RSFLedger.verifyUtr merchantId utr req.bankVerifiedAmount actorId req.reason
  logInfo $ "RSF bank verify: utr=" <> utr <> " amount=" <> show req.bankVerifiedAmount <> " by=" <> actorId
  pure APISuccess.Success

-- Phase 3 (step 11b): signed correction to a verified UTR's bank amount.
postRSFReconciliationRsfUtrAdjust :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Res.UtrAdjustReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfUtrAdjust merchantShortId _opCity utr req = do
  merchantId <- findMerchantId merchantShortId
  actorId <- operatorId
  RSFLedger.adjustUtr merchantId utr req.amount actorId req.reason
  logInfo $ "RSF utr adjust: utr=" <> utr <> " amount=" <> show req.amount <> " by=" <> actorId
  pure APISuccess.Success

-- Phase 3 (step 11): pooled allocation over the day's orders.
postRSFReconciliationRsfAutoAllocation :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.Flow Res.AutoAllocationRes
postRSFReconciliationRsfAutoAllocation merchantShortId _opCity mbDate = do
  merchantId <- findMerchantId merchantShortId
  day <- fromMaybeM (InvalidRequest "Missing 'date'") mbDate
  outcome <- RSFLedger.autoAllocate merchantId day
  logInfo $ "RSF auto-allocation: date=" <> show day <> " orders=" <> show outcome.ordersConsidered <> " settled=" <> show outcome.ordersSettled
  pure
    Res.AutoAllocationRes
      { ordersConsidered = outcome.ordersConsidered,
        ordersSettled = outcome.ordersSettled,
        ordersPartiallyFunded = outcome.ordersPartiallyFunded,
        allocations = [Res.AllocationLine {orderId = o, utr = u, amount = a} | (o, u, a) <- outcome.allocations]
      }

-- Phase 3 (step 11a): manual allocation of one order on one UTR.
postRSFReconciliationRsfOrdersConfirm :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Res.ManualConfirmReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfOrdersConfirm merchantShortId _opCity orderId req = do
  merchantId <- findMerchantId merchantShortId
  actorId <- operatorId
  RSFLedger.reallocateOrder merchantId orderId req.utr req.confirmedAmount actorId req.reason
  logInfo $ "RSF manual allocation: orderId=" <> orderId <> " utr=" <> req.utr <> " by=" <> actorId <> " amount=" <> show req.confirmedAmount
  pure APISuccess.Success

-- Phase 4: skip messages already answered; all gates on every remaining message first, then send.
postRSFReconciliationRsfSend :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.Flow Res.SendForDateRes
postRSFReconciliationRsfSend merchantShortId _opCity mbDate = do
  merchantId <- findMerchantId merchantShortId
  day <- fromMaybeM (InvalidRequest "Missing 'date'") mbDate
  (sent, alreadySent) <- CallRSF.sendOnReceiverReconForDate (Id merchantId) day
  pure $ Res.SendForDateRes {messagesSent = sent, messagesAlreadySent = alreadySent}

getRSFReconciliationRsfReconUnmatched :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.ReconGridListRes
getRSFReconciliationRsfReconUnmatched merchantShortId _opCity mbFrom mbLimit mbOffset mbTo = do
  merchantId <- findMerchantId merchantShortId
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
  -- anti-join and paging in the database: completed, BAP-collected bookings with no BAP_CLAIM
  bookings <- QAgg.findUnclaimedCompletedBookingsPage merchantId from to limit offset
  -- one ride read for the page: the final fare lives on the ride, the booking only has the estimate
  rideByBookingId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map (.id) bookings))
  pure $ Res.ReconGridListRes {totalItems = offset + length bookings, rows = map (toUnmatchedGridRow rideByBookingId) bookings}

toUtrSummary :: UtrView -> Text -> Maybe Res.UtrSummary
toUtrSummary view utr =
  let entries = Map.findWithDefault [] utr view.entriesByUtr
      claims = Map.findWithDefault [] utr view.claimsByUtr
      balance = RSFLedger.utrBalance entries
   in case claims of
        [] -> Nothing
        (firstClaim : _) ->
          Just
            Res.UtrSummary
              { utr = utr,
                bapId = fromMaybe "" firstClaim.collectorAppId,
                claimedTotalAmount = sum (map (.amount) claims),
                bankVerifiedAmount = if balance.verified then Just balance.received else Nothing,
                allocatedAmount = if balance.verified then balance.allocated else 0,
                unallocatedAmount = if balance.verified then balance.received - balance.allocated else 0,
                totalOrders = length claims,
                reportedStatus = Map.lookup utr view.utrStates >>= (.reportedStatus),
                createdAt = firstClaim.createdAt
              }

-- One order, live-folded from the pre-grouped view: its standing claim legs, what allocation
-- gave it, and the verdict. AWAITING until a Phase 2 verdict exists and an allocation run has
-- touched the order.
toOrderRow :: OrderView -> Text -> Maybe Res.OrderRow
toOrderRow view orderId =
  case Map.findWithDefault [] orderId view.claimsByOrder of
    [] -> Nothing
    claims@(firstClaim : _) ->
      let lastClaim = last claims
          mbRide = Map.lookup orderId view.rideByOrderId
          fare = mbRide >>= RSFLedger.rideFare
          allocationRows = Map.findWithDefault [] orderId view.allocationsByOrder
          allocated = sum (map (.amount) allocationRows)
          -- the order's status is the status of its latest BAP_CLAIM
          latestStatus = lastClaim.claimStatus
          rejected = mfilter (`notElem` [L.PENDING, L.ACCEPTED]) latestStatus
          (verdict, diff) = case (rejected, fare) of
            -- signed, ours − theirs: > 0 underpaid, < 0 overpaid (same convention as the wire)
            (Just _, _) -> (Res.REJECTED, fromMaybe 0 lastClaim.rejectionDiff)
            (_, Just fareAmount)
              | latestStatus /= Just L.PENDING && not (null allocationRows) ->
                let d = fareAmount - allocated
                 in (if d == 0 then Res.PAID else if d > 0 then Res.UNDERPAID else Res.OVERPAID, d)
            _ -> (Res.AWAITING, 0)
          perUtr = Map.toList $ Map.fromListWith (+) [(u, e.amount) | e <- allocationRows, Just u <- [e.utr]]
       in Just
            Res.OrderRow
              { orderId = orderId,
                rideId = (.id.getId) <$> mbRide,
                driverId = (.driverId.getId) <$> mbRide,
                bapId = fromMaybe "" (listToMaybe (mapMaybe (.collectorAppId) claims)),
                platformGrossFare = fare,
                -- what the collector says the order is worth (payment.params.amount), from its latest claim
                claimedTotalAmount = fromMaybe 0 lastClaim.orderPaymentAmount,
                receivedTotal = allocated,
                orderVerdict = verdict,
                orderDiff = diff,
                claimStatus = latestStatus,
                settlementUtrs = nub (mapMaybe (.utr) claims),
                allocations = [Res.OrderAllocation {utr = u, amount = a} | (u, a) <- perUtr],
                anyManuallyConfirmed = any ((== L.ADMIN) . (.actorType)) allocationRows,
                -- sent = the collector's latest claim for this order is the one we last answered
                allSent = isJust (lastClaim.messageId) && Map.lookup orderId view.lastReportedMessageByOrder == lastClaim.messageId,
                receivedAt = firstClaim.createdAt
              }

-- A completed booking no receiver_recon ever claimed: the whole final ride fare is outstanding
-- (rideFare, as for claimed orders); the booking's estimate only when the ride row is missing.
toUnmatchedGridRow :: Map.Map Text DRide.Ride -> DBooking.Booking -> Res.ReconGridRow
toUnmatchedGridRow rideByBookingId booking =
  let mbRide = Map.lookup booking.id.getId rideByBookingId
      fare = fromMaybe booking.estimatedFare (mbRide >>= RSFLedger.rideFare)
   in Res.ReconGridRow
        { rideId = (.id.getId) <$> mbRide,
          orderId = booking.id.getId,
          buyerAppName = booking.bapId,
          rideDateTime = Just (fromMaybe booking.startTime (mbRide >>= (.tripStartTime))),
          driverId = (.driverId.getId) <$> mbRide,
          grossFarePlatform = Just fare,
          netReceivablePlatform = Just fare,
          bapSettlementAmount = 0,
          amountDifference = fare,
          settlementDateBap = Nothing,
          settlementUtrs = [],
          reconciliationStatus = Res.Unmatched,
          payoutEligible = False,
          anyManuallyConfirmed = False,
          communicationStatus = "NOT_SENT"
        }

-- We can maintain a api which show the history of the order and utr
