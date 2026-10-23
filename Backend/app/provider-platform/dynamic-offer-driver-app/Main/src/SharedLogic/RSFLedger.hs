{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.RSFLedger
  ( Entry,
    WireVerdict (..),
    baseEntry,
    ofType,
    acceptedClaims,
    latestClaims,
    claimedOnUtr,
    receivedOnUtr,
    isUtrVerified,
    allocatedToOrder,
    allocatedOnPair,
    unallocatedOnUtr,
    UtrBalance (..),
    utrBalance,
    latestRideByBooking,
    rideFare,
    parseInbound,
    amountVerdict,
    rejectionVerdict,
    orderWireVerdict,
    utrWireVerdict,
    AutoAllocationOutcome (..),
    istDayRange,
    verifyUtr,
    adjustUtr,
    autoAllocate,
    reallocateOrder,
  )
where

import qualified BecknV2.RSF.Types as Spec
import BecknV2.RSF.Utils (roundToPaise)
import qualified Data.Aeson as A
import qualified Data.HashSet as HS
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime (..))
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Storage.Queries.RsfLedgerAggregates as QAgg

type Entry = L.RsfReconLedgerEntry

-- What goes on the wire (and into the VARIANCE rows / state tables) for one order or UTR.
data WireVerdict = WireVerdict
  { status :: Text,
    diff :: HighPrecMoney,
    code :: Maybe Text,
    name :: Maybe Text
  }

baseEntry :: (MonadFlow m) => Text -> Maybe Text -> L.RsfLedgerEntryType -> L.RsfLedgerSource -> HighPrecMoney -> UTCTime -> m Entry
baseEntry merchantId merchantOperatingCityId entryType source amount effectiveAt = do
  entryId <- generateGUID
  now <- getCurrentTime
  pure
    L.RsfReconLedgerEntry
      { id = entryId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        messageId = Nothing,
        orderId = Nothing,
        utr = Nothing,
        entryType = entryType,
        amount = amount,
        currency = "INR",
        contextTransactionId = Nothing,
        bapId = Nothing,
        bapUri = Nothing,
        ttl = Nothing,
        rawJson = Nothing,
        orderPaymentAmount = Nothing,
        bffType = Nothing,
        bffAmount = Nothing,
        withholdingTaxGst = Nothing,
        withholdingTaxTds = Nothing,
        deductionByCollector = Nothing,
        settlementId = Nothing,
        orderTransactionId = Nothing,
        invoiceNo = Nothing,
        collectorAppId = Nothing,
        orderState = Nothing,
        settlementReasonCode = Nothing,
        rideId = Nothing,
        driverId = Nothing,
        claimStatus = Nothing,
        rejectionDiff = Nothing,
        source = source,
        actorType = L.SYSTEM,
        actorId = Nothing,
        reason = Nothing,
        counterpartyReconStatus = Nothing,
        diffMessageCode = Nothing,
        diffMessageName = Nothing,
        reportedAt = Nothing,
        reportedInMessageId = Nothing,
        effectiveAt = effectiveAt,
        createdAt = now,
        updatedAt = now
      }

-- ─── Ledger folds (pure) ───────────────────────────────────────────────────

ofType :: L.RsfLedgerEntryType -> [Entry] -> [Entry]
ofType entryType = filter ((== entryType) . (.entryType))

latestClaimsBy :: (Entry -> Bool) -> [Entry] -> Map.Map (Text, Text) Entry
latestClaimsBy keep entries =
  Map.fromListWith
    (\a b -> if a.createdAt >= b.createdAt then a else b)
    [((orderId, utr), e) | e <- ofType L.BAP_CLAIM entries, keep e, Just orderId <- [e.orderId], Just utr <- [e.utr]]

-- Latest ACCEPTED claim per (order, UTR) -- the only claims allocation funds.
acceptedClaims :: [Entry] -> Map.Map (Text, Text) Entry
acceptedClaims = latestClaimsBy ((== Just L.ACCEPTED) . (.claimStatus))

-- Latest claim per (order, UTR) regardless of status -- what the dashboard shows as "the claim".
latestClaims :: [Entry] -> Map.Map (Text, Text) Entry
latestClaims = latestClaimsBy (const True)

-- Σ latest claim per (order, UTR) on the UTR, whatever its status: the collector's current word on it.
claimedOnUtr :: Text -> [Entry] -> HighPrecMoney
claimedOnUtr utr entries = sum [e.amount | ((_, u), e) <- Map.toList (latestClaims entries), u == utr]

receivedOnUtr :: Text -> [Entry] -> HighPrecMoney
receivedOnUtr utr entries = sum [e.amount | e <- entries, e.utr == Just utr, e.entryType `elem` [L.BANK_RECEIPT, L.MANUAL_ADJUSTMENT]]

isUtrVerified :: Text -> [Entry] -> Bool
isUtrVerified utr = any ((== Just utr) . (.utr)) . ofType L.BANK_RECEIPT

allocatedToOrder :: Text -> [Entry] -> HighPrecMoney
allocatedToOrder orderId entries = sum [e.amount | e <- ofType L.BANK_ALLOCATION entries, e.orderId == Just orderId]

allocatedOnPair :: Text -> Text -> [Entry] -> HighPrecMoney
allocatedOnPair orderId utr entries = sum [e.amount | e <- ofType L.BANK_ALLOCATION entries, e.orderId == Just orderId, e.utr == Just utr]

-- received − Σ allocated to orders.
unallocatedOnUtr :: Text -> [Entry] -> HighPrecMoney
unallocatedOnUtr utr entries =
  receivedOnUtr utr entries - sum [e.amount | e <- ofType L.BANK_ALLOCATION entries, e.utr == Just utr, isJust e.orderId]

-- Verified / received / allocated for one UTR in a single pass over that UTR's own ledger rows.
data UtrBalance = UtrBalance
  { verified :: Bool,
    received :: HighPrecMoney,
    allocated :: HighPrecMoney
  }

utrBalance :: [Entry] -> UtrBalance
utrBalance = foldl' step (UtrBalance False 0 0)
  where
    step b e = case e.entryType of
      L.BANK_RECEIPT -> UtrBalance True (b.received + e.amount) b.allocated
      L.MANUAL_ADJUSTMENT -> UtrBalance b.verified (b.received + e.amount) b.allocated
      L.BANK_ALLOCATION | isJust e.orderId -> UtrBalance b.verified b.received (b.allocated + e.amount)
      _ -> b

latestRideByBooking :: [DRide.Ride] -> Map.Map Text DRide.Ride
latestRideByBooking rides =
  Map.fromListWith (\a b -> if a.createdAt >= b.createdAt then a else b) [(ride.bookingId.getId, ride) | ride <- rides]

-- Nothing = ride still in flight, so there is no fare to judge a claim against yet.
-- Rounded to paise: the stored fare carries more decimals than money on the wire ever does.
rideFare :: DRide.Ride -> Maybe HighPrecMoney
rideFare ride
  | Just fare <- ride.fare = Just (roundToPaise fare)
  | ride.status == DRide.CANCELLED = Just (roundToPaise (fromMaybe 0 ride.cancellationChargesOnCancel))
  | otherwise = Nothing

-- The stored receiver_recon payload of a MESSAGE_RECEIVED row: its context, order ids and UTRs.
-- Read from raw_json because a re-sent, unchanged order writes no BAP_CLAIM row of its own.
parseInbound :: Entry -> Maybe (Spec.RSFContext, [Text], [Text])
parseInbound envelope = do
  inbound :: Spec.ReceiverReconReq <- envelope.rawJson >>= A.decodeStrict . TE.encodeUtf8
  let wireOrders = inbound.receiverReconReqMessage.rsfOrderbookMessageOrderbook.rsfOrderbookOrders
      legs order = fromMaybe [] (order.rsfOrderPayment >>= (.rsfPaymentSettlementDetails))
  pure
    ( inbound.receiverReconReqContext,
      nub $ mapMaybe (.rsfOrderId) wireOrders,
      nub $ concatMap (mapMaybe (.rsfSettlementDetailReference) . legs) wireOrders
    )

-- ─── Verdicts (pure) ───────────────────────────────────────────────────────

-- expected − got: 0 → 01 Paid, > 0 → 03 Underpaid, < 0 → 02 Overpaid.
amountVerdict :: HighPrecMoney -> WireVerdict
amountVerdict d
  | d == 0 = WireVerdict "01" 0 Nothing Nothing
  | d > 0 = WireVerdict "03" d (Just "less") (Just "lesser amount")
  | otherwise = WireVerdict "02" (abs d) (Just "more") (Just "excess amount")

rejectionVerdict :: Entry -> Maybe WireVerdict
rejectionVerdict claim = case claim.claimStatus of
  Just L.REJECTED_NOT_PAID -> Just $ WireVerdict "04" 0 (Just "70014") (Just "payment status is not PAID")
  Just L.REJECTED_DETAIL_SUM -> Just $ withDiff "70014" "settlement details do not sum to order amount"
  Just L.REJECTED_FARE_MISMATCH -> Just $ withDiff "70014" "order amount does not match ride fare"
  Just L.REJECTED_BFF_MISMATCH -> Just $ withDiff "70014" "buyer app finder fee does not match"
  Just L.REJECTED_UTR_IMBALANCE -> Just $ WireVerdict "04" 0 (Just "70011") (Just "settlement reference already reconciled")
  _ -> Nothing
  where
    withDiff errCode errName =
      let d = fromMaybe 0 claim.rejectionDiff
       in WireVerdict (if d < 0 then "02" else "03") (abs d) (Just errCode) (Just errName)

-- Phase 4: an order rejected in this message reports its rejection; otherwise fare − Σ allocated.
orderWireVerdict :: Text -> HighPrecMoney -> Text -> [Entry] -> WireVerdict
orderWireVerdict messageId fare orderId entries =
  let msgClaims = [e | e <- ofType L.BAP_CLAIM entries, e.orderId == Just orderId, e.messageId == Just messageId]
   in fromMaybe (amountVerdict (fare - allocatedToOrder orderId entries)) (listToMaybe (mapMaybe rejectionVerdict msgClaims))

-- Phase 4: Σ claimed − received for the UTR.
utrWireVerdict :: Text -> [Entry] -> WireVerdict
utrWireVerdict utr entries = amountVerdict (claimedOnUtr utr entries - receivedOnUtr utr entries)

-- ─── Phase 3: bank receipt + allocation ────────────────────────────────────

-- One lock per merchant around every read-compute-write on the money rows (verify, adjust,
-- reallocate, autoAllocate), so the balances a writer read are the balances it writes against.
withFinanceLock :: (BeamFlow m r, Hedis.HedisFlow m r) => Text -> m a -> m a
withFinanceLock merchantId = Hedis.withLockRedisAndReturnValue ("RsfFinanceLock:" <> merchantId) 300

-- Dates on the dashboard are IST calendar days; the ledger stores UTC.
istDayRange :: Day -> (UTCTime, UTCTime)
istDayRange day = let start = addUTCTime (-19800) (UTCTime day 0) in (start, addUTCTime 86400 start)

-- Phase 3 (step 10): finance enters what the bank credited for a UTR -- written once, then frozen.
-- Verifies and locks only; the money is handed out by autoAllocate.
verifyUtr ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  HighPrecMoney ->
  Text ->
  Maybe Text ->
  m ()
verifyUtr merchantId utr bankVerifiedAmount actorId reason = withFinanceLock merchantId $ do
  when (bankVerifiedAmount < 0) $ throwError $ InvalidRequest "Bank-verified amount must not be negative"
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  when (null (ofType L.BAP_CLAIM utrEntries)) $ throwError $ InvalidRequest "UTR not found"
  when (isUtrVerified utr utrEntries) $ throwError $ InvalidRequest "UTR already bank-verified; the receipt is frozen"
  now <- getCurrentTime
  receipt <- baseEntry merchantId (listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)) L.BANK_RECEIPT L.BANK_CONFIRMED bankVerifiedAmount now
  QLedger.create receipt {L.utr = Just utr, L.actorType = L.FINANCE, L.actorId = Just actorId, L.reason = reason}

-- Phase 3 (step 11b): the money for a verified UTR changed after it was entered. One signed
-- MANUAL_ADJUSTMENT row (the BANK_RECEIPT is never rewritten); the UTR's received amount is the
-- receipt plus all adjustments. A decrease may not take received below what is already allocated.
adjustUtr ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  HighPrecMoney ->
  Text ->
  Text ->
  m ()
adjustUtr merchantId utr delta actorId reason = withFinanceLock merchantId $ do
  when (delta == 0) $ throwError $ InvalidRequest "Adjustment amount must be non-zero"
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  let balance = utrBalance utrEntries
  unless balance.verified $ throwError $ InvalidRequest "UTR is not bank-verified yet"
  when (balance.received + delta < balance.allocated) $
    throwError $ InvalidRequest "Adjustment would take the UTR below what is already allocated to orders"
  now <- getCurrentTime
  row <- baseEntry merchantId (listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)) L.MANUAL_ADJUSTMENT L.FINANCE_MANUAL delta now
  QLedger.create row {L.utr = Just utr, L.actorType = L.ADMIN, L.actorId = Just actorId, L.reason = Just reason}

data AutoAllocationOutcome = AutoAllocationOutcome
  { ordersConsidered :: Int,
    ordersSettled :: Int,
    ordersPartiallyFunded :: Int,
    allocations :: [(Text, Text, HighPrecMoney)]
  }

-- Money the collector claims for an order vs what allocation has already given it.
data OrderData = OrderData
  { orderId :: Text,
    claimed :: HighPrecMoney,
    allocated :: HighPrecMoney
  }

-- Money the bank credited on a UTR, what the collector claims on it, and what allocation has
-- already handed out from it.
data UtrData = UtrData
  { received :: HighPrecMoney,
    claimed :: HighPrecMoney,
    allocated :: HighPrecMoney
  }

-- The day's ACCEPTED claims minus the orders already reported to the collector as 01 Paid.
-- An order reported short (02/03/04) stays open: the collector settles it in a later
-- receiver_recon, so it is allocated again from whatever that brings.
unprocessedOrderIds :: (BeamFlow m r) => Text -> [Entry] -> m [Text]
unprocessedOrderIds merchantId dayClaims = do
  let orderIds = nub (mapMaybe (.orderId) dayClaims)
  states <- QOrderState.findAllByMerchantAndOrderIds merchantId orderIds
  let paid = HS.fromList [s.orderId | s <- states, s.reportedStatus == Just "01"]
  pure $ filter (not . (`HS.member` paid)) orderIds

-- orderId -> OrderData: claimed from the day's claims (order_payment_amount), allocated from one GROUP BY.
giveOrder :: (BeamFlow m r) => Text -> [Entry] -> [Text] -> m (Map.Map Text OrderData)
giveOrder merchantId dayClaims orderIds = do
  allocatedByOrder <- QAgg.sumAllocatedByOrder merchantId orderIds
  let claimedByOrder = Map.fromListWith max [(o, fromMaybe 0 c.orderPaymentAmount) | c <- dayClaims, Just o <- [c.orderId]]
  pure $
    Map.fromList
      [ (o, OrderData {orderId = o, claimed = Map.findWithDefault 0 o claimedByOrder, allocated = Map.findWithDefault 0 o allocatedByOrder})
        | o <- orderIds
      ]

-- utr -> UtrData: received and allocated from one GROUP BY (utr, entry_type), claimed from the day's
-- latest accepted claims. A UTR with no BANK_RECEIPT is unverified.
giveUtr :: (BeamFlow m r) => Text -> [Entry] -> [Text] -> m (Map.Map Text UtrData)
giveUtr merchantId dayClaims utrs = do
  sums <- QAgg.sumByUtrAndEntryType merchantId [L.BANK_RECEIPT, L.MANUAL_ADJUSTMENT, L.BANK_ALLOCATION] utrs
  let amount u t = Map.lookup (u, t) sums
      claimedByUtr = Map.fromListWith (+) [(u, c.amount) | ((_, u), c) <- Map.toList (acceptedClaims dayClaims)]
      unverified = [u | u <- utrs, isNothing (amount u L.BANK_RECEIPT)]
  unless (null unverified) $ throwError $ InvalidRequest ("UTRs not bank-verified yet: " <> show unverified)
  pure $
    Map.fromList
      [ (u, UtrData {received = fromMaybe 0 (amount u L.BANK_RECEIPT) + fromMaybe 0 (amount u L.MANUAL_ADJUSTMENT), claimed = Map.findWithDefault 0 u claimedByUtr, allocated = fromMaybe 0 (amount u L.BANK_ALLOCATION)})
        | u <- utrs
      ]

-- utr -> number of (unprocessed) orders linked to it.
giveDegreeUtr :: [Entry] -> Map.Map Text Int
giveDegreeUtr dayClaims = Map.map HS.size $ Map.fromListWith HS.union [(u, HS.singleton o) | c <- dayClaims, Just u <- [c.utr], Just o <- [c.orderId]]

-- Orders in increasing order of what they are still owed, each with its UTRs ordered settled
-- first (bank received = claimed, so it can be used up in full), then least shared, then smallest.
getLinker :: (BeamFlow m r) => Text -> [Entry] -> [Text] -> Map.Map Text UtrData -> m [(OrderData, Set.Set ((Bool, Int, HighPrecMoney), Text))]
getLinker merchantId dayClaims orderIds utrMap = do
  orderMap <- giveOrder merchantId dayClaims orderIds
  let degUtr = giveDegreeUtr dayClaims
      utrKey u = let utr = Map.findWithDefault (UtrData 0 0 0) u utrMap in (utr.received /= utr.claimed, Map.findWithDefault 0 u degUtr, utr.received)
      utrsByOrder = Map.fromListWith Set.union [(o, Set.singleton (utrKey u, u)) | c <- dayClaims, Just o <- [c.orderId], Just u <- [c.utr]]
  pure $
    sortOn (\(o, _) -> o.claimed - o.allocated) [(o, Map.findWithDefault Set.empty o.orderId utrsByOrder) | o <- Map.elems orderMap]

-- Phase 3 (step 11): pooled allocation over one IST day's accepted claims. A UTR's money is
-- fungible across the orders linked to it: orders are filled whole, smallest outstanding first,
-- draining each order's UTRs settled first, then least shared, then smallest. Every UTR of the
-- day must be bank-verified.
-- Reported orders are frozen; re-runs only top up. An order that gets nothing is given a
-- 0-amount row so Phase 4 can tell "allocated nothing" from "never allocated".
autoAllocate ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Day ->
  m AutoAllocationOutcome
autoAllocate merchantId day = withFinanceLock merchantId $ do
  let (from, to) = istDayRange day
  dayClaims <- QLedger.findClaimsByMerchantStatusAndCreatedAtRange merchantId L.ACCEPTED from to
  when (null dayClaims) $ throwError $ InvalidRequest "No accepted receiver_recon claims on this date"
  utrMap0 <- giveUtr merchantId dayClaims (nub (mapMaybe (.utr) dayClaims))
  orderIds <- unprocessedOrderIds merchantId dayClaims
  let processable = filter (\c -> maybe False (`elem` orderIds) c.orderId) dayClaims
  linker <- getLinker merchantId processable orderIds utrMap0
  let fillOrder (utrMap, acc) (order, utrs) =
        let owed0 = order.claimed - order.allocated
            step (pool, owed, taken) (_, u) =
              let utr = Map.findWithDefault (UtrData 0 0 0) u pool
                  give = min owed (utr.received - utr.allocated)
               in if owed <= 0 || give <= 0
                    then (pool, owed, taken)
                    else (Map.insert u (UtrData utr.received utr.claimed (utr.allocated + give)) pool, owed - give, taken <> [(order.orderId, u, give)])
            (utrMap', owed', taken') = foldl' step (utrMap, owed0, []) (Set.toAscList utrs)
            taken'' = if null taken' then [(order.orderId, maybe "" snd (Set.lookupMin utrs), 0)] else taken'
         in (utrMap', acc <> [(owed0, owed', taken'')])
      (_, results) = foldl' fillOrder (utrMap0, []) [l | l@(o, _) <- linker, o.claimed - o.allocated > 0]
      allLines = concatMap (\(_, _, ls) -> ls) results
  now <- getCurrentTime
  let merchantOpCityId = listToMaybe (mapMaybe (.merchantOperatingCityId) dayClaims)
  rows <- forM allLines $ \(orderId, utr, amount) -> do
    row <- baseEntry merchantId merchantOpCityId L.BANK_ALLOCATION L.SYSTEM_JOB amount now
    pure row {L.orderId = Just orderId, L.utr = Just utr}
  QLedger.createMany rows
  pure
    AutoAllocationOutcome
      { ordersConsidered = length results,
        ordersSettled = length [() | (_, owed, _) <- results, owed <= 0],
        ordersPartiallyFunded = length [() | (owed0, owed, _) <- results, owed > 0, owed < owed0],
        allocations = allLines
      }

-- Phase 3 (step 11a): ops sets an order's allocation on a UTR by hand. Appends a delta
-- allocation; never rewrites earlier rows.
reallocateOrder ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  Text ->
  HighPrecMoney ->
  Text ->
  Text ->
  m ()
reallocateOrder merchantId orderId utr newAmount actorId reason = withFinanceLock merchantId $ do
  when (newAmount < 0) $ throwError $ InvalidRequest "Allocation amount must not be negative"
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  unless (isUtrVerified utr utrEntries) $ throwError $ InvalidRequest "UTR is not bank-verified yet"
  unless (Map.member (orderId, utr) (acceptedClaims utrEntries)) $ throwError $ InvalidRequest "No accepted claim for this order on this UTR"
  let delta = newAmount - allocatedOnPair orderId utr utrEntries
  when (delta > unallocatedOnUtr utr utrEntries) $ throwError $ InvalidRequest "Allocation exceeds the bank-verified amount left on this UTR"
  when (delta /= 0) $ do
    now <- getCurrentTime
    allocation <- baseEntry merchantId (listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)) L.BANK_ALLOCATION L.FINANCE_MANUAL delta now
    QLedger.create allocation {L.orderId = Just orderId, L.utr = Just utr, L.actorType = L.ADMIN, L.actorId = Just actorId, L.reason = Just reason}
