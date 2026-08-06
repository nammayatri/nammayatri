{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.RSFLedger
  ( Entry,
    WireVerdict (..),
    baseEntry,
    ofType,
    acceptedClaims,
    standingClaims,
    claimedOnUtr,
    receivedOnUtr,
    isUtrVerified,
    allocatedToOrder,
    allocatedOnPair,
    unallocatedOnUtr,
    latestRideByBooking,
    rideFare,
    parseInbound,
    amountVerdict,
    rejectionVerdict,
    orderWireVerdict,
    utrWireVerdict,
    allocateUtr,
    verifyUtr,
    reallocateOrder,
  )
where

import qualified BecknV2.RSF.Types as Spec
import qualified Data.Aeson as A
import Data.List (nub, partition, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Ride as DRide
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Storage.Queries.Ride as QRide

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

-- Latest claim per (order, UTR) the collector still stands on (a refused revision does not replace it).
standingClaims :: [Entry] -> Map.Map (Text, Text) Entry
standingClaims = latestClaimsBy ((/= Just L.REJECTED_UTR_IMBALANCE) . (.claimStatus))

claimedOnUtr :: Text -> [Entry] -> HighPrecMoney
claimedOnUtr utr entries = sum [e.amount | ((_, u), e) <- Map.toList (standingClaims entries), u == utr]

receivedOnUtr :: Text -> [Entry] -> HighPrecMoney
receivedOnUtr utr entries = sum [e.amount | e <- entries, e.utr == Just utr, e.entryType `elem` [L.BANK_RECEIPT, L.MANUAL_ADJUSTMENT]]

isUtrVerified :: Text -> [Entry] -> Bool
isUtrVerified utr = any ((== Just utr) . (.utr)) . ofType L.BANK_RECEIPT

allocatedToOrder :: Text -> [Entry] -> HighPrecMoney
allocatedToOrder orderId entries = sum [e.amount | e <- ofType L.BANK_ALLOCATION entries, e.orderId == Just orderId]

allocatedOnPair :: Text -> Text -> [Entry] -> HighPrecMoney
allocatedOnPair orderId utr entries = sum [e.amount | e <- ofType L.BANK_ALLOCATION entries, e.orderId == Just orderId, e.utr == Just utr]

-- received − Σ allocated to real orders; the suspense rows (orderId NULL) are the record of this figure.
unallocatedOnUtr :: Text -> [Entry] -> HighPrecMoney
unallocatedOnUtr utr entries =
  receivedOnUtr utr entries - sum [e.amount | e <- ofType L.BANK_ALLOCATION entries, e.utr == Just utr, isJust e.orderId]

latestRideByBooking :: [DRide.Ride] -> Map.Map Text DRide.Ride
latestRideByBooking rides =
  Map.fromListWith (\a b -> if a.createdAt >= b.createdAt then a else b) [(ride.bookingId.getId, ride) | ride <- rides]

-- Nothing = ride still in flight, so there is no fare to judge a claim against yet.
rideFare :: DRide.Ride -> Maybe HighPrecMoney
rideFare ride
  | Just fare <- ride.fare = Just fare
  | ride.status == DRide.CANCELLED = Just (fromMaybe 0 ride.cancellationChargesOnCancel)
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

withUtrLock :: (BeamFlow m r, Hedis.HedisFlow m r) => Text -> Text -> m () -> m ()
withUtrLock merchantId utr = Hedis.withLockRedis ("RsfUtrLock:" <> merchantId <> ":" <> utr) 60

-- Phase 3 (step 11) and Phase 2 (stage C top-up): funds the UTR's ACCEPTED claims from whatever
-- is unallocated, min(outstanding claim, available). Largest order shortfall goes first, ties in
-- claim order. Rejected claims get nothing; a UTR with no BANK_RECEIPT is left alone.
allocateUtr ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  m ()
allocateUtr merchantId utr = withUtrLock merchantId utr $ allocateUtrUnlocked False merchantId utr

allocateUtrUnlocked :: (BeamFlow m r) => Bool -> Text -> Text -> m ()
allocateUtrUnlocked isFirstRun merchantId utr = do
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  when (isUtrVerified utr utrEntries) $ do
    let manuallySplit claim = any (\e -> e.orderId == claim.orderId && e.actorType == L.ADMIN) (ofType L.BANK_ALLOCATION utrEntries)
        outstanding claim = claim.amount - allocatedOnPair (fromMaybe "" claim.orderId) utr utrEntries
        unfunded = [c | ((_, u), c) <- Map.toList (acceptedClaims utrEntries), u == utr, outstanding c /= 0, not (manuallySplit c)]
        orderIds = mapMaybe (.orderId) unfunded
    orderEntries <- QLedger.findAllByMerchantAndOrderIds merchantId orderIds
    rideByOrderId <- latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
    let shortfall claim =
          let orderId = fromMaybe "" claim.orderId
           in fromMaybe 0 (rideFare =<< Map.lookup orderId rideByOrderId) - allocatedToOrder orderId orderEntries
        (clawbacks, payments) = partition ((< 0) . outstanding) unfunded
        ordered = clawbacks <> sortOn (\c -> (Down (shortfall c), c.createdAt)) payments
        merchantOpCityId = listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)
        fund (available, acc) claim =
          let funded = if outstanding claim < 0 then outstanding claim else min (outstanding claim) (max 0 available)
           in (available - funded, if funded == 0 then acc else acc <> [(claim, funded)])
        (leftover, fundings) = foldl' fund (unallocatedOnUtr utr utrEntries, []) ordered
    now <- getCurrentTime
    let mkAllocation mbOrderId amount = do
          row <- baseEntry merchantId merchantOpCityId L.BANK_ALLOCATION L.SYSTEM_JOB amount now
          pure row {L.orderId = mbOrderId, L.utr = Just utr}
    allocationRows <- forM fundings $ \(claim, funded) -> mkAllocation claim.orderId funded
    -- suspense (orderId NULL): the whole remainder on the first run, a delta per funding afterwards
    suspenseRows <-
      if isFirstRun
        then sequence [mkAllocation Nothing leftover | leftover /= 0]
        else forM fundings $ \(_, funded) -> mkAllocation Nothing (negate funded)
    QLedger.createMany (allocationRows <> suspenseRows)

-- Phase 3 (step 10): finance enters what the bank credited for a UTR -- written once, then frozen.
verifyUtr ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  HighPrecMoney ->
  Maybe Text ->
  Maybe Text ->
  m ()
verifyUtr merchantId utr bankVerifiedAmount actorId reason = withUtrLock merchantId utr $ do
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  when (null (ofType L.BAP_CLAIM utrEntries)) $ throwError $ InvalidRequest "UTR not found"
  when (isUtrVerified utr utrEntries) $ throwError $ InvalidRequest "UTR already bank-verified; the receipt is frozen"
  now <- getCurrentTime
  receipt <- baseEntry merchantId (listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)) L.BANK_RECEIPT L.BANK_CONFIRMED bankVerifiedAmount now
  QLedger.create receipt {L.utr = Just utr, L.actorType = L.FINANCE, L.actorId = actorId, L.reason = reason}
  allocateUtrUnlocked True merchantId utr

-- Phase 3 (step 11a): ops re-splits an order's allocation on a UTR by hand. Appends a delta
-- allocation and the inverse suspense delta; never rewrites earlier rows.
reallocateOrder ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Text ->
  Text ->
  HighPrecMoney ->
  Text ->
  Text ->
  m ()
reallocateOrder merchantId orderId utr newAmount actorId reason = withUtrLock merchantId utr $ do
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  unless (isUtrVerified utr utrEntries) $ throwError $ InvalidRequest "UTR is not bank-verified yet"
  unless (Map.member (orderId, utr) (acceptedClaims utrEntries)) $ throwError $ InvalidRequest "No accepted claim for this order on this UTR"
  let delta = newAmount - allocatedOnPair orderId utr utrEntries
  when (delta > unallocatedOnUtr utr utrEntries) $ throwError $ InvalidRequest "Allocation exceeds the bank-verified amount left on this UTR"
  when (delta /= 0) $ do
    now <- getCurrentTime
    let merchantOpCityId = listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)
    allocation <- baseEntry merchantId merchantOpCityId L.BANK_ALLOCATION L.FINANCE_MANUAL delta now
    suspense <- baseEntry merchantId merchantOpCityId L.BANK_ALLOCATION L.FINANCE_MANUAL (negate delta) now
    QLedger.createMany
      [ allocation {L.orderId = Just orderId, L.utr = Just utr, L.actorType = L.ADMIN, L.actorId = Just actorId, L.reason = Just reason},
        suspense {L.utr = Just utr, L.actorType = L.ADMIN, L.actorId = Just actorId, L.reason = Just reason}
      ]
