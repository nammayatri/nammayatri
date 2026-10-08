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
    AutoAllocationOutcome (..),
    istDayRange,
    verifyUtr,
    autoAllocate,
    reallocateOrder,
  )
where

import qualified BecknV2.RSF.Types as Spec
import qualified Data.Aeson as A
import qualified Data.HashSet as HS
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime (..))
import qualified Domain.Types.Ride as DRide
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
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

-- received − Σ allocated to orders.
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

data AutoAllocationOutcome = AutoAllocationOutcome
  { ordersConsidered :: Int,
    ordersSettled :: Int,
    ordersPartiallyFunded :: Int,
    allocations :: [(Text, Text, HighPrecMoney)]
  }

-- Phase 3 (step 11): pooled allocation over one IST day's claims. A UTR's money is fungible
-- across the orders it is linked to, so orders are filled whole rather than leg by leg:
--   orders sorted by fare asc (then fewest linked UTRs, then claim order);
--   each order drains its linked UTRs sorted by degree asc (then lowest available);
--   degree(U) = number of the day's orders linked to U.
-- Every UTR the day's claims mention must be bank-verified. Orders already reported to the
-- collector are frozen; everything else is only ever topped up, never taken back. An order
-- considered but given nothing gets a 0-amount row so Phase 4 can tell "allocated nothing"
-- from "never allocated".
autoAllocate ::
  (BeamFlow m r, Hedis.HedisFlow m r) =>
  Text ->
  Day ->
  m AutoAllocationOutcome
autoAllocate merchantId day = do
  let (from, to) = istDayRange day
  dayClaims <- ofType L.BAP_CLAIM <$> QLedger.findAllByMerchantEntryTypeAndCreatedAtRange merchantId L.BAP_CLAIM from to
  let dayUtrs = nub (mapMaybe (.utr) dayClaims)
      orderIds = nub (mapMaybe (.orderId) dayClaims)
  when (null dayClaims) $ throwError $ InvalidRequest "No receiver_recon claims on this date"
  orderEntries <- QLedger.findAllByMerchantAndOrderIds merchantId orderIds
  let accepted = acceptedClaims orderEntries
      linkedUtrs orderId = nub [u | ((o, u), _) <- Map.toList accepted, o == orderId]
      allUtrs = nub (dayUtrs <> concatMap linkedUtrs orderIds)
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId allUtrs
  let unverified = filter (\u -> not (isUtrVerified u utrEntries)) dayUtrs
  unless (null unverified) $ throwError $ InvalidRequest ("UTRs not bank-verified yet: " <> show unverified)
  reportedOrders <- QOrderState.findAllByMerchantAndOrderIds merchantId orderIds
  rideByOrderId <- latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
  let frozen = HS.fromList [s.orderId | s <- reportedOrders, isJust s.reportedStatus]
      fareOf orderId = fromMaybe 0 (Map.lookup orderId rideByOrderId >>= rideFare)
      firstClaimAt orderId = minimum [c.createdAt | ((o, _), c) <- Map.toList accepted, o == orderId]
      eligible =
        [ (orderId, utrs)
          | orderId <- orderIds,
            let utrs = linkedUtrs orderId,
            not (null utrs),
            not (HS.member orderId frozen),
            fareOf orderId - allocatedToOrder orderId orderEntries > 0
        ]
      degree u = length [() | (_, utrs) <- eligible, u `elem` utrs]
      ordered = sortOn (\(o, utrs) -> (fareOf o, length utrs, firstClaimAt o)) eligible
      pool0 = Map.fromList [(u, unallocatedOnUtr u utrEntries) | u <- allUtrs]
      fillOrder (pool, acc) (orderId, utrs) =
        let outstanding0 = fareOf orderId - allocatedToOrder orderId orderEntries
            byPreference = sortOn (\u -> (degree u, Map.findWithDefault 0 u pool, u)) utrs
            step (pool', outstanding, taken) u =
              let available = max 0 (Map.findWithDefault 0 u pool')
                  take' = min outstanding available
               in if outstanding <= 0 || take' <= 0
                    then (pool', outstanding, taken)
                    else (Map.insert u (available - take') pool', outstanding - take', taken <> [(orderId, u, take')])
            (pool1, shortBy, taken') = foldl' step (pool, outstanding0, []) byPreference
            taken'' = if null taken' then [(orderId, fromMaybe "" (listToMaybe byPreference), 0)] else taken'
         in (pool1, acc <> [(outstanding0, shortBy, taken'')])
      (_, results) = foldl' fillOrder (pool0, []) ordered
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
        ordersSettled = length [() | (_, shortBy, _) <- results, shortBy <= 0],
        ordersPartiallyFunded = length [() | (outstanding, shortBy, _) <- results, shortBy > 0, shortBy < outstanding],
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
reallocateOrder merchantId orderId utr newAmount actorId reason = withUtrLock merchantId utr $ do
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  unless (isUtrVerified utr utrEntries) $ throwError $ InvalidRequest "UTR is not bank-verified yet"
  unless (Map.member (orderId, utr) (acceptedClaims utrEntries)) $ throwError $ InvalidRequest "No accepted claim for this order on this UTR"
  let delta = newAmount - allocatedOnPair orderId utr utrEntries
  when (delta > unallocatedOnUtr utr utrEntries) $ throwError $ InvalidRequest "Allocation exceeds the bank-verified amount left on this UTR"
  when (delta /= 0) $ do
    now <- getCurrentTime
    allocation <- baseEntry merchantId (listToMaybe (mapMaybe (.merchantOperatingCityId) utrEntries)) L.BANK_ALLOCATION L.FINANCE_MANUAL delta now
    QLedger.create allocation {L.orderId = Just orderId, L.utr = Just utr, L.actorType = L.ADMIN, L.actorId = Just actorId, L.reason = Just reason}
