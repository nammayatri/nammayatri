{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Domain.Action.Dashboard.Management.RSFReconciliation
  ( getRSFReconciliationRsfOrders,
    getRSFReconciliationRsfUtrs,
    getRSFReconciliationRsfUtr,
    postRSFReconciliationRsfUtrBankVerify,
    postRSFReconciliationRsfAutoAllocation,
    postRSFReconciliationRsfOrdersConfirm,
    postRSFReconciliationRsfSend,
    getRSFReconciliationRsfReconUnmatched,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation as Res
import Control.Applicative ((<|>))
import qualified Data.HashSet as HS
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Time
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
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import qualified Lib.Finance.Domain.Types.RsfUtrState as DUtrState
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.RsfUtrState as QUtrState
import qualified Sequelize as Se
import qualified SharedLogic.CallRSF as CallRSF
import qualified SharedLogic.RSFLedger as RSFLedger
import qualified Storage.Beam.Ride as BeamR
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideExtra as QRideExtra

-- Everything the dashboard rows are folded from: the ledger rows of a set of orders, the ledger
-- rows of every UTR those orders touch, the rides (fare), and the two "last reported" projections.
data LedgerView = LedgerView
  { orderEntries :: [RSFLedger.Entry],
    utrEntries :: [RSFLedger.Entry],
    rideByOrderId :: Map.Map Text DRide.Ride,
    reportedOrders :: HS.HashSet Text,
    utrStates :: Map.Map Text DUtrState.RsfUtrState
  }

loadView :: Text -> [Text] -> [Text] -> Environment.Flow LedgerView
loadView merchantId orderIds extraUtrs = do
  orderEntries <- QLedger.findAllByMerchantAndOrderIds merchantId orderIds
  let utrs = nub $ extraUtrs <> mapMaybe (.utr) (RSFLedger.ofType L.BAP_CLAIM orderEntries)
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId utrs
  rideByOrderId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
  orderStates <- QOrderState.findAllByMerchantAndOrderIds merchantId orderIds
  utrStates <- QUtrState.findAllByMerchantAndUtrs merchantId utrs
  pure
    LedgerView
      { orderEntries = orderEntries,
        utrEntries = utrEntries,
        rideByOrderId = rideByOrderId,
        reportedOrders = HS.fromList [s.orderId | s <- orderStates, isJust s.reportedStatus],
        utrStates = Map.fromList [(s.utr, s) | s <- utrStates]
      }

findMerchantId :: ShortId M.Merchant -> Environment.Flow Text
findMerchantId merchantShortId = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  pure merchant.id.getId

-- The day's BAP_CLAIM rows (IST calendar day).
dayClaims :: Text -> Maybe Data.Time.Day -> Environment.Flow [RSFLedger.Entry]
dayClaims merchantId mbDay = do
  day <- fromMaybeM (InvalidRequest "Missing 'date'") mbDay
  let (from, to) = RSFLedger.istDayRange day
  QLedger.findAllByMerchantEntryTypeAndCreatedAtRange merchantId L.BAP_CLAIM from to

getRSFReconciliationRsfOrders :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Res.OrderReconVerdict -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow Res.OrderListRes
getRSFReconciliationRsfOrders merchantShortId _opCity mbBapId mbDate mbLimit mbOffset mbStatus mbUtr = do
  merchantId <- findMerchantId merchantShortId
  claims <- dayClaims merchantId mbDate
  let orderIds = nub $ mapMaybe (.orderId) claims
  view <- loadView merchantId orderIds []
  let rows = mapMaybe (toOrderRow view) orderIds
      byBap = maybe rows (\b -> filter ((== b) . (.bapId)) rows) mbBapId
      byUtr = maybe byBap (\u -> filter ((u `elem`) . (.settlementUtrs)) byBap) mbUtr
      byStatus = maybe byUtr (\st -> filter ((== st) . (.orderVerdict)) byUtr) mbStatus
      sorted = sortOn (.receivedAt) byStatus
  pure $ Res.OrderListRes {totalItems = length sorted, orders = paginate mbLimit mbOffset 50 sorted}

getRSFReconciliationRsfUtrs :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Res.UtrListRes
getRSFReconciliationRsfUtrs merchantShortId _opCity mbBapId mbDate mbVerified mbLimit mbOffset = do
  merchantId <- findMerchantId merchantShortId
  claims <- dayClaims merchantId mbDate
  let utrs = nub $ mapMaybe (.utr) claims
  view <- loadView merchantId [] utrs
  let summaries = mapMaybe (toUtrSummary view) utrs
      byBap = maybe summaries (\b -> filter ((== b) . (.bapId)) summaries) mbBapId
      byVerified = maybe byBap (\v -> filter ((== v) . isJust . (.bankVerifiedAmount)) byBap) mbVerified
      sorted = sortOn (Down . (.createdAt)) byVerified
  pure $ Res.UtrListRes {totalItems = length sorted, utrs = paginate mbLimit mbOffset 20 sorted}

getRSFReconciliationRsfUtr :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow Res.UtrDetailRes
getRSFReconciliationRsfUtr merchantShortId _opCity utr = do
  merchantId <- findMerchantId merchantShortId
  utrEntries <- QLedger.findAllByMerchantAndUtrs merchantId [utr]
  let orderIds = nub $ mapMaybe (.orderId) (RSFLedger.ofType L.BAP_CLAIM utrEntries)
  view <- loadView merchantId orderIds [utr]
  summary <- fromMaybeM (InvalidRequest "UTR not found") (toUtrSummary view utr)
  pure $ Res.UtrDetailRes {utr = summary, orders = mapMaybe (toOrderRow view) orderIds}

-- Phase 3 (step 10): BANK_RECEIPT only; the UTR is locked from here.
postRSFReconciliationRsfUtrBankVerify :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Res.BankVerifyReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfUtrBankVerify merchantShortId _opCity utr req = do
  merchantId <- findMerchantId merchantShortId
  RSFLedger.verifyUtr merchantId utr req.bankVerifiedAmount req.verifiedBy req.reason
  logInfo $ "RSF bank verify: utr=" <> utr <> " amount=" <> show req.bankVerifiedAmount
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
  RSFLedger.reallocateOrder merchantId orderId req.utr req.confirmedAmount req.confirmedBy req.reason
  logInfo $ "RSF manual allocation: orderId=" <> orderId <> " utr=" <> req.utr <> " by=" <> req.confirmedBy <> " amount=" <> show req.confirmedAmount
  pure APISuccess.Success

-- Phase 4: all gates on every message of the day first, then send.
postRSFReconciliationRsfSend :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.Flow Res.SendForDateRes
postRSFReconciliationRsfSend merchantShortId _opCity mbDate = do
  merchantId <- findMerchantId merchantShortId
  day <- fromMaybeM (InvalidRequest "Missing 'date'") mbDate
  sent <- CallRSF.sendOnReceiverReconForDate (Id merchantId) day
  pure $ Res.SendForDateRes {messagesSent = sent}

getRSFReconciliationRsfReconUnmatched :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.ReconGridListRes
getRSFReconciliationRsfReconUnmatched merchantShortId _opCity mbFrom mbLimit mbOffset mbTo = do
  merchantId <- findMerchantId merchantShortId
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  completedRides <-
    QRideExtra.findAllRidesWithSeConditionsCreatedAtDesc
      [ Se.And
          [ Se.Is BeamR.merchantId $ Se.Eq (Just merchantId),
            Se.Is BeamR.status $ Se.Eq DRide.COMPLETED,
            Se.Is BeamR.createdAt $ Se.GreaterThanOrEq from,
            Se.Is BeamR.createdAt $ Se.LessThan to
          ]
      ]
  claimedEntries <- QLedger.findAllByMerchantAndOrderIds merchantId (map (\r -> r.bookingId.getId) completedRides)
  let claimedOrderIds = HS.fromList $ mapMaybe (.orderId) (RSFLedger.ofType L.BAP_CLAIM claimedEntries)
      unmatchedRides = filter (\r -> not (HS.member r.bookingId.getId claimedOrderIds)) completedRides
  pure $ Res.ReconGridListRes {totalItems = length unmatchedRides, rows = map toUnmatchedGridRow (paginate mbLimit mbOffset 20 unmatchedRides)}

paginate :: Maybe Int -> Maybe Int -> Int -> [a] -> [a]
paginate mbLimit mbOffset defaultLimit = take (fromMaybe defaultLimit mbLimit) . drop (fromMaybe 0 mbOffset)

toUtrSummary :: LedgerView -> Text -> Maybe Res.UtrSummary
toUtrSummary view utr =
  let claims = [c | ((_, u), c) <- Map.toList (RSFLedger.standingClaims view.utrEntries), u == utr]
      verified = RSFLedger.isUtrVerified utr view.utrEntries
      received = RSFLedger.receivedOnUtr utr view.utrEntries
   in case sortOn (.createdAt) claims of
        [] -> Nothing
        (firstClaim : _) ->
          Just
            Res.UtrSummary
              { utr = utr,
                bapId = fromMaybe "" firstClaim.collectorAppId,
                claimedTotalAmount = RSFLedger.claimedOnUtr utr view.utrEntries,
                bankVerifiedAmount = if verified then Just received else Nothing,
                allocatedAmount = if verified then received - RSFLedger.unallocatedOnUtr utr view.utrEntries else 0,
                unallocatedAmount = if verified then RSFLedger.unallocatedOnUtr utr view.utrEntries else 0,
                totalOrders = length claims,
                reportedStatus = Map.lookup utr view.utrStates >>= (.reportedStatus),
                createdAt = firstClaim.createdAt
              }

-- One order, live-folded: its standing claim legs, what allocation gave it, and the verdict.
-- AWAITING until a Phase 2 verdict exists and an allocation run has touched the order.
toOrderRow :: LedgerView -> Text -> Maybe Res.OrderRow
toOrderRow view orderId =
  case sortOn (.createdAt) [c | ((o, _), c) <- Map.toList (RSFLedger.standingClaims view.orderEntries), o == orderId] of
    [] -> Nothing
    claims@(firstClaim : _) ->
      let fare = Map.lookup orderId view.rideByOrderId >>= RSFLedger.rideFare
          allocationRows = [e | e <- RSFLedger.ofType L.BANK_ALLOCATION view.orderEntries, e.orderId == Just orderId]
          allocated = sum (map (.amount) allocationRows)
          statuses = mapMaybe (.claimStatus) claims
          rejected = find (`notElem` [L.PENDING, L.ACCEPTED]) statuses
          (verdict, diff) = case (rejected, fare) of
            (Just _, _) -> (Res.REJECTED, maybe 0 abs (listToMaybe (mapMaybe (.rejectionDiff) claims)))
            (_, Just fareAmount)
              | L.PENDING `notElem` statuses && not (null allocationRows) ->
                let d = fareAmount - allocated
                 in (if d == 0 then Res.PAID else if d > 0 then Res.UNDERPAID else Res.OVERPAID, d)
            _ -> (Res.AWAITING, 0)
          perUtr = Map.toList $ Map.fromListWith (+) [(u, e.amount) | e <- allocationRows, Just u <- [e.utr]]
       in Just
            Res.OrderRow
              { orderId = orderId,
                rideId = listToMaybe (mapMaybe (.rideId) claims),
                driverId = listToMaybe (mapMaybe (.driverId) claims),
                bapId = fromMaybe "" (listToMaybe (mapMaybe (.collectorAppId) claims)),
                platformGrossFare = fare,
                claimedTotalAmount = sum (map (.amount) claims),
                receivedTotal = allocated,
                orderVerdict = verdict,
                orderDiff = diff,
                claimStatus = rejected <|> find (== L.PENDING) statuses <|> listToMaybe statuses,
                settlementUtrs = nub (mapMaybe (.utr) claims),
                allocations = [Res.OrderAllocation {utr = u, amount = a} | (u, a) <- perUtr],
                anyManuallyConfirmed = any ((== L.ADMIN) . (.actorType)) allocationRows,
                allSent = HS.member orderId view.reportedOrders,
                receivedAt = firstClaim.createdAt
              }

-- A completed ride no receiver_recon ever claimed: the whole fare is outstanding.
toUnmatchedGridRow :: DRide.Ride -> Res.ReconGridRow
toUnmatchedGridRow ride =
  Res.ReconGridRow
    { rideId = Just ride.id.getId,
      orderId = ride.bookingId.getId,
      buyerAppName = "",
      rideDateTime = Just (fromMaybe ride.createdAt ride.tripStartTime),
      driverId = Just ride.driverId.getId,
      grossFarePlatform = ride.fare,
      netReceivablePlatform = ride.fare,
      bapSettlementAmount = 0,
      amountDifference = fromMaybe 0 ride.fare,
      settlementDateBap = Nothing,
      settlementUtrs = [],
      reconciliationStatus = Res.Unmatched,
      payoutEligible = False,
      anyManuallyConfirmed = False,
      communicationStatus = "NOT_SENT"
    }
