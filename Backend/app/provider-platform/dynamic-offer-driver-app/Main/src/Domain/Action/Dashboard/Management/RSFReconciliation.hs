{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Domain.Action.Dashboard.Management.RSFReconciliation
  ( getRSFReconciliationRsfMessages,
    getRSFReconciliationRsfMessagesUtrs,
    getRSFReconciliationRsfMessagesOrders,
    postRSFReconciliationRsfMessagesSend,
    getRSFReconciliationRsfUtrs,
    getRSFReconciliationRsfUtr,
    postRSFReconciliationRsfUtrBankVerify,
    postRSFReconciliationRsfOrdersConfirm,
    getRSFReconciliationRsfReconGrid,
    getRSFReconciliationRsfReconUnmatched,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation as Res
import Control.Applicative ((<|>))
import qualified Data.HashSet as HS
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
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

getRSFReconciliationRsfMessages :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.MessageBatchListRes
getRSFReconciliationRsfMessages merchantShortId _opCity mbBapId mbFrom mbLimit mbOffset mbTo = do
  merchantId <- findMerchantId merchantShortId
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  envelopes <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange merchantId L.MESSAGE_RECEIVED from to
  let wanted = sortOn (Down . (.createdAt)) $ filter (\e -> maybe True (\b -> e.bapId == Just b) mbBapId) envelopes
  summaries <- fmap catMaybes . forM wanted $ \envelope -> do
    msgEntries <- maybe (pure []) (QLedger.findAllByMerchantAndMessageId merchantId) envelope.messageId
    let claims = RSFLedger.ofType L.BAP_CLAIM msgEntries
        (orderIds, utrs) = maybe ([], []) (\(_, o, u) -> (o, u)) (RSFLedger.parseInbound envelope)
        alreadySent = not . null $ RSFLedger.ofType L.ORDER_VARIANCE msgEntries
    pure $
      if alreadySent
        then Nothing
        else
          Just
            Res.MessageBatchSummary
              { messageId = fromMaybe "" envelope.messageId,
                bapId = fromMaybe "" envelope.bapId,
                receivedAt = envelope.createdAt,
                utrCount = length (if null utrs then nub (mapMaybe (.utr) claims) else utrs),
                orderCount = length (if null orderIds then nub (mapMaybe (.orderId) claims) else orderIds),
                processed = not . null $ RSFLedger.ofType L.MESSAGE_PROCESSED msgEntries
              }
  pure $ Res.MessageBatchListRes {totalItems = length summaries, batches = paginate mbLimit mbOffset 20 summaries}

-- The order ids and UTRs an inbound message carried, from its stored payload.
messageScope :: Text -> Text -> Environment.Flow ([Text], [Text])
messageScope merchantId messageId = do
  envelope <- QLedger.findMessageReceived merchantId messageId >>= fromMaybeM (InvalidRequest "Message not found")
  (_, orderIds, utrs) <- fromMaybeM (InternalError "RSF: stored receiver_recon payload does not parse") $ RSFLedger.parseInbound envelope
  pure (orderIds, utrs)

getRSFReconciliationRsfMessagesUtrs :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow Res.MessageBatchUtrListRes
getRSFReconciliationRsfMessagesUtrs merchantShortId _opCity messageId = do
  merchantId <- findMerchantId merchantShortId
  (_, utrs) <- messageScope merchantId messageId
  view <- loadView merchantId [] utrs
  pure $ Res.MessageBatchUtrListRes {utrs = mapMaybe (toUtrSummary view) utrs}

getRSFReconciliationRsfMessagesOrders :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Res.MessageBatchOrderListRes
getRSFReconciliationRsfMessagesOrders merchantShortId _opCity messageId mbLimit mbOffset = do
  merchantId <- findMerchantId merchantShortId
  (orderIds, _) <- messageScope merchantId messageId
  let page = paginate mbLimit mbOffset 50 orderIds
  view <- loadView merchantId page []
  pure $ Res.MessageBatchOrderListRes {totalItems = length orderIds, orders = mapMaybe (toOrderRow view) page}

postRSFReconciliationRsfMessagesSend :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfMessagesSend merchantShortId _opCity messageId = do
  merchantId <- findMerchantId merchantShortId
  CallRSF.sendOnReceiverRecon (Id merchantId) messageId
  pure APISuccess.Success

getRSFReconciliationRsfUtrs :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.UtrListRes
getRSFReconciliationRsfUtrs merchantShortId _opCity mbBapId mbFrom mbVerified mbLimit mbOffset mbTo = do
  merchantId <- findMerchantId merchantShortId
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  claims <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange merchantId L.BAP_CLAIM from to
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

-- Phase 3 (step 10 + 11): BANK_RECEIPT, then allocation over the UTR's ACCEPTED claims.
postRSFReconciliationRsfUtrBankVerify :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Res.BankVerifyReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfUtrBankVerify merchantShortId _opCity utr req = do
  merchantId <- findMerchantId merchantShortId
  RSFLedger.verifyUtr merchantId utr req.bankVerifiedAmount req.verifiedBy req.reason
  logInfo $ "RSF bank verify: utr=" <> utr <> " amount=" <> show req.bankVerifiedAmount
  pure APISuccess.Success

-- Phase 3 (step 11a): manual re-split of one order's allocation on one UTR.
postRSFReconciliationRsfOrdersConfirm :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Res.ManualConfirmReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfOrdersConfirm merchantShortId _opCity orderId req = do
  merchantId <- findMerchantId merchantShortId
  RSFLedger.reallocateOrder merchantId orderId req.utr req.confirmedAmount req.confirmedBy req.reason
  logInfo $ "RSF manual allocation: orderId=" <> orderId <> " utr=" <> req.utr <> " by=" <> req.confirmedBy <> " amount=" <> show req.confirmedAmount
  pure APISuccess.Success

getRSFReconciliationRsfReconGrid :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Res.ReconTabStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.ReconGridListRes
getRSFReconciliationRsfReconGrid merchantShortId _opCity mbBapId mbFrom mbLimit mbManuallyConfirmedOnly mbOffset mbStatus mbTo = do
  when (mbStatus == Just Res.Unmatched) $
    throwError $ InvalidRequest "status=Unmatched is not valid on /rsf/recon/grid; use /rsf/recon/unmatched"
  merchantId <- findMerchantId merchantShortId
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  claims <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange merchantId L.BAP_CLAIM from to
  let orderIds = nub $ mapMaybe (.orderId) claims
  view <- loadView merchantId orderIds []
  let allGridRows = mapMaybe (toReconGridRow view) orderIds
      byBap = maybe allGridRows (\b -> filter (\r -> r.buyerAppName == b) allGridRows) mbBapId
      byStatus = maybe byBap (\s -> filter (\r -> r.reconciliationStatus == s) byBap) mbStatus
      byConfirmed = if mbManuallyConfirmedOnly == Just True then filter (.anyManuallyConfirmed) byStatus else byStatus
  pure $ Res.ReconGridListRes {totalItems = length byConfirmed, rows = paginate mbLimit mbOffset 20 byConfirmed}

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
   in case sortOn (.createdAt) claims of
        [] -> Nothing
        (firstClaim : _) ->
          Just
            Res.UtrSummary
              { utr = utr,
                bapId = fromMaybe "" firstClaim.collectorAppId,
                claimedTotalAmount = RSFLedger.claimedOnUtr utr view.utrEntries,
                bankVerifiedAmount = if verified then Just (RSFLedger.receivedOnUtr utr view.utrEntries) else Nothing,
                unallocatedAmount = if verified then RSFLedger.unallocatedOnUtr utr view.utrEntries else 0,
                totalOrders = length claims,
                reportedStatus = Map.lookup utr view.utrStates >>= (.reportedStatus),
                createdAt = firstClaim.createdAt
              }

-- One order, live-folded: its standing claim legs, what the bank allocation gave it, and the verdict.
data OrderFold = OrderFold
  { claims :: [RSFLedger.Entry],
    fare :: Maybe HighPrecMoney,
    claimedTotal :: HighPrecMoney,
    allocated :: HighPrecMoney,
    claimStatus :: Maybe L.RsfClaimStatus,
    verdict :: Res.OrderReconVerdict,
    diff :: HighPrecMoney,
    manuallyConfirmed :: Bool,
    receivedAt :: UTCTime
  }

foldOrder :: LedgerView -> Text -> Maybe OrderFold
foldOrder view orderId =
  let claims = sortOn (.createdAt) [c | ((o, _), c) <- Map.toList (RSFLedger.standingClaims view.orderEntries), o == orderId]
      fare = Map.lookup orderId view.rideByOrderId >>= RSFLedger.rideFare
      allocated = RSFLedger.allocatedToOrder orderId view.orderEntries
      statuses = mapMaybe (.claimStatus) claims
      rejected = find (`notElem` [L.PENDING, L.ACCEPTED]) statuses
      allVerified = all (\c -> maybe False (`RSFLedger.isUtrVerified` view.utrEntries) c.utr) claims
      (verdict, diff) = case (rejected, fare) of
        (Just _, _) -> (Res.REJECTED, maybe 0 abs (listToMaybe (mapMaybe (.rejectionDiff) claims)))
        (_, Just fareAmount)
          | L.PENDING `notElem` statuses && allVerified ->
            let d = fareAmount - allocated
             in (if d == 0 then Res.PAID else if d > 0 then Res.UNDERPAID else Res.OVERPAID, d)
        _ -> (Res.AWAITING, 0)
   in case claims of
        [] -> Nothing
        (firstClaim : _) ->
          Just
            OrderFold
              { claims = claims,
                fare = fare,
                claimedTotal = sum (map (.amount) claims),
                allocated = allocated,
                claimStatus = rejected <|> find (== L.PENDING) statuses <|> listToMaybe statuses,
                verdict = verdict,
                diff = diff,
                manuallyConfirmed = any (\e -> e.orderId == Just orderId && e.actorType == L.ADMIN) (RSFLedger.ofType L.BANK_ALLOCATION view.orderEntries),
                receivedAt = firstClaim.createdAt
              }

toOrderRow :: LedgerView -> Text -> Maybe Res.OrderRow
toOrderRow view orderId =
  foldOrder view orderId <&> \o ->
    Res.OrderRow
      { orderId = orderId,
        rideId = listToMaybe (mapMaybe (.rideId) o.claims),
        driverId = listToMaybe (mapMaybe (.driverId) o.claims),
        platformGrossFare = o.fare,
        claimedTotalAmount = o.claimedTotal,
        receivedTotal = o.allocated,
        orderVerdict = o.verdict,
        orderDiff = o.diff,
        claimStatus = o.claimStatus,
        settlementUtrs = nub (mapMaybe (.utr) o.claims),
        anyManuallyConfirmed = o.manuallyConfirmed,
        allSent = HS.member orderId view.reportedOrders,
        receivedAt = o.receivedAt
      }

toReconGridRow :: LedgerView -> Text -> Maybe Res.ReconGridRow
toReconGridRow view orderId =
  foldOrder view orderId <&> \o ->
    let mbRide = Map.lookup orderId view.rideByOrderId
        allSent = HS.member orderId view.reportedOrders
        reconStatus = case o.verdict of
          Res.PAID -> Res.Matched
          Res.AWAITING -> Res.Pending
          _ -> Res.Mismatch
     in Res.ReconGridRow
          { rideId = (\r -> r.id.getId) <$> mbRide,
            orderId = orderId,
            buyerAppName = fromMaybe "" (listToMaybe (mapMaybe (.collectorAppId) o.claims)),
            rideDateTime = (\r -> fromMaybe r.createdAt r.tripStartTime) <$> mbRide,
            driverId = listToMaybe (mapMaybe (.driverId) o.claims),
            grossFarePlatform = o.fare,
            netReceivablePlatform = o.fare,
            bapSettlementAmount = o.claimedTotal,
            amountDifference = o.diff,
            settlementDateBap = listToMaybe (sortOn Down (map (.effectiveAt) o.claims)),
            settlementUtrs = nub (mapMaybe (.utr) o.claims),
            reconciliationStatus = reconStatus,
            payoutEligible = reconStatus == Res.Matched && allSent,
            anyManuallyConfirmed = o.manuallyConfirmed,
            communicationStatus = if allSent then "SENT" else "PENDING"
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
