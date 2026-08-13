module Domain.Action.Dashboard.Management.RSFReconciliation
  ( getRSFReconciliationRsfSettlements,
    getRSFReconciliationRsfSettlementsUtrs,
    getRSFReconciliationRsfSettlementsOrders,
    postRSFReconciliationRsfSettlementsSend,
    getRSFReconciliationRsfUtrs,
    getRSFReconciliationRsfUtr,
    postRSFReconciliationRsfUtrBankVerify,
    postRSFReconciliationRsfOrdersConfirm,
    getRSFReconciliationRsfReconGrid,
    getRSFReconciliationRsfReconUnmatched,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation as Res
import qualified Data.HashSet as HS
import Data.List (nub)
import qualified Data.List
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant as M
import qualified Domain.Types.Ride as DRide
import qualified Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as RUS
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlement as QRUS
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlementExtra as QRUSExtra
import qualified Sequelize as Se
import qualified SharedLogic.CallRSF as CallRSF
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide as RsfOrderRecipe
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfUtrVsBankDeposit as RsfUtrRecipe
import qualified SharedLogic.RSFOrderStatus as RSFOrderStatus
import qualified Storage.Beam.Ride as BeamR
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideExtra as QRideExtra

getRSFReconciliationRsfSettlements :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.SettlementBatchListRes
getRSFReconciliationRsfSettlements merchantShortId _opCity mbBapId mbFrom mbLimit mbOffset mbTo = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset

  (total, summaries) <- QRSOExtra.listBatchSummariesForDashboard (getId merchant.id) mbBapId from to limit offset

  -- Resolve the per-batch bapId by fetching one representative row per
  -- settlement_id and reading its UTR row's bapId. Handles the case where
  -- the request didn't filter by bapId at all (we must still populate the
  -- field with the real value, not echo an empty filter).
  mapped <- forM summaries $ \(sid, uCnt, oCnt, uoC, earliest) -> do
    rows <- QRSOExtra.findBySettlementId sid
    let mbUtrId = listToMaybe (mapMaybe (.utrSettlementId) rows)
    bapId <- case mbUtrId of
      Nothing -> pure (fromMaybe "" mbBapId)
      Just utrId -> do
        mbUtr <- QRUS.findById utrId
        pure (maybe (fromMaybe "" mbBapId) (.bapId) mbUtr)
    pure $
      Res.SettlementBatchSummary
        { settlementId = sid,
          bapId = bapId,
          receivedAt = earliest,
          utrCount = uCnt,
          orderCount = oCnt,
          unsentOrderCount = uoC
        }

  pure $ Res.SettlementBatchListRes {totalItems = total, batches = mapped}

getRSFReconciliationRsfSettlementsUtrs :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow Res.SettlementBatchUtrListRes
getRSFReconciliationRsfSettlementsUtrs merchantShortId _opCity settlementId = do
  _merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  orders <- QRSOExtra.findBySettlementId settlementId
  let utrIds = nub (mapMaybe (.utrSettlementId) orders)
  utrs <- QRUSExtra.findByIds utrIds
  let mapped = map toUtrSummary utrs
  pure $ Res.SettlementBatchUtrListRes {utrs = mapped}

getRSFReconciliationRsfSettlementsOrders :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Res.SettlementBatchOrderListRes
getRSFReconciliationRsfSettlementsOrders merchantShortId _opCity settlementId limit offset = do
  _merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  orders <- QRSOExtra.findBySettlementId settlementId
  let limitInt = fromMaybe 50 limit
      offsetInt = fromMaybe 0 offset
      orderIds = nub (map (.orderId) orders)
      paginatedOrderIds = take limitInt (drop offsetInt orderIds)

  -- Fetch every row for each paginated order, not just the ones in this
  -- settlement batch -- an order can span multiple batches (correction UTR
  -- under a different settlement_id), and computeOrderStatus needs the
  -- full row set for the order to give the right answer.
  allRowsForOrders <- QRSOExtra.findByOrderIds paginatedOrderIds
  let rowsByOrderId = Map.fromListWith (<>) [(r.orderId, [r]) | r <- allRowsForOrders]
      mappedOrders = mapMaybe (toOrderRow . snd) (Map.toList rowsByOrderId)

  pure $ Res.SettlementBatchOrderListRes {totalItems = length orderIds, orders = mappedOrders}

postRSFReconciliationRsfSettlementsSend :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfSettlementsSend merchantShortId _opCity settlementId = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  CallRSF.sendOnReceiverRecon merchant.id settlementId
  pure APISuccess.Success

getRSFReconciliationRsfUtrs :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.UtrListRes
getRSFReconciliationRsfUtrs merchantShortId _opCity mbBapId mbFrom mbVerified mbLimit mbOffset mbTo = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
  (total, utrs) <- QRUSExtra.listUtrSummariesForDashboard (getId merchant.id) mbBapId mbVerified from to limit offset
  let mapped = map toUtrSummary utrs
  pure $ Res.UtrListRes {totalItems = total, utrs = mapped}

getRSFReconciliationRsfUtr :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> (Kernel.Types.Id.Id RUS.ReconUtrSettlement) -> Environment.Flow Res.UtrDetailRes
getRSFReconciliationRsfUtr merchantShortId _opCity utrId = do
  _merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  utr <- QRUS.findById utrId >>= fromMaybeM (InvalidRequest "UTR not found")

  -- findByUtrSettlementIds takes UTR *row ids* (the utr_settlement_id FK on
  -- RSO), not the UTR text field. Pass getId utr.id.
  ordersUnderUtr <- QRSOExtra.findByUtrSettlementIds [getId utr.id]

  -- Fetch every row for every order that touches this UTR (an order might
  -- span multiple UTRs -- correction/split cases -- and computeOrderStatus
  -- needs the full row set for the order to be correct).
  let orderIds = nub (map (.orderId) ordersUnderUtr)
  allOrderRows <- QRSOExtra.findByOrderIds orderIds
  let rowsByOrderId = Map.fromListWith (<>) [(r.orderId, [r]) | r <- allOrderRows]
      mappedOrders = mapMaybe (toOrderRow . snd) (Map.toList rowsByOrderId)

  pure $ Res.UtrDetailRes {utr = toUtrSummary utr, orders = mappedOrders}

postRSFReconciliationRsfUtrBankVerify :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> (Kernel.Types.Id.Id RUS.ReconUtrSettlement) -> Res.BankVerifyReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfUtrBankVerify merchantShortId _opCity utrId req = do
  _merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  QRUSExtra.updateBankVerifiedAmount utrId req.bankVerifiedAmount
  utr <- QRUS.findById utrId >>= fromMaybeM (InvalidRequest "UTR not found")
  let scope = ReconT.MerchantScope (fromMaybe "" utr.merchantId) (fromMaybe "" utr.merchantOperatingCityId)
  ReconRunner.reconcileSources RsfUtrRecipe.recipe scope [ReconT.SourceId $ getId utrId]
  logInfo $ "RSF bank verify: utrId=" <> getId utrId <> " amount=" <> show req.bankVerifiedAmount
  pure APISuccess.Success

postRSFReconciliationRsfOrdersConfirm :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> (Kernel.Types.Id.Id RSO.ReconSettlementOrder) -> Res.ManualConfirmReq -> Environment.Flow APISuccess.APISuccess
postRSFReconciliationRsfOrdersConfirm merchantShortId _opCity rsoId req = do
  _merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  Hedis.withLockRedis ("RsfConfirmLock:" <> getId rsoId) 30 $ do
    rsos <- QRSO.findByIds [getId rsoId]
    rso <- case rsos of
      [] -> throwError $ InvalidRequest "RSO not found"
      (r : _) -> pure r
    when (isNothing rso.platformGrossFare) $
      throwError $ InvalidRequest "Ride not resolved yet for this order -- cannot confirm before the fare is known"
    when (rso.reconciliationStatus == Just "SENT") $
      throwError $ InvalidRequest "Order already sent to BAP"
    when (isJust rso.manuallyConfirmedAt) $
      throwError $ InvalidRequest "Order already manually confirmed"
    now <- getCurrentTime

    siblingRows <- QRSO.findByOrderId rso.orderId
    let fare = fromMaybe 0 rso.platformGrossFare
        otherRowsClaimed = sum [RsfOrderRecipe.effectiveClaimedAmount r | r <- siblingRows, r.id /= rso.id]
        totalClaimed = otherRowsClaimed + req.confirmedAmount
        diffAmt = fare - totalClaimed
        verdict
          | diffAmt == 0 = RSO.PAID
          | diffAmt > 0 = RSO.UNDERPAID
          | otherwise = RSO.OVERPAID

    QRSOExtra.updateManualConfirmation rsoId now req.confirmedBy req.reason req.confirmedAmount verdict (Just diffAmt)
    logInfo $ "RSF manual confirm: orderId=" <> rso.orderId <> " by=" <> req.confirmedBy <> " amount=" <> show req.confirmedAmount

    let scope = ReconT.MerchantScope (fromMaybe "" rso.merchantId) (fromMaybe "" rso.merchantOperatingCityId)
    ReconRunner.reconcileSources RsfOrderRecipe.recipe scope [ReconT.SourceId rso.orderId]
  pure APISuccess.Success

toUtrSummary :: RUS.ReconUtrSettlement -> Res.UtrSummary
toUtrSummary u =
  Res.UtrSummary
    { id = u.id,
      utr = u.utr,
      bapId = u.bapId,
      claimedTotalAmount = u.claimedTotalAmount,
      bankVerifiedAmount = u.bankVerifiedAmount,
      totalOrders = u.totalOrders,
      createdAt = u.createdAt
    }

-- | Build one OrderRow from all RSO rows sharing an orderId. Returns
-- Nothing for an empty input rather than crashing (never happens in
-- practice -- Map.fromListWith never produces empty groups -- but keeps
-- the function total).
toOrderRow :: [RSO.ReconSettlementOrder] -> Maybe Res.OrderRow
toOrderRow [] = Nothing
toOrderRow rows@(firstRow : _) =
  let orderId = firstRow.orderId
      rideId = listToMaybe (mapMaybe (.rideId) rows)
      driverId = listToMaybe (mapMaybe (.driverId) rows)
      platformGrossFare = listToMaybe (mapMaybe (.platformGrossFare) rows)
      claimedTotalAmount = sum (map (.claimedSettlementAmount) rows)
      receivedTotal = sum (map RsfOrderRecipe.effectiveClaimedAmount rows)

      (verdict, diffAmt) = verdictAndDiffFor platformGrossFare rows

      -- The actual UTR text ("UTR-UC1-HAPPY"), one entry per distinct UTR
      -- this order touched. Deduped; matches the "Settlement UTR(Array)"
      -- field the MSIL admin spec asks for.
      settlementUtrs = nub (map (.settlementReferenceNo) rows)
      anyManuallyConfirmed = any (\r -> isJust r.manuallyConfirmedAt) rows
      allSent = all (\r -> r.reconciliationStatus == Just "SENT") rows
      receivedAt = Data.List.minimum (map (.receivedAt) rows)
   in Just
        Res.OrderRow
          { rsoIds = map (.id) rows,
            orderId = orderId,
            rideId = rideId,
            driverId = driverId,
            platformGrossFare = platformGrossFare,
            claimedTotalAmount = claimedTotalAmount,
            receivedTotal = receivedTotal,
            orderVerdict = verdict,
            orderDiff = diffAmt,
            settlementUtrs = settlementUtrs,
            anyManuallyConfirmed = anyManuallyConfirmed,
            allSent = allSent,
            receivedAt = receivedAt
          }

-- | A ride not yet resolved (platformGrossFare unknown) must not be forced
-- through the live-compute formula against a fake 0 fare -- that would
-- misclassify a genuinely in-flight order as OVERPAID the moment any claim
-- exists. Report it as PENDING instead; only delegate to the real formula
-- once the fare is actually known.
verdictAndDiffFor :: Maybe HighPrecMoney -> [RSO.ReconSettlementOrder] -> (RSO.OrderReconVerdict, HighPrecMoney)
verdictAndDiffFor Nothing _ = (RSO.PENDING, 0)
verdictAndDiffFor (Just fare) rows = RSFOrderStatus.computeOrderStatus fare rows

mapVerdictToTabStatus :: RSO.OrderReconVerdict -> Res.ReconTabStatus
mapVerdictToTabStatus RSO.PAID = Res.Matched
mapVerdictToTabStatus RSO.UNDERPAID = Res.Mismatch
mapVerdictToTabStatus RSO.OVERPAID = Res.Mismatch
mapVerdictToTabStatus RSO.PENDING = Res.Pending
mapVerdictToTabStatus RSO.NOT_PAID = Res.Pending
mapVerdictToTabStatus RSO.UNMATCHED = Res.Pending

getRSFReconciliationRsfReconGrid :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Res.ReconTabStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.ReconGridListRes
getRSFReconciliationRsfReconGrid merchantShortId _opCity mbBapId mbFrom mbLimit mbManuallyConfirmedOnly mbOffset mbStatus mbTo = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset

  allRows <- QRSOExtra.findByMerchantIdAndReceivedAtRange (getId merchant.id) from to
  let orderGroups = Map.elems (Map.fromListWith (<>) [(r.orderId, [r]) | r <- allRows])
      utrIds = nub (mapMaybe (.utrSettlementId) allRows)
      orderIds = nub (map (.orderId) allRows)

  utrs <- QRUSExtra.findByIds utrIds
  let bapIdByUtrId = Map.fromList [(getId u.id, u.bapId) | u <- utrs]

  rides <- B.runInReplica $ QRide.findRidesByBookingId (map Id orderIds)
  let rideByOrderId = Map.fromListWith (\a b -> if a.createdAt >= b.createdAt then a else b) [(r.bookingId.getId, r) | r <- rides]

  let allGridRows = mapMaybe (toReconGridRow rideByOrderId bapIdByUtrId) orderGroups
      byBap = maybe allGridRows (\b -> filter (\r -> r.buyerAppName == b) allGridRows) mbBapId
      byStatus = maybe byBap (\s -> filter (\r -> r.reconciliationStatus == s) byBap) mbStatus
      byConfirmed = case mbManuallyConfirmedOnly of
        Just True -> filter (.anyManuallyConfirmed) byStatus
        _ -> byStatus
      total = length byConfirmed
      paginated = take limit (drop offset byConfirmed)

  pure $ Res.ReconGridListRes {totalItems = total, rows = paginated}

getRSFReconciliationRsfReconUnmatched :: Kernel.Types.Id.ShortId M.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow Res.ReconGridListRes
getRSFReconciliationRsfReconUnmatched merchantShortId _opCity mbFrom mbLimit mbOffset mbTo = do
  merchant <- CQMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  from <- fromMaybeM (InvalidRequest "Missing 'from'") mbFrom
  to <- fromMaybeM (InvalidRequest "Missing 'to'") mbTo
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset

  completedRides <-
    QRideExtra.findAllRidesWithSeConditionsCreatedAtDesc
      [ Se.And
          [ Se.Is BeamR.merchantId $ Se.Eq (Just (getId merchant.id)),
            Se.Is BeamR.status $ Se.Eq DRide.COMPLETED,
            Se.Is BeamR.createdAt $ Se.GreaterThanOrEq from,
            Se.Is BeamR.createdAt $ Se.LessThan to
          ]
      ]
  let orderIds = map (\r -> r.bookingId.getId) completedRides
  claimedRows <- QRSOExtra.findByOrderIds orderIds
  let claimedOrderIds = HS.fromList (map (.orderId) claimedRows)
      unmatchedRides = filter (\r -> not (HS.member r.bookingId.getId claimedOrderIds)) completedRides
      total = length unmatchedRides
      paginated = take limit (drop offset unmatchedRides)

  pure $ Res.ReconGridListRes {totalItems = total, rows = map toUnmatchedGridRow paginated}

-- | Every RSO row this order shares. `rideByOrderId`/`bapIdByUtrId` are
-- pre-fetched maps (batched, not N+1) covering every order/UTR in the
-- current page's date range.
toReconGridRow :: Map.Map Text DRide.Ride -> Map.Map Text Text -> [RSO.ReconSettlementOrder] -> Maybe Res.ReconGridRow
toReconGridRow _ _ [] = Nothing
toReconGridRow rideByOrderId bapIdByUtrId rows@(firstRow : _) =
  let orderId = firstRow.orderId
      mbRide = Map.lookup orderId rideByOrderId
      platformGrossFare = listToMaybe (mapMaybe (.platformGrossFare) rows)
      platformNetReceivable = listToMaybe (mapMaybe (.platformNetReceivable) rows)
      bapSettlementAmount = sum (map RsfOrderRecipe.effectiveClaimedAmount rows)
      (verdict, diffAmt) = verdictAndDiffFor platformGrossFare rows
      buyerAppName = fromMaybe "" $ listToMaybe (mapMaybe (\r -> r.utrSettlementId >>= \uid -> Map.lookup (getId uid) bapIdByUtrId) rows)
      rideDateTime = (\r -> fromMaybe r.createdAt r.tripStartTime) <$> mbRide
      settlementDateBap = if null rows then Nothing else Just (Data.List.maximum (map (.settlementDate) rows))
      settlementUtrs = nub (map (.settlementReferenceNo) rows)
      anyManuallyConfirmed = any (isJust . (.manuallyConfirmedAt)) rows
      allSent = all (\r -> r.reconciliationStatus == Just "SENT") rows
      reconStatus = mapVerdictToTabStatus verdict
      payoutEligible = reconStatus == Res.Matched && allSent
      communicationStatus = if allSent then "SENT" else "PENDING"
   in Just
        Res.ReconGridRow
          { rsoIds = map (.id) rows,
            rideId = (\r -> r.id.getId) <$> mbRide,
            orderId = orderId,
            buyerAppName = buyerAppName,
            rideDateTime = rideDateTime,
            driverId = listToMaybe (mapMaybe (.driverId) rows),
            grossFarePlatform = platformGrossFare,
            netReceivablePlatform = platformNetReceivable,
            bapSettlementAmount = bapSettlementAmount,
            amountDifference = diffAmt,
            settlementDateBap = settlementDateBap,
            settlementUtrs = settlementUtrs,
            reconciliationStatus = reconStatus,
            payoutEligible = payoutEligible,
            anyManuallyConfirmed = anyManuallyConfirmed,
            communicationStatus = communicationStatus
          }

-- | A completed ride with zero RSO rows -- never claimed by the BAP in any
-- receiver_recon message. No claim exists, so buyerAppName/settlementUtrs
-- have nothing real to show; amountDifference is the full fare (nothing
-- received against it at all).
toUnmatchedGridRow :: DRide.Ride -> Res.ReconGridRow
toUnmatchedGridRow ride =
  Res.ReconGridRow
    { rsoIds = [],
      rideId = Just ride.id.getId,
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
