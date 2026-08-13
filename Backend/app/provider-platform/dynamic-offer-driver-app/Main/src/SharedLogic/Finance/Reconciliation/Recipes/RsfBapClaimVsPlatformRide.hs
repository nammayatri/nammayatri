{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide
  ( recipe,
    effectiveClaimedAmount,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as A
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Data.Time (nominalDay)
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import Lib.Finance.Reconciliation.Recipe (Recipe (..))
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Storage.Queries.Ride as QRide

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = mySpec,
      chunkPlan = ReconT.ByDay,
      settlementBuffer = 2 * nominalDay,
      grouping = ReconT.GroupByTargetKey,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      fetchSourcesByIds = fetchSourcesById,
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = rsfClassify,
      syncSourceStatus = Just (syncRsoStatus mySpec)
    }
  where
    mySpec = ReconT.ReconciliationSpec ReconT.ONDC_RSF ReconT.RSF_CLAIM ReconT.RIDE

fetchSources ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope range = do
  rsoRows <- QRSOExtra.findByMerchantIdAndReceivedAtRange scope.merchantId range.from range.to
  rsoRowsToSourceRecords rsoRows

fetchSourcesById ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  [Text] ->
  m [ReconT.SourceRecord]
fetchSourcesById _scope orderIds = do
  rsoRows <- QRSOExtra.findByOrderIds orderIds
  rsoRowsToSourceRecords rsoRows

rsoRowsToSourceRecords ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  [RSO.ReconSettlementOrder] ->
  m [ReconT.SourceRecord]
rsoRowsToSourceRecords rsoRows = do
  let grouped :: Map.Map Text [RSO.ReconSettlementOrder]
      grouped = Map.fromListWith (<>) [(r.orderId, [r]) | r <- rsoRows]
      orderIds = Map.keys grouped
  rides <- B.runInReplica $ QRide.findRidesByBookingId (map Id orderIds)
  let rideByBookingId = Map.fromListWith (\a b -> if a.createdAt >= b.createdAt then a else b) [(ride.bookingId.getId, ride) | ride <- rides]
  pure . catMaybes $
    flip map (Map.toList grouped) $ \(orderId, rows) ->
      let firstRow = head rows
          bff = fromMaybe 0 firstRow.bffAmount
          gst = fromMaybe 0 firstRow.withholdingTaxGst
          tds = fromMaybe 0 firstRow.withholdingTaxTds
          ded = fromMaybe 0 firstRow.deductionByCollector
          totalClaimed = sum (map effectiveClaimedAmount rows)
          mbRide = Map.lookup orderId rideByBookingId
          rideIdText = (.id.getId) <$> mbRide
          driverIdText = (.driverId.getId) <$> mbRide
          orderTimestamp = (\ride -> fromMaybe ride.createdAt ride.tripEndTime) <$> mbRide -- best way to sort instead of order sequence
          (rideFare, lifecycle) = case mbRide of
            Nothing -> (0, ReconT.InFlight)
            Just ride
              | Just fare <- ride.fare -> (fare, ReconT.Settled)
              | ride.status == DRide.CANCELLED -> (fromMaybe 0 ride.cancellationChargesOnCancel, ReconT.Cancelled)
              | otherwise -> (0, ReconT.InFlight)
          meta =
            A.object
              [ "totalClaimed" .= totalClaimed,
                "rideFare" .= rideFare,
                "rideId" .= rideIdText,
                "driverId" .= driverIdText,
                "bff" .= bff,
                "gst" .= gst,
                "tds" .= tds,
                "deductions" .= ded,
                "expectedNet" .= rideFare, -- No deductions to be calculated by system for now, not debt-driven
                "orderTimestamp" .= orderTimestamp,
                "rsoIds" .= map (.id.getId) rows
              ]
       in Just
            ReconT.SourceRecord
              { srcId = orderId,
                srcEntityId = Just orderId,
                srcPartyId = driverIdText,
                srcAmount = rideFare,
                srcMatchKey = Just orderId,
                srcComponent = Nothing,
                srcMeta = Just meta,
                srcTimestamp = firstRow.receivedAt,
                srcLifecycle = lifecycle
              }

fetchTargets ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope orderIds = do
  rsoRows <- QRSOExtra.findByOrderIds (HS.toList orderIds)
  let grouped :: Map.Map Text [RSO.ReconSettlementOrder]
      grouped = Map.fromListWith (<>) [(r.orderId, [r]) | r <- rsoRows]
  pure
    [ ReconT.TargetRecord
        { tgtId = orderId,
          tgtMatchKey = orderId,
          tgtAmount = sum (map effectiveClaimedAmount rows),
          tgtMeta = Nothing,
          tgtSettlementId = (.settlementId) <$> listToMaybe rows,
          tgtSettlementDate = (.settlementDate) <$> listToMaybe rows,
          tgtSettlementMode = (.settlementType) <$> listToMaybe rows,
          tgtRrn = Nothing,
          tgtTransactionDate = (.receivedAt) <$> listToMaybe rows
        }
      | (orderId, rows) <- Map.toList grouped
    ]

rsfClassify :: [ReconT.SourceRecord] -> [ReconT.TargetRecord] -> ReconT.ReconResult
rsfClassify srcs tgts
  | any ((== ReconT.InFlight) . (.srcLifecycle)) srcs =
    ReconT.ReconResult ReconT.AWAITING_SETTLEMENT (Just "Booking/ride not found yet")
  | null srcs && null tgts =
    ReconT.ReconResult ReconT.MATCHED Nothing
  | null srcs =
    ReconT.ReconResult ReconT.MISSING_IN_SOURCE (Just "No platform record for this claim")
  | null tgts =
    ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just "Claim not found")
  | otherwise =
    let expectedNet = sum (map (.srcAmount) srcs)
        totalClaimed = sum (map (.tgtAmount) tgts)
        diff = expectedNet - totalClaimed
     in if diff == 0
          then ReconT.ReconResult ReconT.MATCHED Nothing
          else
            if diff > 0
              then ReconT.ReconResult ReconT.LOWER_IN_TARGET (Just "Underpaid")
              else ReconT.ReconResult ReconT.HIGHER_IN_TARGET (Just "Overpaid")

-- | Order-level write-back for the automatic order-vs-ride check. No fresh
syncRsoStatus ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.ReconciliationSpec ->
  ReconT.SourceRecord ->
  ReconT.ReconciliationStatus ->
  m ()
syncRsoStatus _spec src status = do
  let orderId = fromMaybe "" src.srcEntityId
      meta = src.srcMeta
      rideId = meta >>= extractText "rideId"
      driverId = meta >>= extractText "driverId"
      rideFare = fromMaybe 0 (meta >>= extractMoney "rideFare")
      totalClaimed = fromMaybe 0 (meta >>= extractMoney "totalClaimed")
      orderTimestamp = meta >>= extractUTCTime "orderTimestamp"
      diffAmt = rideFare - totalClaimed
      verdict
        | diffAmt == 0 = RSO.PAID
        | diffAmt > 0 = RSO.UNDERPAID
        | otherwise = RSO.OVERPAID

  case status of
    ReconT.AWAITING_SETTLEMENT -> pure ()
    _ -> QRSOExtra.updateRsfReconResult orderId verdict (Just diffAmt) rideId driverId (Just rideFare) (Just rideFare) orderTimestamp

effectiveClaimedAmount :: RSO.ReconSettlementOrder -> HighPrecMoney
effectiveClaimedAmount rso = case rso.allocatedBankCash of
  Just amount -> amount
  Nothing -> rso.claimedSettlementAmount - fromMaybe 0 rso.diffAmount

extractText :: Text -> A.Value -> Maybe Text
extractText key (A.Object o) = case A.parseMaybe (\_ -> o A..:? AK.fromText key) () of
  Just v -> v
  Nothing -> Nothing
extractText _ _ = Nothing

extractMoney :: Text -> A.Value -> Maybe HighPrecMoney
extractMoney key (A.Object o) = case A.parseMaybe (\_ -> o A..:? AK.fromText key) () of
  Just v -> v
  Nothing -> Nothing
extractMoney _ _ = Nothing

extractUTCTime :: Text -> A.Value -> Maybe UTCTime
extractUTCTime key (A.Object o) = case A.parseMaybe (\_ -> o A..:? AK.fromText key) () of
  Just v -> v
  Nothing -> Nothing
extractUTCTime _ _ = Nothing
