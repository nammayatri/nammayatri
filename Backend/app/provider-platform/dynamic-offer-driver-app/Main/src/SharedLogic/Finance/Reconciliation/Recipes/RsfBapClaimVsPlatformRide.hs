{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide
  ( recipe,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as A
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Data.Time (nominalDay)
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import Lib.Finance.Reconciliation.Recipe (Recipe (..))
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Storage.Queries.Booking as QBooking
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
  fmap catMaybes . forM (Map.toList grouped) $ \(orderId, rows) -> do
    let firstRow = head rows
        bff = fromMaybe 0 firstRow.bffAmount
        gst = fromMaybe 0 firstRow.withholdingTaxGst
        tds = fromMaybe 0 firstRow.withholdingTaxTds
        ded = fromMaybe 0 firstRow.deductionByCollector
        totalClaimed = sum [r.claimedSettlementAmount | r <- rows]
    mbBooking <- B.runInReplica $ QBooking.findById (Id orderId)
    mbRide <- case mbBooking of
      Nothing -> pure Nothing
      Just booking -> B.runInReplica $ QRide.findOneByBookingId booking.id
    let rideFare = fromMaybe 0 (mbRide >>= (.fare))
        rideIdText = (.id.getId) <$> mbRide
        driverIdText = (.driverId.getId) <$> mbRide
        expectedNet = rideFare - bff - gst - tds - ded
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
              "expectedNet" .= expectedNet,
              "rsoIds" .= map (.id.getId) rows
            ]
    pure $
      Just
        ReconT.SourceRecord
          { srcId = orderId,
            srcEntityId = Just orderId,
            srcPartyId = driverIdText,
            srcAmount = expectedNet,
            srcMatchKey = Just orderId,
            srcComponent = Nothing,
            srcMeta = Just meta,
            srcTimestamp = firstRow.receivedAt,
            srcLifecycle = if isJust mbRide then ReconT.Settled else ReconT.InFlight
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
          tgtAmount = sum [r.claimedSettlementAmount | r <- rows],
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
        tolerance = 1.0
     in if abs diff <= tolerance
          then ReconT.ReconResult ReconT.MATCHED Nothing
          else
            if diff > 0
              then ReconT.ReconResult ReconT.HIGHER_IN_TARGET (Just "Underpaid")
              else ReconT.ReconResult ReconT.LOWER_IN_TARGET (Just "Overpaid")

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
      verdict = frameworkToRsfVerdict status
      meta = src.srcMeta
      rideId = meta >>= extractText "rideId"
      driverId = meta >>= extractText "driverId"
      rideFare = meta >>= extractMoney "rideFare"
      expectedNet = meta >>= extractMoney "expectedNet"
      totalClaimed = meta >>= extractMoney "totalClaimed"
      diffAmt = case (expectedNet, totalClaimed) of
        (Just en, Just tc) -> Just (en - tc)
        _ -> Nothing
  QRSOExtra.updateRsfReconResult orderId verdict diffAmt rideId driverId rideFare expectedNet

frameworkToRsfVerdict :: ReconT.ReconciliationStatus -> RSO.OrderReconVerdict
frameworkToRsfVerdict = \case
  ReconT.MATCHED -> RSO.PAID
  ReconT.HIGHER_IN_TARGET -> RSO.UNDERPAID
  ReconT.LOWER_IN_TARGET -> RSO.OVERPAID
  ReconT.MISSING_IN_TARGET -> RSO.NOT_PAID
  ReconT.MISSING_IN_SOURCE -> RSO.UNMATCHED
  ReconT.AWAITING_SETTLEMENT -> RSO.PENDING

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
