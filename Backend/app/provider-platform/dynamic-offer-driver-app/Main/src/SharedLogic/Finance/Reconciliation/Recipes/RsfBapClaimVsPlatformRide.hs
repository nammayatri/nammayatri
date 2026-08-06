{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide
  ( recipe,
  )
where

import qualified BecknV2.OnDemand.Utils.Common as BecknUtils
import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Types as A
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (nominalDay)
import qualified Domain.Types.Booking as DBooking
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import Lib.Finance.Reconciliation.Recipe (Recipe (..))
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified SharedLogic.RSFLedger as RSFLedger
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

-- Phase 2, stage B: checks 3 (claimed fare == our ride fare) and 4 (finder fee == what we
-- advertised) in one recon-framework pass over the orders whose BAP_CLAIM rows are still PENDING.
-- The write-back settles only the failures; a claim that passes stays PENDING for stage C.
recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = ReconT.ReconciliationSpec ReconT.ONDC_RSF ReconT.RSF_CLAIM ReconT.RIDE,
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
      syncSourceStatus = Just syncClaimStatus
    }

isPendingClaim :: L.RsfReconLedgerEntry -> Bool
isPendingClaim e = e.entryType == L.BAP_CLAIM && e.claimStatus == Just L.PENDING

fetchSources ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope range = do
  claims <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange scope.merchantId L.BAP_CLAIM range.from range.to
  claimsToSourceRecords scope (filter isPendingClaim claims)

fetchSourcesById ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  ReconT.MerchantScope ->
  [Text] ->
  m [ReconT.SourceRecord]
fetchSourcesById scope orderIds = do
  entries <- QLedger.findAllByMerchantAndOrderIds scope.merchantId orderIds
  claimsToSourceRecords scope (filter isPendingClaim entries)

-- One source per order: srcAmount is our fare, and both checks' inputs ride on srcMeta
-- (persisted verbatim as the recon entry's entityMeta).
claimsToSourceRecords ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  ReconT.MerchantScope ->
  [L.RsfReconLedgerEntry] ->
  m [ReconT.SourceRecord]
claimsToSourceRecords scope claims = do
  let grouped = Map.fromListWith (flip (<>)) [(orderId, [c]) | c <- claims, Just orderId <- [c.orderId]]
      orderIds = Map.keys grouped
  rideByOrderId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
  bookings <- B.runInReplica $ QBooking.findByIds (map Id orderIds)
  bffByOrderId <- fmap Map.fromList . forM bookings $ \booking -> (booking.id.getId,) <$> expectedBff scope booking
  pure
    [ ReconT.SourceRecord
        { srcId = orderId,
          srcEntityId = Just orderId,
          srcPartyId = (.driverId.getId) <$> mbRide,
          srcAmount = fromMaybe 0 mbFare,
          srcMatchKey = Just orderId,
          srcComponent = Nothing,
          srcMeta = Just meta,
          srcTimestamp = firstClaim.createdAt,
          srcLifecycle = if isJust mbFare then ReconT.Settled else ReconT.InFlight
        }
      | (orderId, rows@(firstClaim : _)) <- Map.toList grouped,
        let mbRide = Map.lookup orderId rideByOrderId
            mbFare = mbRide >>= RSFLedger.rideFare
            bffClaimed = firstClaim.bffAmount
            -- no stored fee for this booking means there is nothing to hold the claim against
            bffExpected = join (Map.lookup orderId bffByOrderId) <|> bffClaimed
            bffTypeOk = maybe True ((`elem` ["percent", "percentage"]) . T.toLower) firstClaim.bffType
            meta =
              A.object
                [ "totalClaimed" .= fromMaybe 0 firstClaim.orderPaymentAmount,
                  "rideFare" .= fromMaybe 0 mbFare,
                  "bffExpected" .= bffExpected,
                  "bffClaimed" .= bffClaimed,
                  "bffType" .= firstClaim.bffType,
                  "bffTypeOk" .= bffTypeOk,
                  "rideId" .= ((.id.getId) <$> mbRide),
                  "driverId" .= ((.driverId.getId) <$> mbRide),
                  "claimIds" .= map (.id.getId) rows
                ]
    ]

-- Our stored finder fee: the percentage this merchant advertises on-network for the booking's vehicle category.
expectedBff ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  ReconT.MerchantScope ->
  DBooking.Booking ->
  m (Maybe HighPrecMoney)
expectedBff scope booking = do
  mbConfig <- CQBC.findByMerchantIdDomainAndVehicle (Id scope.merchantId) "MOBILITY" (BecknUtils.mapServiceTierToCategory booking.vehicleServiceTier)
  pure $ mbConfig >>= (.buyerFinderFee) >>= readMaybe . T.unpack

-- The target is the collector's claim: payment.params.amount as carried on the order's PENDING legs.
fetchTargets ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets scope orderIds = do
  entries <- QLedger.findAllByMerchantAndOrderIds scope.merchantId (HS.toList orderIds)
  let grouped = Map.fromListWith (flip (<>)) [(orderId, [c]) | c <- filter isPendingClaim entries, Just orderId <- [c.orderId]]
  pure
    [ ReconT.TargetRecord
        { tgtId = orderId,
          tgtMatchKey = orderId,
          tgtAmount = fromMaybe 0 firstClaim.orderPaymentAmount,
          tgtMeta = Nothing,
          tgtSettlementId = firstClaim.settlementId,
          tgtSettlementDate = Just firstClaim.effectiveAt,
          tgtSettlementMode = Nothing,
          tgtRrn = firstClaim.utr,
          tgtTransactionDate = Just firstClaim.createdAt
        }
      | (orderId, firstClaim : _) <- Map.toList grouped
    ]

-- Fare first: the finder fee is a percentage of the fare, so it only means something once the fare agrees.
-- A fee mismatch reuses LOWER/HIGHER_IN_TARGET; mismatchReason is what tells the two apart.
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
    let fareDiff = sum (map (.srcAmount) srcs) - sum (map (.tgtAmount) tgts)
        meta = listToMaybe srcs >>= (.srcMeta)
        bffExpected = fromMaybe 0 (meta >>= extractMoney "bffExpected")
        bffClaimed = fromMaybe 0 (meta >>= extractMoney "bffClaimed")
        bffReason = "BFF_MISMATCH: expected " <> show bffExpected <> ", claimed " <> show bffClaimed
        bffTypeOk = meta >>= extractField "bffTypeOk"
     in if fareDiff > 0
          then ReconT.ReconResult ReconT.LOWER_IN_TARGET (Just "FARE_MISMATCH: underpaid")
          else
            if fareDiff < 0
              then ReconT.ReconResult ReconT.HIGHER_IN_TARGET (Just "FARE_MISMATCH: overpaid")
              else
                if bffExpected > bffClaimed || bffTypeOk == Just False
                  then ReconT.ReconResult ReconT.LOWER_IN_TARGET (Just bffReason)
                  else
                    if bffExpected < bffClaimed
                      then ReconT.ReconResult ReconT.HIGHER_IN_TARGET (Just bffReason)
                      else ReconT.ReconResult ReconT.MATCHED Nothing

-- Write-back: only a mismatch settles the claim here (REJECTED_FARE_MISMATCH / REJECTED_BFF_MISMATCH).
-- rejectionDiff is signed: ours − theirs, the fee delta converted to money via the fare.
syncClaimStatus ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  ReconT.SourceRecord ->
  ReconT.ReconciliationStatus ->
  m ()
syncClaimStatus src status =
  when (status `elem` [ReconT.LOWER_IN_TARGET, ReconT.HIGHER_IN_TARGET]) $ do
    let money key = fromMaybe 0 (src.srcMeta >>= extractMoney key)
        claimIds = fromMaybe [] (src.srcMeta >>= extractField "claimIds") :: [Text]
        fareDiff = money "rideFare" - money "totalClaimed"
        bffDiff = (money "bffExpected" - money "bffClaimed") * money "rideFare" / 100
        (claimStatus, rejectionDiff)
          | fareDiff /= 0 = (L.REJECTED_FARE_MISMATCH, fareDiff)
          | otherwise = (L.REJECTED_BFF_MISMATCH, bffDiff)
    forM_ claimIds $ \claimId -> QLedger.updateClaimStatus (Id claimId) claimStatus (Just rejectionDiff)

extractField :: (A.FromJSON a) => Text -> A.Value -> Maybe a
extractField key (A.Object o) = AKM.lookup (AK.fromText key) o >>= A.parseMaybe A.parseJSON
extractField _ _ = Nothing

extractMoney :: Text -> A.Value -> Maybe HighPrecMoney
extractMoney = extractField
