{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Surge-table management + pricing observability (fare-policy revamp
-- Phase 3+4). Invariants:
--   * at most one ACTIVE SurgeConfig per (city, tier, timeBounds) — status
--     transitions are serialized per (city, tier) by a Redis lock, and
--     activate archives the previous ACTIVE before flipping the new one
--     (archive-first: a crash between the two writes leaves NO active config,
--     which safely falls back to the json-logic path, never two actives);
--   * activation validates guardrails and rejects a jump beyond
--     maxDeltaPerUpdate (the incoming config's, else the outgoing ACTIVE's)
--     vs the previous ACTIVE version;
--   * every status/row change clears the city+tier cache, so activation and
--     rollback take effect on the next NEW search (already-priced
--     transactions stay pinned to their version — see SharedLogic.FarePolicy).
module Domain.Action.Dashboard.Management.Pricing
  ( getPricingSurgeList,
    postPricingSurgeCreate,
    postPricingSurgeUpdate,
    postPricingSurgeStatus,
    postPricingSurgePreview,
    getPricingObservabilityEstimate,
    getPricingObservabilityHealth,
  )
where

import qualified API.Types.ProviderPlatform.Management.Pricing as Common
import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.Common as DCommon
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.Extra.SurgeConfig as DSCE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SurgeConfig as DSC
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SurgeConfig as SSC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SurgeConfig as CQSC
import qualified Storage.Clickhouse.Estimate as CHEst
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.SurgeConfig as QSC

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  pure (merchant, merchantOpCity)

--------------------------------------------------------------------------------
-- surge config CRUD
--------------------------------------------------------------------------------

getPricingSurgeList :: ShortId DM.Merchant -> Context.City -> Maybe ServiceTierType -> Flow Common.PricingSurgeConfigListRes
getPricingSurgeList merchantShortId opCity mbServiceTier = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  configs <- QSC.findAllByMerchantOperatingCityId merchantOpCity.id
  let filtered = filter (\c -> maybe True (== c.vehicleServiceTier) mbServiceTier) configs
      sorted = sortOn (Down . (.version)) filtered
  pure $ Common.PricingSurgeConfigListRes {configs = map toApiConfig sorted}

postPricingSurgeCreate :: ShortId DM.Merchant -> Context.City -> Common.PricingSurgeConfigReq -> Flow Common.PricingSurgeConfigRes
postPricingSurgeCreate merchantShortId opCity req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  validateConfigReq req
  createdBy <- fromMaybeM (InvalidRequest "createdBy missing (must be set by the dashboard proxy)") req.createdBy
  siblings <- QSC.findAllByCityAndServiceTier merchantOpCity.id req.vehicleServiceTier
  let version = 1 + foldr (max . (.version)) 0 siblings
  newId <- generateGUID
  now <- getCurrentTime
  QSC.create
    DSC.SurgeConfig
      { id = newId,
        merchantId = merchant.id,
        merchantOperatingCityId = merchantOpCity.id,
        vehicleServiceTier = req.vehicleServiceTier,
        timeBounds = req.timeBounds,
        version,
        status = DSC.DRAFT,
        rows = map fromApiRow req.rows,
        minMultiplier = req.minMultiplier,
        maxMultiplier = req.maxMultiplier,
        maxDeltaPerUpdate = req.maxDeltaPerUpdate,
        applyOnExtraDistanceOnly = req.applyOnExtraDistanceOnly,
        excludedAreas = req.excludedAreas,
        description = req.description,
        createdBy,
        createdAt = now,
        updatedAt = now
      }
  pure Common.PricingSurgeConfigRes {surgeConfigId = cast newId, version, status = Common.DRAFT}

postPricingSurgeUpdate :: ShortId DM.Merchant -> Context.City -> Id DCommon.SurgeConfig -> Common.PricingSurgeConfigReq -> Flow APISuccess
postPricingSurgeUpdate merchantShortId opCity reqConfigId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  config <- findScopedConfig merchantOpCity (cast reqConfigId)
  unless (config.status `elem` [DSC.DRAFT, DSC.SHADOW]) $
    throwError (InvalidRequest "only DRAFT or SHADOW configs are editable; create a new version instead")
  validateConfigReq req
  now <- getCurrentTime
  QSC.updateByPrimaryKey
    config
      { DSC.rows = map fromApiRow req.rows,
        DSC.timeBounds = req.timeBounds,
        DSC.minMultiplier = req.minMultiplier,
        DSC.maxMultiplier = req.maxMultiplier,
        DSC.maxDeltaPerUpdate = req.maxDeltaPerUpdate,
        DSC.applyOnExtraDistanceOnly = req.applyOnExtraDistanceOnly,
        DSC.excludedAreas = req.excludedAreas,
        DSC.description = req.description,
        DSC.updatedAt = now
      }
  CQSC.clearCache merchantOpCity.id config.vehicleServiceTier
  pure Success

postPricingSurgeStatus :: ShortId DM.Merchant -> Context.City -> Id DCommon.SurgeConfig -> Common.PricingSurgeStatusReq -> Flow APISuccess
postPricingSurgeStatus merchantShortId opCity reqConfigId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  scoped <- findScopedConfig merchantOpCity (cast reqConfigId)
  -- serialize status transitions per (city, tier): concurrent activations could
  -- otherwise interleave the sibling scan and the two status writes and leave
  -- two ACTIVE configs for the same window
  Redis.withWaitOnLockRedisWithExpiry (surgeStatusLockKey merchantOpCity.id scoped.vehicleServiceTier) 10 60 $ do
    -- re-read inside the lock: a concurrent transition may have already moved it
    config <- findScopedConfig merchantOpCity (cast reqConfigId)
    let newStatus = fromApiStatus req.status
    when (config.status == newStatus) $ throwError (InvalidRequest "config is already in the requested status")
    when (newStatus == DSC.ACTIVE) $ do
      siblings <- QSC.findAllByCityAndServiceTier merchantOpCity.id config.vehicleServiceTier
      let previousActive = find (\c -> c.status == DSC.ACTIVE && c.timeBounds == config.timeBounds && c.id /= config.id) siblings
      -- guardrail: reject a multiplier jump beyond maxDeltaPerUpdate vs the
      -- outgoing version; a config that omits it inherits the outgoing one's,
      -- so dropping the field cannot be used to skip the check
      whenJust previousActive $ \prev -> do
        whenJust (config.maxDeltaPerUpdate <|> prev.maxDeltaPerUpdate) $ \maxDelta -> do
          let jump = abs (maxRowMultiplier config - maxRowMultiplier prev)
          when (jump > maxDelta) $
            throwError (InvalidRequest $ "activation rejected: max multiplier jump " <> show jump <> " exceeds maxDeltaPerUpdate " <> show maxDelta <> " vs version " <> show prev.version)
        QSC.updateStatusById DSC.ARCHIVED prev.id
    QSC.updateStatusById newStatus config.id
    CQSC.clearCache merchantOpCity.id config.vehicleServiceTier
    logInfo $ "SURGE_STATUS_CHANGE: config " <> config.id.getId <> " v" <> show config.version <> " -> " <> show newStatus <> " (city " <> merchantOpCity.id.getId <> ", tier " <> show config.vehicleServiceTier <> ")"
  pure Success

surgeStatusLockKey :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> Text
surgeStatusLockKey cityId tier = "SurgeConfig:Status:CityId-" <> cityId.getId <> ":Tier-" <> show tier

postPricingSurgePreview :: ShortId DM.Merchant -> Context.City -> Common.PricingSurgePreviewReq -> Flow Common.PricingSurgePreviewRes
postPricingSurgePreview merchantShortId opCity req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  (rows, minMultiplier, maxMultiplier) <- case (req.surgeConfigId, req.rows) of
    (Just configId, _) -> do
      config <- findScopedConfig merchantOpCity (cast configId)
      pure (config.rows, config.minMultiplier, config.maxMultiplier)
    (Nothing, Just apiRows) -> pure (map fromApiRow apiRows, fromMaybe 0.1 req.minMultiplier, fromMaybe 10 req.maxMultiplier)
    (Nothing, Nothing) -> throwError (InvalidRequest "either surgeConfigId or inline rows are required")
  let signals = SSC.SurgeSignals {qar = req.signals.qar, supplyDemandRatio = req.signals.supplyDemandRatio, distanceKm = req.signals.distanceKm, area = Nothing}
      mbMatch = find (SSC.rowMatches signals . snd) (zip [0 :: Int ..] rows)
      clamp m = max minMultiplier (min maxMultiplier m)
  pure
    Common.PricingSurgePreviewRes
      { matchedRowIndex = fst <$> mbMatch,
        congestionMultiplier = clamp <$> (mbMatch >>= (.congestionMultiplier) . snd),
        congestionPerMinCharge = mbMatch >>= (.congestionPerMinCharge) . snd
      }

findScopedConfig :: DMOC.MerchantOperatingCity -> Id DSC.SurgeConfig -> Flow DSC.SurgeConfig
findScopedConfig merchantOpCity configId = do
  config <- QSC.findByPrimaryKey configId >>= fromMaybeM (InvalidRequest $ "Surge config not found: " <> configId.getId)
  unless (config.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Surge config belongs to a different operating city")
  pure config

validateConfigReq :: Common.PricingSurgeConfigReq -> Flow ()
validateConfigReq req = do
  when (null req.rows) $ throwError (InvalidRequest "at least one surge row is required")
  when (req.minMultiplier > req.maxMultiplier) $ throwError (InvalidRequest "minMultiplier must not exceed maxMultiplier")
  when (req.minMultiplier <= 0) $ throwError (InvalidRequest "minMultiplier must be positive")
  forM_ (zip [0 :: Int ..] req.rows) $ \(idx, row) -> do
    let rowErr msg = throwError (InvalidRequest $ "row " <> show idx <> ": " <> msg)
    when (isNothing row.congestionMultiplier && isNothing row.congestionPerMinCharge) $
      rowErr "at least one of congestionMultiplier / congestionPerMinCharge is required"
    -- reject rather than silently clamp at evaluation: an out-of-range row is
    -- an operator mistake, not an intent the guardrails should quietly rewrite
    whenJust row.congestionMultiplier $ \m -> do
      when (m < req.minMultiplier) $ rowErr $ "congestionMultiplier " <> show m <> " is below minMultiplier " <> show req.minMultiplier
      when (m > req.maxMultiplier) $ rowErr $ "congestionMultiplier " <> show m <> " exceeds maxMultiplier " <> show req.maxMultiplier
    whenJust ((,) <$> row.qarMin <*> row.qarMax) $ \(lo, hi) -> when (lo >= hi) $ rowErr "qarMin must be below qarMax"
    whenJust ((,) <$> row.supplyDemandRatioMin <*> row.supplyDemandRatioMax) $ \(lo, hi) -> when (lo >= hi) $ rowErr "supplyDemandRatioMin must be below supplyDemandRatioMax"
    whenJust ((,) <$> row.distanceKmMin <*> row.distanceKmMax) $ \(lo, hi) -> when (lo >= hi) $ rowErr "distanceKmMin must be below distanceKmMax"

maxRowMultiplier :: DSC.SurgeConfig -> Centesimal
maxRowMultiplier config = foldr (max . fromMaybe 1 . (.congestionMultiplier)) 1 config.rows

--------------------------------------------------------------------------------
-- API <-> domain mapping
--------------------------------------------------------------------------------

toApiConfig :: DSC.SurgeConfig -> Common.PricingSurgeConfig
toApiConfig config =
  Common.PricingSurgeConfig
    { surgeConfigId = cast config.id,
      vehicleServiceTier = config.vehicleServiceTier,
      timeBounds = config.timeBounds,
      version = config.version,
      status = toApiStatus config.status,
      rows = map toApiRow config.rows,
      minMultiplier = config.minMultiplier,
      maxMultiplier = config.maxMultiplier,
      maxDeltaPerUpdate = config.maxDeltaPerUpdate,
      applyOnExtraDistanceOnly = config.applyOnExtraDistanceOnly,
      excludedAreas = config.excludedAreas,
      description = config.description,
      createdBy = config.createdBy,
      createdAt = config.createdAt
    }

toApiRow :: DSCE.SurgeRow -> Common.PricingSurgeRow
toApiRow r =
  Common.PricingSurgeRow
    { qarMin = r.qarMin,
      qarMax = r.qarMax,
      supplyDemandRatioMin = r.supplyDemandRatioMin,
      supplyDemandRatioMax = r.supplyDemandRatioMax,
      distanceKmMin = r.distanceKmMin,
      distanceKmMax = r.distanceKmMax,
      congestionMultiplier = r.congestionMultiplier,
      congestionPerMinCharge = r.congestionPerMinCharge
    }

fromApiRow :: Common.PricingSurgeRow -> DSCE.SurgeRow
fromApiRow r =
  DSCE.SurgeRow
    { qarMin = r.qarMin,
      qarMax = r.qarMax,
      supplyDemandRatioMin = r.supplyDemandRatioMin,
      supplyDemandRatioMax = r.supplyDemandRatioMax,
      distanceKmMin = r.distanceKmMin,
      distanceKmMax = r.distanceKmMax,
      congestionMultiplier = r.congestionMultiplier,
      congestionPerMinCharge = r.congestionPerMinCharge
    }

toApiStatus :: DSC.SurgeConfigStatus -> Common.PricingSurgeStatus
toApiStatus = \case
  DSC.DRAFT -> Common.DRAFT
  DSC.SHADOW -> Common.SHADOW
  DSC.ACTIVE -> Common.ACTIVE
  DSC.ARCHIVED -> Common.ARCHIVED

fromApiStatus :: Common.PricingSurgeStatus -> DSC.SurgeConfigStatus
fromApiStatus = \case
  Common.DRAFT -> DSC.DRAFT
  Common.SHADOW -> DSC.SHADOW
  Common.ACTIVE -> DSC.ACTIVE
  Common.ARCHIVED -> DSC.ARCHIVED

--------------------------------------------------------------------------------
-- observability
--------------------------------------------------------------------------------

getPricingObservabilityEstimate :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.PricingEstimateExplainRes
getPricingObservabilityEstimate merchantShortId opCity estimateId = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  estimate <- QEstimate.findById (Id estimateId) >>= fromMaybeM (InvalidRequest $ "Estimate not found: " <> estimateId)
  whenJust estimate.merchantOperatingCityId $ \cityId ->
    unless (cityId == merchantOpCity.id) $ throwError (InvalidRequest "Estimate belongs to a different operating city")
  pure
    Common.PricingEstimateExplainRes
      { estimateId = estimateId,
        createdAt = estimate.createdAt,
        vehicleServiceTier = estimate.vehicleServiceTier,
        tripCategory = estimate.tripCategory,
        minFare = estimate.minFare,
        maxFare = estimate.maxFare,
        engine = deriveEngine estimate.dpVersion,
        dpVersion = estimate.dpVersion,
        congestionMultiplier = estimate.congestionMultiplier,
        supplyDemandRatioFromLoc = estimate.supplyDemandRatioFromLoc,
        supplyDemandRatioToLoc = estimate.supplyDemandRatioToLoc,
        fromLocGeohash = estimate.fromLocGeohash,
        smartTipSuggestion = estimate.smartTipSuggestion,
        smartTipReason = estimate.smartTipReason,
        shadowSurgeMultiplier = estimate.shadowSurgeMultiplier,
        shadowSurgeVersion = estimate.shadowSurgeVersion
      }

deriveEngine :: Maybe Text -> Text
deriveEngine = \case
  Nothing -> "NoDecision"
  Just v
    | "SurgeConfig" `T.isPrefixOf` v -> "SurgeConfig"
    | v == "Static" -> "Static"
    | otherwise -> "JsonLogic"

getPricingObservabilityHealth :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Flow Common.PricingHealthRes
getPricingObservabilityHealth merchantShortId opCity mbHours = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  let hours = min 72 (max 1 (fromMaybe 24 mbHours))
  now <- getCurrentTime
  let from = addUTCTime (fromIntegral (negate (hours * 3600)) :: NominalDiffTime) now
  tierStats <- CHEst.pricingStatsByTier merchantOpCity.id from now
  decidedCounts <- CHEst.pricingDecidedCountByTier merchantOpCity.id from now
  surgedCounts <- CHEst.pricingSurgedCountByTier merchantOpCity.id from now
  engineStats <- CHEst.pricingStatsByEngine merchantOpCity.id from now
  geohashStats <- CHEst.pricingStatsByGeohash merchantOpCity.id from now
  shadowStats <- CHEst.pricingShadowComparisonByTier merchantOpCity.id from now
  let tiers =
        [ Common.PricingTierHealth
            { serviceTier = tier,
              totalEstimates = total,
              decidedEstimates = fromMaybe 0 (lookup tier decidedCounts),
              surgedEstimates = fromMaybe 0 (lookup tier surgedCounts),
              avgMultiplier = avgM
            }
          | (tier, total, avgM) <- tierStats
        ]
      engines =
        [ Common.PricingEngineShare {engine = fromMaybe "NoDecision" mbV, estimates = total, avgMultiplier = avgM}
          | (mbV, total, avgM) <- engineStats
        ]
      topGeohashes =
        take 25 $
          sortOn (Down . (.estimates)) $
            [ Common.PricingGeohashStat {geohash = gh, estimates = total, avgMultiplier = avgM}
              | (Just gh, total, avgM) <- geohashStats
            ]
      shadowComparison =
        [ Common.PricingShadowComparison
            { serviceTier = tier,
              shadowVersion = mbVersion,
              estimatesWithShadow = total,
              avgShadowMultiplier = avgShadow,
              avgAppliedMultiplier = avgApplied
            }
          | (tier, mbVersion, total, avgShadow, avgApplied) <- shadowStats
        ]
  pure Common.PricingHealthRes {windowHours = hours, tiers, engines, topGeohashes, shadowComparison}
