{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Evaluation of the typed surge table (Phase 4 of the fare-policy revamp).
-- ACTIVE config wins over the json-logic path in
-- 'getCongestionChargeMultiplierFromModel''; SHADOW only logs. The config is
-- read through a city+tier Redis cache cleared on every status change, so
-- activation and rollback take effect on the next NEW search; transactions
-- already priced stay pinned to the version that priced them (surgePinKey in
-- SharedLogic.FarePolicy) so end-ride recompute replays the same version.
module SharedLogic.SurgeConfig
  ( SurgeSignals (..),
    SurgeOutcome (..),
    SurgeConfigsForPricing (..),
    evaluateSurgeConfig,
    findConfigsForPricing,
    rowMatches,
  )
where

import Control.Applicative ((<|>))
import qualified Data.List as List
import Domain.Types.Common (ServiceTierType)
import Domain.Types.Extra.SurgeConfig (SurgeRow (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SurgeConfig as DSC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common (CacheFlow)
import qualified Lib.Types.SpecialLocation as SL
import qualified Storage.CachedQueries.SurgeConfig as CQSC

data SurgeSignals = SurgeSignals
  { qar :: Maybe Double,
    supplyDemandRatio :: Maybe Double,
    distanceKm :: Maybe Int,
    -- the fare product's area, matched against the config's excludedAreas
    -- opt-out list
    area :: Maybe SL.Area
  }
  deriving (Show)

data SurgeOutcome = SurgeOutcome
  { multiplier :: Maybe Centesimal, -- already clamped to the config's guardrails
    perMinCharge :: Maybe Double,
    matchedRowIndex :: Int,
    configVersion :: Int
  }
  deriving (Show)

data SurgeConfigsForPricing = SurgeConfigsForPricing
  { activeConfig :: Maybe DSC.SurgeConfig,
    shadowConfig :: Maybe DSC.SurgeConfig
  }

-- | A bound only matches when the signal is PRESENT: missing data never
-- satisfies a bounded row, so a cold Redis key cannot read as scarcity.
rowMatches :: SurgeSignals -> SurgeRow -> Bool
rowMatches signals row =
  boundsOk signals.qar row.qarMin row.qarMax
    && boundsOk signals.supplyDemandRatio row.supplyDemandRatioMin row.supplyDemandRatioMax
    && boundsOk (fromIntegral <$> signals.distanceKm) (fromIntegral <$> row.distanceKmMin) (fromIntegral <$> row.distanceKmMax)
  where
    boundsOk :: Maybe Double -> Maybe Double -> Maybe Double -> Bool
    boundsOk _ Nothing Nothing = True -- unbounded on this signal
    boundsOk Nothing _ _ = False -- bounded but signal missing
    boundsOk (Just v) mbMin mbMax = maybe True (v >=) mbMin && maybe True (v <) mbMax

-- | Rows evaluated top-down, first match wins; Nothing when no row matches
-- (the caller falls back to the fare policy's static multiplier). An area on
-- the config's excludedAreas opt-out list never surges — same static fallback.
evaluateSurgeConfig :: DSC.SurgeConfig -> SurgeSignals -> Maybe SurgeOutcome
evaluateSurgeConfig config signals = do
  case (signals.area, config.excludedAreas) of
    (Just area', Just excluded) | area' `elem` excluded -> Nothing
    _ -> pure ()
  (idx, row) <- List.find (rowMatches signals . snd) (zip [0 ..] config.rows)
  Just
    SurgeOutcome
      { multiplier = clamp <$> row.congestionMultiplier,
        perMinCharge = row.congestionPerMinCharge,
        matchedRowIndex = idx,
        configVersion = config.version
      }
  where
    clamp m = max config.minMultiplier (min config.maxMultiplier m)

-- | The single ACTIVE and SHADOW config governing (city, tier) right now:
-- a time-bounded config matching the local time wins over the Unbounded one.
findConfigsForPricing ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  ServiceTierType ->
  UTCTime ->
  m SurgeConfigsForPricing
findConfigsForPricing merchantOpCityId serviceTier localTime = do
  configs <- CQSC.findAllByCityAndServiceTier merchantOpCityId serviceTier
  let pick status =
        let candidates = filter (\c -> c.status == status) configs
            bounded = DTB.findBoundedDomain (filter (\c -> c.timeBounds /= DTB.Unbounded) candidates) localTime
            unbounded = filter (\c -> c.timeBounds == DTB.Unbounded) candidates
         in listToMaybe bounded <|> listToMaybe unbounded
  pure SurgeConfigsForPricing {activeConfig = pick DSC.ACTIVE, shadowConfig = pick DSC.SHADOW}
