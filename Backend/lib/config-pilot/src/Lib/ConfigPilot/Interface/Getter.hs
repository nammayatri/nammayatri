{-# LANGUAGE ExistentialQuantification #-}

module Lib.ConfigPilot.Interface.Getter
  ( DimMatcher (..),
    resolveConfigList,
    TxnIdKey (..),
    PersonIdKey (..),
    invalidateConfigInMem,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Lib.ConfigPilot.Config.GetterInternal (PersonIdKey (..), TxnIdKey (..), configPilotInMemKey, getConfigImpl, invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (ConfigDimensions (..))
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Types as LYT

-- | Pairs a dims-side extractor with a cfg-side extractor and a custom equality
-- function for optional secondary-key filtering.
--
-- When the dims extractor returns 'Nothing' the filter is skipped (pass-all).
-- When it returns 'Just x', the cfg extractor must return 'Just y' where
-- @eqFn x y@ holds for the record to pass.
--
-- Examples:
--   Non-Maybe cfg field:  @DimMatcher (.callService)    (Just . (.callService)) (==)@
--   Maybe cfg field:      @DimMatcher (.vehicleCategory) (.vehicleCategory)     (==)@
--   Custom equality:      @DimMatcher (.domain) (Just . (.domain)) (\a b -> Text.toLower a == Text.toLower b)@
data DimMatcher dims cfg
  = forall a.
    DimMatcher
      (dims -> Maybe a)
      (cfg -> Maybe a)
      (a -> a -> Bool)

-- | One-stop resolver for list-valued configs keyed by 'MerchantOperatingCity'.
--
-- Handles fetch → filter → wrap → dynamic-logic in one call.
-- Pass @[]@ for @matchers@ when no secondary-key filtering is needed.
--
-- The caller applies the primary key to the fetch action directly
-- (avoids phantom-type mismatch between lib and app @Id@ types):
--
-- > getConfigList a =
-- >   resolveConfigList a (LYT.RIDER_CONFIG MyConfig) (Id a.merchantOperatingCityId)
-- >     (MyQuery.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
-- >     []
resolveConfigList ::
  forall dims cfg m r.
  ( ConfigDimensions dims,
    FromJSON cfg,
    ToJSON cfg,
    Typeable cfg,
    Show cfg,
    BeamFlow.BeamFlow m r
  ) =>
  dims ->
  LYT.LogicDomain ->
  Id LYT.MerchantOperatingCity ->
  m [cfg] ->
  [DimMatcher dims cfg] ->
  -- | Optional fallback matchers: used when primary matchers yield no results.
  Maybe [DimMatcher dims cfg] ->
  m [cfg]
resolveConfigList dims logicDomain mocId fetch matchers fallback =
  -- Two-layer cache: per-pod in-mem (L1) wraps a cross-pod Redis hash (L2).
  -- L2 hash prefix mirrors 'configPilotInMemKey' ("ConfigPilot:<configType>");
  -- the dimensions form the hash field and 'withRedisCache' appends the cached
  -- type name. L2 currently relies on its TTL; explicit invalidation is TODO.
  IM.withInMemCache cacheKey 3600 $
    Hedis.withRedisCache redisHashPrefix [dimensionsCacheKey dims] 1800 $ do
      cfgs <- fetch
      let applyMatchers ms = filter (\c -> all (matchesDim dims c) ms) cfgs
      let filtered = case (applyMatchers matchers, fallback) of
            ([], Just fb) -> applyMatchers fb
            (results, _) -> results
      let wrappers = map (\c -> LYT.Config {config = c, extraDimensions = Nothing, identifier = 0}) filtered
      mapM (\w -> getConfigImpl dims w logicDomain mocId) wrappers
  where
    cacheKey = configPilotInMemKey dims
    redisHashPrefix = "ConfigPilot:" <> show (getConfigType dims)
    matchesDim dims' c (DimMatcher getDimVal getCfgVal eqFn) =
      maybe True (\dv -> maybe False (eqFn dv) (getCfgVal c)) (getDimVal dims')
