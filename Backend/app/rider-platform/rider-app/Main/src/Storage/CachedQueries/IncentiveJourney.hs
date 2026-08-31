{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourney
  ( findById,
    findByMerchantOperatingCityId,
    findEnabledByMerchantOperatingCityId,
    findEnabledByMerchantIdAndMerchantOperatingCityId,
    clearCache,
    clearCacheByMerchantOperatingCityId,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as LibCQ
import Storage.Beam.IncentiveJourney ()

actor :: IJ.JourneyActor
actor = IJ.RiderActor

findById :: (BeamFlow m r) => Id DIJ.IncentiveJourney -> m (Maybe DIJ.IncentiveJourney)
findById = LibCQ.findById actor

findByMerchantOperatingCityId ::
  (BeamFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findByMerchantOperatingCityId merchantOpCityId =
  LibCQ.findByMerchantOperatingCityId actor (cast merchantOpCityId)

findEnabledByMerchantOperatingCityId ::
  (BeamFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findEnabledByMerchantOperatingCityId merchantOpCityId =
  LibCQ.findEnabledByMerchantOperatingCityId actor (cast merchantOpCityId)

findEnabledByMerchantIdAndMerchantOperatingCityId ::
  (BeamFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findEnabledByMerchantIdAndMerchantOperatingCityId merchantId merchantOpCityId =
  LibCQ.findEnabledByMerchantIdAndMerchantOperatingCityId actor (cast merchantId) (cast merchantOpCityId)

clearCache :: (CacheFlow m r) => DIJ.IncentiveJourney -> m ()
clearCache = LibCQ.clearCache actor

clearCacheByMerchantOperatingCityId :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
clearCacheByMerchantOperatingCityId merchantOpCityId =
  LibCQ.clearCacheByMerchantOperatingCityId actor (cast merchantOpCityId)
