module Storage.CachedQueries.SystemConfigs.LeanFlow
  ( findLeanFlowConfig,
    isFeatureExcluded,
    clearCache,
  )
where

import Domain.Types.Extra.LeanFlow
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()

data RiderLeanFlowOption = RiderLeanFlowOption
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity RiderLeanFlowOption LeanFlowConfig

data RiderLeanFlowLastUpdatedTime = RiderLeanFlowLastUpdatedTime
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity RiderLeanFlowLastUpdatedTime UTCTime

leanFlowConfigId :: Text
leanFlowConfigId = "lean_flow"

leanFlowCacheTtl :: NominalDiffTime
leanFlowCacheTtl = 60

findLeanFlowConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m (Maybe LeanFlowConfig)
findLeanFlowConfig = do
  now <- getCurrentTime
  mbLastUpdated <- L.getOption RiderLeanFlowLastUpdatedTime
  let isStale = maybe True (\lastUpdated -> diffUTCTime now lastUpdated > leanFlowCacheTtl) mbLastUpdated
  if isStale
    then do
      mbConfig <- decodeFromText' @LeanFlowConfig <$> KSQS.findById leanFlowConfigId
      L.setOption RiderLeanFlowLastUpdatedTime now
      whenJust mbConfig $ \config -> L.setOption RiderLeanFlowOption config
      case mbConfig of
        Just config -> pure (Just config)
        Nothing -> L.getOption RiderLeanFlowOption
    else L.getOption RiderLeanFlowOption

isFeatureExcluded :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LeanFlowFeature -> m Bool
isFeatureExcluded feature =
  maybe False (\config -> config.enabled && feature `elem` config.featuresExcluded) <$> findLeanFlowConfig

clearCache :: MonadFlow m => m ()
clearCache = do
  now <- getCurrentTime
  L.setOption RiderLeanFlowLastUpdatedTime (addUTCTime (-1000000000) now)
