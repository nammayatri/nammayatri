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

data DriverLeanFlowOption = DriverLeanFlowOption
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity DriverLeanFlowOption LeanFlowConfig

data DriverLeanFlowLastUpdatedTime = DriverLeanFlowLastUpdatedTime
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity DriverLeanFlowLastUpdatedTime UTCTime

leanFlowConfigId :: Text
leanFlowConfigId = "lean_flow"

leanFlowCacheTtl :: NominalDiffTime
leanFlowCacheTtl = 60

findLeanFlowConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m (Maybe LeanFlowConfig)
findLeanFlowConfig = do
  now <- getCurrentTime
  mbLastUpdated <- L.getOption DriverLeanFlowLastUpdatedTime
  let isStale = maybe True (\lastUpdated -> diffUTCTime now lastUpdated > leanFlowCacheTtl) mbLastUpdated
  if isStale
    then do
      mbConfig <- decodeFromText' @LeanFlowConfig <$> KSQS.findById leanFlowConfigId
      L.setOption DriverLeanFlowLastUpdatedTime now
      whenJust mbConfig $ \config -> L.setOption DriverLeanFlowOption config
      case mbConfig of
        Just config -> pure (Just config)
        Nothing -> L.getOption DriverLeanFlowOption
    else L.getOption DriverLeanFlowOption

isFeatureExcluded :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LeanFlowFeature -> m Bool
isFeatureExcluded feature =
  maybe False (\config -> config.enabled && feature `elem` config.featuresExcluded) <$> findLeanFlowConfig

clearCache :: MonadFlow m => m ()
clearCache = do
  now <- getCurrentTime
  L.setOption DriverLeanFlowLastUpdatedTime (addUTCTime (-1000000000) now)
