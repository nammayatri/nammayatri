{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.RideFeedbackConfig
  ( RideFeedbackConfigDimensions (..),
    findAllServedUncached,
  )
where

import qualified Domain.Types.RideFeedbackConfig as DT
import Kernel.Beam.Functions (runInMasterDbAndRedis)
import Kernel.Prelude
import Kernel.Types.Id
import Lib.ConfigPilot.Config.GetterInternal (getConfigImpl, selectActiveElementVersions)
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.RideFeedbackConfig as Q

-- | During-ride feedback questions of a city. `enabled` and `questionKey` are applied after the
-- Config Pilot patches (in 'filterByDimensions'), not as matchers on the stored rows, so a staged
-- change that enables or disables a question takes effect for the rides it is rolled out to.
data RideFeedbackConfigDimensions = RideFeedbackConfigDimensions
  { merchantOperatingCityId :: Text,
    enabled :: Maybe Bool,
    questionKey :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'RideFeedbackConfig where
  type DimensionsFor 'RideFeedbackConfig = RideFeedbackConfigDimensions
  configTypeValue = RideFeedbackConfig
  sConfigType = SRideFeedbackConfig

instance ConfigDimensions RideFeedbackConfigDimensions where
  type ConfigTypeOf RideFeedbackConfigDimensions = 'RideFeedbackConfig
  type ConfigValueTypeOf RideFeedbackConfigDimensions = [DT.RideFeedbackConfig]
  getConfigType _ = RideFeedbackConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG RideFeedbackConfig)
      (Id a.merchantOperatingCityId)
      (Q.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      []
      Nothing
  filterByDimensions = filter . matches

  -- Questions are read on every in-ride fetch and answer: if Config Pilot fails, serve the stored rows.
  configFallback a = Just $ filter (matches a) <$> Q.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId)

matches :: RideFeedbackConfigDimensions -> DT.RideFeedbackConfig -> Bool
matches dims config =
  maybe True (== config.enabled) dims.enabled
    && maybe True (== config.questionKey) dims.questionKey

-- | A city's questions as 'getConfigList' serves them (stored rows with the Config Pilot patches),
-- read from the master DB without its in-memory and Redis caches. For the dashboard: a question
-- created on one pod is not in the other pods' in-memory cache until the refresh reaches them, so
-- a cached read right after a create can miss it.
findAllServedUncached :: BeamFlow.BeamFlow m r => Id LYT.MerchantOperatingCity -> m [DT.RideFeedbackConfig]
findAllServedUncached mocId = do
  let domain = LYT.DRIVER_CONFIG RideFeedbackConfig
  versions <- selectActiveElementVersions domain mocId
  rows <- runInMasterDbAndRedis $ Q.findAllByMerchantOperatingCityId (cast mocId)
  mapM (\row -> getConfigImpl versions (LYT.Config row Nothing 0) domain mocId) rows
