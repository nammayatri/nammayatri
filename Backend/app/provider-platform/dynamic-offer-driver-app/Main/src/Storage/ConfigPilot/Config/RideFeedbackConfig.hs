{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.RideFeedbackConfig (RideFeedbackConfigDimensions (..)) where

import qualified Domain.Types.RideFeedbackConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
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
