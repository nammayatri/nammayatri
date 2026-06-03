{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.MerchantServiceUsageConfig
  ( MerchantServiceUsageConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC

data MerchantServiceUsageConfigDimensions = MerchantServiceUsageConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantServiceUsageConfig where
  type DimensionsFor 'MerchantServiceUsageConfig = MerchantServiceUsageConfigDimensions
  configTypeValue = MerchantServiceUsageConfig
  sConfigType = SMerchantServiceUsageConfig

instance ConfigDimensions MerchantServiceUsageConfigDimensions where
  type ConfigTypeOf MerchantServiceUsageConfigDimensions = 'MerchantServiceUsageConfig
  type ConfigValueTypeOf MerchantServiceUsageConfigDimensions = Maybe DMSUC.MerchantServiceUsageConfig
  getConfigType _ = MerchantServiceUsageConfig
  getConfigList a =
    listToMaybe
      <$> CR.resolveConfigList
        a
        (LYT.RIDER_CONFIG MerchantServiceUsageConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> CQMSUC.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
        ([] :: [CR.DimMatcher MerchantServiceUsageConfigDimensions DMSUC.MerchantServiceUsageConfig])
        Nothing
