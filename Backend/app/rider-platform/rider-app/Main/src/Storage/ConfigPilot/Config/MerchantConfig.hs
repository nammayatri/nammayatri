{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.MerchantConfig
  ( MerchantConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantConfig as DMC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.MerchantConfig as SCMC

data MerchantConfigDimensions = MerchantConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantConfig where
  type DimensionsFor 'MerchantConfig = MerchantConfigDimensions
  configTypeValue = MerchantConfig
  sConfigType = SMerchantConfig

instance ConfigDimensions MerchantConfigDimensions where
  type ConfigTypeOf MerchantConfigDimensions = 'MerchantConfig
  type ConfigValueTypeOf MerchantConfigDimensions = [DMC.MerchantConfig]
  getConfigType _ = MerchantConfig
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG MerchantConfig)
      (Id a.merchantOperatingCityId)
      (SCMC.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
      []
      Nothing
