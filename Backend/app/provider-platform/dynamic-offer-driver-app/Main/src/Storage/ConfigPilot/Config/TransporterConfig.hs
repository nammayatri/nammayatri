{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..)) where

import qualified Domain.Types.TransporterConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.TransporterConfig as QTransporterConfig

data TransporterConfigDimensions = TransporterConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'TransporterConfig where
  type DimensionsFor 'TransporterConfig = TransporterConfigDimensions
  configTypeValue = TransporterConfig
  sConfigType = STransporterConfig

instance ConfigDimensions TransporterConfigDimensions where
  type ConfigTypeOf TransporterConfigDimensions = 'TransporterConfig
  type ConfigValueTypeOf TransporterConfigDimensions = Maybe DT.TransporterConfig
  getConfigType _ = TransporterConfig
  configDBFetch a = Just (QTransporterConfig.findByMerchantOpCityId (Id a.merchantOperatingCityId))
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.DRIVER_CONFIG TransporterConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> QTransporterConfig.findByMerchantOpCityId (Id a.merchantOperatingCityId))
        (([] :: [LCP.DimMatcher TransporterConfigDimensions DT.TransporterConfig]))
        Nothing
  configFallback a = Just (QTransporterConfig.findByMerchantOpCityId (Id a.merchantOperatingCityId))
