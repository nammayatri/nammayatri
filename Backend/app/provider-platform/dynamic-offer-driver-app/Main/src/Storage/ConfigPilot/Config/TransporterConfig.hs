{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.TransporterConfig
  ( TransporterDimensions (..),
  )
where

import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QCTC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data TransporterDimensions = TransporterDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'TransporterConfig where
  type DimensionsFor 'TransporterConfig = TransporterDimensions
  configTypeValue = TransporterConfig
  sConfigType = STransporterConfig

instance ConfigDimensions TransporterDimensions where
  type ConfigTypeOf TransporterDimensions = 'TransporterConfig
  type ConfigValueTypeOf TransporterDimensions = Maybe DTC.TransporterConfig
  getConfigType _ = TransporterConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfg <- QCTC.getTransporterConfigFromDB (Id mocId)
      case cfg of
        Nothing -> pure Nothing
        Just c -> do
          let configWrapper = LYT.Config {config = c, extraDimensions = Nothing, identifier = 0}
          Just <$> getConfigImpl a configWrapper (LYT.DRIVER_CONFIG TransporterConfig) (Id mocId)
