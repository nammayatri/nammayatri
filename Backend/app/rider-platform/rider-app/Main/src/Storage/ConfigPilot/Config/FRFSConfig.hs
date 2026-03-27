{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.FRFSConfig
  ( FRFSConfigDimensions (..),
  )
where

import qualified Domain.Types.FRFSConfig as DFRFS
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.FRFSConfig as SCFRFS
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data FRFSConfigDimensions = FRFSConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'FRFSConfig where
  type DimensionsFor 'FRFSConfig = FRFSConfigDimensions
  configTypeValue = FRFSConfig
  sConfigType = SFRFSConfig

instance ConfigDimensions FRFSConfigDimensions where
  type ConfigTypeOf FRFSConfigDimensions = 'FRFSConfig
  type ConfigValueTypeOf FRFSConfigDimensions = Maybe DFRFS.FRFSConfig
  getConfigType _ = FRFSConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    cfg <- IM.withInMemCache (configPilotInMemKey FRFSConfig mocId) 3600 $ SCFRFS.findByMerchantOperatingCityId (Id mocId) (Just [])
    let configWrapper = LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG FRFSConfig) (Id mocId)
