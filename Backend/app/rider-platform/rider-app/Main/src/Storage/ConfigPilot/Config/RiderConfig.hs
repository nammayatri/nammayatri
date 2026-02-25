{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.RiderConfig
  ( RiderDimensions (..),
  )
where

import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data RiderDimensions = RiderDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'RiderConfig where
  type DimensionsFor 'RiderConfig = RiderDimensions
  configTypeValue = RiderConfig
  sConfigType = SRiderConfig

instance ConfigDimensions RiderDimensions where
  type ConfigTypeOf RiderDimensions = 'RiderConfig
  type ConfigValueTypeOf RiderDimensions = Maybe DRC.RiderConfig
  getConfigType _ = RiderConfig
  getConfig a = do
    cfg <- QRC.findByMerchantOperatingCityId (Id (merchantOperatingCityId a))
    let configWrapper = LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG RiderConfig) (Id (merchantOperatingCityId a))
