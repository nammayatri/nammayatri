{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.BecknConfig
  ( BecknConfigDimensions (..),
    filterByDomain,
    filterByDomainAndVehicleWithFallback,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.Queries.BecknConfig as SQBC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

-- | Filter BecknConfigs by domain
filterByDomain :: [DBC.BecknConfig] -> Text -> [DBC.BecknConfig]
filterByDomain cfgs domain = filter (\c -> c.domain == domain) cfgs

-- | Find a BecknConfig by domain and vehicle category, with fallback to merchantId-level match
filterByDomainAndVehicleWithFallback :: [DBC.BecknConfig] -> Text -> Enums.VehicleCategory -> Maybe DBC.BecknConfig
filterByDomainAndVehicleWithFallback cfgs domain vehicle =
  find (\c -> c.domain == domain && c.vehicleCategory == vehicle) cfgs

data BecknConfigDimensions = BecknConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'BecknConfig where
  type DimensionsFor 'BecknConfig = BecknConfigDimensions
  configTypeValue = BecknConfig
  sConfigType = SBecknConfig

instance ConfigDimensions BecknConfigDimensions where
  type ConfigTypeOf BecknConfigDimensions = 'BecknConfig
  type ConfigValueTypeOf BecknConfigDimensions = [DBC.BecknConfig]
  getConfigType _ = BecknConfig
  getConfig a = do
    cfgs <- SQBC.findAllByMerchantOperatingCityId (Just (Id (merchantOperatingCityId a)))
    let configWrapper = LYT.Config {config = cfgs, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG BecknConfig) (Id (merchantOperatingCityId a))
