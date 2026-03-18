{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.BecknConfig
  ( BecknConfigDimensions (..),
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import qualified Storage.Queries.BecknConfig as SQBC

data BecknConfigDimensions = BecknConfigDimensions
  { merchantOperatingCityId :: Text,
    domain :: Maybe Text,
    vehicleCategory :: Maybe Enums.VehicleCategory
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'BecknConfig where
  type DimensionsFor 'BecknConfig = BecknConfigDimensions
  configTypeValue = BecknConfig
  sConfigType = SBecknConfig

instance ConfigDimensions BecknConfigDimensions where
  type ConfigTypeOf BecknConfigDimensions = 'BecknConfig
  type ConfigValueTypeOf BecknConfigDimensions = [DBC.BecknConfig]
  getConfigType _ = BecknConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey BecknConfig mocId) 3600 $ do
      cfgs <- SQBC.findAllByMerchantOperatingCityId (Just (Id mocId))
      let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
      mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG BecknConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDims cfgs
    where
      matchesDims c =
        maybe True (\d -> c.domain == d) dims.domain
          && maybe True (\vc -> c.vehicleCategory == vc) dims.vehicleCategory
