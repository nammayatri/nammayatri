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
    merchantId :: Text,
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
    let mId = a.merchantId
    cfgs <- IM.withInMemCache (configPilotInMemKey BecknConfig mId) 3600 $ SQBC.findByMerchantId (Just (Id mId))
    let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
    mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG BecknConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs =
    let foundCfg = filter matchesDimsMocId cfgs
     in if null foundCfg
          then maybeToList . listToMaybe $ filter matchesDims cfgs
          else foundCfg
    where
      matchesDimsMocId c =
        matchesDims c
          && c.merchantOperatingCityId == Just (Id dims.merchantOperatingCityId)
      matchesDims c =
        maybe True (\d -> c.domain == d) dims.domain
          && maybe True (\vc -> c.vehicleCategory == vc) dims.vehicleCategory