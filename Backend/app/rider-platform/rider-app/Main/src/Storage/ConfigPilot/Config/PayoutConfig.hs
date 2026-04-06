{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.PayoutConfig
  ( PayoutDimensions (..),
  )
where

import qualified Domain.Types.PayoutConfig as DPC
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data PayoutDimensions = PayoutDimensions
  { merchantOperatingCityId :: Text,
    vehicleCategory :: Maybe VehicleCategory,
    isPayoutEnabled :: Maybe Bool,
    payoutEntity :: Maybe DPC.PayoutEntity
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'PayoutConfig where
  type DimensionsFor 'PayoutConfig = PayoutDimensions
  configTypeValue = PayoutConfig
  sConfigType = SPayoutConfig

instance ConfigDimensions PayoutDimensions where
  type ConfigTypeOf PayoutDimensions = 'PayoutConfig
  type ConfigValueTypeOf PayoutDimensions = [DPC.PayoutConfig]
  getConfigType _ = PayoutConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfgs <- CPC.findAllByMerchantOpCityId (Id mocId) (Just [])
      let filtered = filterByDimensions a cfgs
      let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) filtered
      mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG PayoutConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDims cfgs
    where
      matchesDims c =
        maybe True (\vc -> c.vehicleCategory == Just vc) dims.vehicleCategory
          && maybe True (\en -> c.isPayoutEnabled == en) dims.isPayoutEnabled
          && maybe True (\pe -> c.payoutEntity == pe) dims.payoutEntity
