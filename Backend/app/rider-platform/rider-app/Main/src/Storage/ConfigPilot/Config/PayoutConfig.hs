{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.PayoutConfig
  ( PayoutDimensions (..),
    filterByCityIdAndVehicleCategory,
    filterByPayoutEnabledAndEntity,
  )
where

import qualified Domain.Types.PayoutConfig as DPC
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data PayoutDimensions = PayoutDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Text,
    txnId :: Maybe Text,
    payoutType :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'PayoutConfig where
  type DimensionsFor 'PayoutConfig = PayoutDimensions
  configTypeValue = PayoutConfig
  sConfigType = SPayoutConfig

instance ConfigDimensions PayoutDimensions where
  type ConfigTypeOf PayoutDimensions = 'PayoutConfig
  type ConfigValueTypeOf PayoutDimensions = [DPC.PayoutConfig]
  getConfigType _ = PayoutConfig
  getConfig a = do
    cfgs <- CPC.findAllByMerchantOpCityId (Id (merchantOperatingCityId a)) (Just [])
    let configWrapper = LYT.Config {config = cfgs, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG PayoutConfig) (Id (merchantOperatingCityId a))

filterByCityIdAndVehicleCategory :: [DPC.PayoutConfig] -> VehicleCategory -> Maybe DPC.PayoutEntity -> Maybe DPC.PayoutConfig
filterByCityIdAndVehicleCategory cfgs vehicleCategory mbEntity =
  find
    ( \c ->
        c.vehicleCategory == Just vehicleCategory
          && maybe True (\entity -> c.payoutEntity == entity) mbEntity
    )
    cfgs

filterByPayoutEnabledAndEntity :: [DPC.PayoutConfig] -> Bool -> DPC.PayoutEntity -> Maybe DPC.PayoutConfig
filterByPayoutEnabledAndEntity cfgs isPayoutEnabled payoutEntity =
  find (\c -> c.isPayoutEnabled == isPayoutEnabled && c.payoutEntity == payoutEntity) cfgs
