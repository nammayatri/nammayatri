{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.CoinsConfig (CoinsConfigDimensions (..)) where

import qualified Domain.Types.Coins.CoinsConfig as DT
import qualified Domain.Types.Common as DTC
import Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.CoinsConfig as SQ

data CoinsConfigDimensions = CoinsConfigDimensions
  { merchantOptCityId :: Text,
    eventFunction :: Maybe DCT.DriverCoinsFunctionType,
    merchantId :: Maybe Text,
    active :: Maybe Bool,
    vehicleCategory :: Maybe DTV.VehicleCategory,
    serviceTierType :: Maybe DTC.ServiceTierType,
    eventName :: Maybe Text,
    tripCategoryType :: Maybe DCT.TripCategoryType,
    configId :: Maybe (Id DT.CoinsConfig)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'CoinsConfig where
  type DimensionsFor 'CoinsConfig = CoinsConfigDimensions
  configTypeValue = CoinsConfig
  sConfigType = SCoinsConfig

instance ConfigDimensions CoinsConfigDimensions where
  type ConfigTypeOf CoinsConfigDimensions = 'CoinsConfig
  type ConfigValueTypeOf CoinsConfigDimensions = [DT.CoinsConfig]
  getConfigType _ = CoinsConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG CoinsConfig)
      (Id a.merchantOptCityId)
      (SQ.findAllByMerchantOptCityId (Id a.merchantOptCityId))
      [ LCP.DimMatcher (.eventFunction) (Just . (.eventFunction)) (==),
        LCP.DimMatcher (.merchantId) (Just . (.merchantId)) (==),
        LCP.DimMatcher (.active) (Just . (.active)) (==),
        LCP.DimMatcher (.vehicleCategory) (.vehicleCategory) (==),
        LCP.DimMatcher (.serviceTierType) (.serviceTierType) (==),
        LCP.DimMatcher (.eventName) (Just . (.eventName)) (==),
        LCP.DimMatcher (.tripCategoryType) (.tripCategoryType) (==),
        LCP.DimMatcher (.configId) (Just . (.id)) (==)
      ]
      Nothing
