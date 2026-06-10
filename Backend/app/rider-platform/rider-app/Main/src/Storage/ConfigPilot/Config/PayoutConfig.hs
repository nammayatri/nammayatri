{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..)) where

import qualified Domain.Types.PayoutConfig
import qualified Domain.Types.PayoutConfig as DT
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SQ

data PayoutConfigDimensions = PayoutConfigDimensions
  { merchantOperatingCityId :: Text,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    isPayoutEnabled :: Maybe Kernel.Prelude.Bool,
    payoutEntity :: Maybe Domain.Types.PayoutConfig.PayoutEntity
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'PayoutConfigRider where
  type DimensionsFor 'PayoutConfigRider = PayoutConfigDimensions
  configTypeValue = PayoutConfigRider
  sConfigType = SPayoutConfigRider

instance ConfigDimensions PayoutConfigDimensions where
  type ConfigTypeOf PayoutConfigDimensions = 'PayoutConfigRider
  type ConfigValueTypeOf PayoutConfigDimensions = [DT.PayoutConfig]
  getConfigType _ = PayoutConfigRider
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG PayoutConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      [ LCP.DimMatcher (.vehicleCategory) (.vehicleCategory) (==),
        LCP.DimMatcher (.isPayoutEnabled) (Just . (.isPayoutEnabled)) (==),
        LCP.DimMatcher (.payoutEntity) (Just . (.payoutEntity)) (==)
      ]
      Nothing
