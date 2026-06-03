{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.PayoutConfig
  ( PayoutDimensions (..),
  )
where

import qualified Domain.Types.PayoutConfig as DPC
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC

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
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG PayoutConfig)
      (Id a.merchantOperatingCityId)
      (CPC.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      [ CR.DimMatcher (.vehicleCategory) (.vehicleCategory) (==),
        CR.DimMatcher (.isPayoutEnabled) (Just . (.isPayoutEnabled)) (==),
        CR.DimMatcher (.payoutEntity) (Just . (.payoutEntity)) (==)
      ]
      Nothing
