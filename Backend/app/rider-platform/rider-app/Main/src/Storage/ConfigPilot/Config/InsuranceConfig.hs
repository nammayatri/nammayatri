{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.InsuranceConfig
  ( InsuranceConfigDimensions (..),
  )
where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.InsuranceConfig as DIC
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import qualified Storage.Queries.InsuranceConfig as QIC

data InsuranceConfigDimensions = InsuranceConfigDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Text,
    tripCategory :: DTC.TripCategory,
    vehicleCategory :: DVC.VehicleCategory
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'InsuranceConfig where
  type DimensionsFor 'InsuranceConfig = InsuranceConfigDimensions
  configTypeValue = InsuranceConfig
  sConfigType = SInsuranceConfig

instance ConfigDimensions InsuranceConfigDimensions where
  type ConfigTypeOf InsuranceConfigDimensions = 'InsuranceConfig
  type ConfigValueTypeOf InsuranceConfigDimensions = Maybe DIC.InsuranceConfig
  getConfigType _ = InsuranceConfig
  getConfigList a = do
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfg <- QIC.findByMerchantIdAndMerchantOperatingCityIdAndTripCategoryAndVehicleCategory (Id a.merchantId) (Id a.merchantOperatingCityId) a.tripCategory a.vehicleCategory
      case cfg of
        Nothing -> pure Nothing
        Just c -> do
          let configWrapper = LYT.Config {config = c, extraDimensions = Nothing, identifier = 0}
          Just <$> getConfigImpl a configWrapper (LYT.RIDER_CONFIG InsuranceConfig) (Id a.merchantOperatingCityId)
