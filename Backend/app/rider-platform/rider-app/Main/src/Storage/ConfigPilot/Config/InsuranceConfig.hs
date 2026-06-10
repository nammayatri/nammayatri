{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.InsuranceConfig
  ( InsuranceConfigDimensions (..),
  )
where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.InsuranceConfig as DIC
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
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
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG InsuranceConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> QIC.findByMerchantIdAndMerchantOperatingCityIdAndTripCategoryAndVehicleCategory (Id a.merchantId) (Id a.merchantOperatingCityId) a.tripCategory a.vehicleCategory)
        ([] :: [LCP.DimMatcher InsuranceConfigDimensions DIC.InsuranceConfig])
        Nothing
