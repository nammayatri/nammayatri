{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.BecknConfig
  ( BecknConfigDimensions (..),
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
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
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG BecknConfig)
      (Id a.merchantOperatingCityId)
      (SQBC.findByMerchantId (Just (Id a.merchantId)))
      [ CR.DimMatcher (\dims -> Just dims.merchantOperatingCityId) (fmap (.getId) . (.merchantOperatingCityId)) (==),
        CR.DimMatcher (.domain) (Just . (.domain)) (==),
        CR.DimMatcher (.vehicleCategory) (Just . (.vehicleCategory)) (==)
      ]
      ( Just
          [ CR.DimMatcher (.domain) (Just . (.domain)) (==),
            CR.DimMatcher (.vehicleCategory) (Just . (.vehicleCategory)) (==)
          ]
      )
