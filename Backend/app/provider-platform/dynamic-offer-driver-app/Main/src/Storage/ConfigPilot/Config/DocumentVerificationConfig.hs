{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..)) where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationConfig as DT
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.DocumentVerificationConfig as SQ

data DocumentVerificationConfigDimensions = DocumentVerificationConfigDimensions
  { merchantOperatingCityId :: Text,
    documentType :: Maybe Domain.Types.DocumentVerificationConfig.DocumentType,
    vehicleCategory :: Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'DocumentVerificationConfig where
  type DimensionsFor 'DocumentVerificationConfig = DocumentVerificationConfigDimensions
  configTypeValue = DocumentVerificationConfig
  sConfigType = SDocumentVerificationConfig

instance ConfigDimensions DocumentVerificationConfigDimensions where
  type ConfigTypeOf DocumentVerificationConfigDimensions = 'DocumentVerificationConfig
  type ConfigValueTypeOf DocumentVerificationConfigDimensions = [DT.DocumentVerificationConfig]
  getConfigType _ = DocumentVerificationConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG DocumentVerificationConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      [ LCP.DimMatcher (.documentType) (Just . (.documentType)) (==),
        LCP.DimMatcher (.vehicleCategory) (Just . (.vehicleCategory)) (==)
      ]
      Nothing
