{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.DocumentVerificationStagesConfig (DocumentVerificationStagesConfigDimensions (..)) where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationStagesConfig as DT
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.DocumentVerificationStagesConfig as SQ

-- documentOnboardingStage is deliberately NOT a dimension: callers want the whole ordered
-- stage list for a city and filter it themselves, the way filterByStage already does for docs.
data DocumentVerificationStagesConfigDimensions = DocumentVerificationStagesConfigDimensions
  { merchantOperatingCityId :: Text,
    vehicleCategory :: Maybe Domain.Types.VehicleCategory.VehicleCategory,
    applicableTo :: Maybe Domain.Types.DocumentVerificationConfig.DocumentApplicableType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'DocumentVerificationStagesConfig where
  type DimensionsFor 'DocumentVerificationStagesConfig = DocumentVerificationStagesConfigDimensions
  configTypeValue = DocumentVerificationStagesConfig
  sConfigType = SDocumentVerificationStagesConfig

instance ConfigDimensions DocumentVerificationStagesConfigDimensions where
  type ConfigTypeOf DocumentVerificationStagesConfigDimensions = 'DocumentVerificationStagesConfig
  type ConfigValueTypeOf DocumentVerificationStagesConfigDimensions = [DT.DocumentVerificationStagesConfig]
  getConfigType _ = DocumentVerificationStagesConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG DocumentVerificationStagesConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      [ LCP.DimMatcher (.vehicleCategory) (Just . (.vehicleCategory)) (==),
        LCP.DimMatcher (.applicableTo) (Just . (.applicableTo)) (==)
      ]
      Nothing
  configFallback _ = Nothing
