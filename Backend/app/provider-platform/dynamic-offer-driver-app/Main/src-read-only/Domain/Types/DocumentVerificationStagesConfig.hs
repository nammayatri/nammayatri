{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DocumentVerificationStagesConfig where

import Data.Aeson
import qualified Domain.Types.DocumentOnboardingStage
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DocumentVerificationStagesConfig = DocumentVerificationStagesConfig
  { applicableTo :: Domain.Types.DocumentVerificationConfig.DocumentApplicableType,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentCategory :: Domain.Types.DocumentVerificationConfig.DocumentCategory,
    documentOnboardingStage :: Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage,
    hint :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isHidden :: Kernel.Prelude.Bool,
    media :: Kernel.Prelude.Maybe [Domain.Types.DocumentVerificationStagesConfig.MediaInfo],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    order :: Kernel.Prelude.Int,
    stageDependency :: [Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage],
    title :: Kernel.Prelude.Text,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MediaInfo = MediaInfo {description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, thumbnailUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text, title :: Kernel.Prelude.Text, url :: Kernel.Prelude.Text}
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
