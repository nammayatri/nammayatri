{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DocumentVerificationStagesConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentOnboardingStage
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DocumentVerificationStagesConfigT f = DocumentVerificationStagesConfigT
  { applicableTo :: B.C f Domain.Types.DocumentVerificationConfig.DocumentApplicableType,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    documentCategory :: B.C f Domain.Types.DocumentVerificationConfig.DocumentCategory,
    documentOnboardingStage :: B.C f Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage,
    hint :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isHidden :: B.C f Kernel.Prelude.Bool,
    mediaJSON :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    order :: B.C f Kernel.Prelude.Int,
    stageDependency :: B.C f [Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage],
    title :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f Domain.Types.VehicleCategory.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DocumentVerificationStagesConfigT where
  data PrimaryKey DocumentVerificationStagesConfigT f
    = DocumentVerificationStagesConfigId
        (B.C f Domain.Types.DocumentVerificationConfig.DocumentApplicableType)
        (B.C f Domain.Types.DocumentVerificationConfig.DocumentCategory)
        (B.C f Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage)
        (B.C f Kernel.Prelude.Text)
        (B.C f Domain.Types.VehicleCategory.VehicleCategory)
    deriving (Generic, B.Beamable)
  primaryKey = DocumentVerificationStagesConfigId <$> applicableTo <*> documentCategory <*> documentOnboardingStage <*> merchantOperatingCityId <*> vehicleCategory

type DocumentVerificationStagesConfig = DocumentVerificationStagesConfigT Identity

$(enableKVPG ''DocumentVerificationStagesConfigT ['applicableTo, 'documentCategory, 'documentOnboardingStage, 'merchantOperatingCityId, 'vehicleCategory] [])

$(mkTableInstances ''DocumentVerificationStagesConfigT "document_verification_stages_config")
