{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverOnboardingV2 where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DocumentVerificationConfig
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DocumentVerificationConfigAPIEntity = DocumentVerificationConfigAPIEntity
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [Domain.Types.DocumentVerificationConfig.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    rcNumberPrefixList :: [Kernel.Prelude.Text],
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigList = DocumentVerificationConfigList
  { autos :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    bikes :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity],
    cabs :: Kernel.Prelude.Maybe [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
