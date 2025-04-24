{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantOnboardingStepConfig where

import Data.Aeson
import qualified Domain.Types.MerchantOnboarding
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data MerchantOnboardingStepConfig = MerchantOnboardingStepConfig
  { createdAt :: Kernel.Prelude.UTCTime,
    dependency :: [Kernel.Prelude.Text],
    isAdminOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isApprovalRequired :: Kernel.Prelude.Bool,
    onboardingType :: Domain.Types.MerchantOnboarding.OnboardingType,
    stepDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stepNameIdentifier :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
