{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantOnboardingStepConfig where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.MerchantOnboarding
import qualified Tools.Beam.UtilsTH



data MerchantOnboardingStepConfig
    = MerchantOnboardingStepConfig {createdAt :: Kernel.Prelude.UTCTime,
                                    dependency :: [Kernel.Prelude.Text],
                                    isAdminOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                                    isApprovalRequired :: Kernel.Prelude.Bool,
                                    onboardingType :: Domain.Types.MerchantOnboarding.OnboardingType,
                                    stepDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                    stepNameIdentifier :: Kernel.Prelude.Text,
                                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



