{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MerchantOnboardingStepConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.MerchantOnboarding
import qualified Database.Beam as B



data MerchantOnboardingStepConfigT f
    = MerchantOnboardingStepConfigT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                     dependency :: (B.C f [Kernel.Prelude.Text]),
                                     isAdminOnly :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                                     isApprovalRequired :: (B.C f Kernel.Prelude.Bool),
                                     onboardingType :: (B.C f Domain.Types.MerchantOnboarding.OnboardingType),
                                     stepDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                     stepNameIdentifier :: (B.C f Kernel.Prelude.Text),
                                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MerchantOnboardingStepConfigT
    where data PrimaryKey MerchantOnboardingStepConfigT f = MerchantOnboardingStepConfigId (B.C f Domain.Types.MerchantOnboarding.OnboardingType) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MerchantOnboardingStepConfigId <$> onboardingType <*> stepNameIdentifier
type MerchantOnboardingStepConfig = MerchantOnboardingStepConfigT Identity

$(enableKVPG (''MerchantOnboardingStepConfigT) [('onboardingType), ('stepNameIdentifier)] [])

$(mkTableInstances (''MerchantOnboardingStepConfigT) "merchant_onboarding_step_config")

