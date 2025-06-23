{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantOnboardingStep where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantOnboardingStep
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantOnboardingStepT f = MerchantOnboardingStepT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    dependency :: B.C f [Kernel.Prelude.Text],
    id :: B.C f Kernel.Prelude.Text,
    isAdminOnly :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isApprovalRequired :: B.C f Kernel.Prelude.Bool,
    merchantOnboardingId :: B.C f Kernel.Prelude.Text,
    payload :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    remarks :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.MerchantOnboardingStep.StepStatus,
    stepDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    stepNameIdentifier :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantOnboardingStepT where
  data PrimaryKey MerchantOnboardingStepT f = MerchantOnboardingStepId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantOnboardingStepId . id

type MerchantOnboardingStep = MerchantOnboardingStepT Identity

$(enableKVPG ''MerchantOnboardingStepT ['id] [['merchantOnboardingId]])

$(mkTableInstances ''MerchantOnboardingStepT "merchant_onboarding_step")
