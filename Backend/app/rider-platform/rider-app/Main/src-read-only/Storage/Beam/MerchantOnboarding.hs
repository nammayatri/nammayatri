{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantOnboarding where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantOnboarding
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantOnboardingT f = MerchantOnboardingT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    onboardingType :: B.C f Domain.Types.MerchantOnboarding.OnboardingType,
    remarks :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestorId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.MerchantOnboarding.OnboardingStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantOnboardingT where
  data PrimaryKey MerchantOnboardingT f = MerchantOnboardingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantOnboardingId . id

type MerchantOnboarding = MerchantOnboardingT Identity

$(enableKVPG ''MerchantOnboardingT ['id] [])

$(mkTableInstances ''MerchantOnboardingT "merchant_onboarding")
