{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantOnboardingStep where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data MerchantOnboardingStep = MerchantOnboardingStep
  { createdAt :: Kernel.Prelude.UTCTime,
    dependency :: [Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep],
    id :: Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep,
    isAdminOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isApprovalRequired :: Kernel.Prelude.Bool,
    merchantOnboardingId :: Kernel.Prelude.Text,
    payload :: Kernel.Prelude.Maybe Data.Aeson.Value,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.MerchantOnboardingStep.StepStatus,
    stepDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stepNameIdentifier :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data StepStatus = UNAVAILABLE | AVAILABLE | INPROGRESS | SUBMITTED | COMPLETED | REOPENED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''StepStatus)

$(mkHttpInstancesForEnum ''StepStatus)
