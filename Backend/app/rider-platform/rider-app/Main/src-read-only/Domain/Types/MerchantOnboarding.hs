{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantOnboarding where
import Kernel.Prelude
import Kernel.Utils.TH
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOnboardingStep
import qualified Tools.Beam.UtilsTH



data MerchantOnboarding
    = MerchantOnboarding {createdAt :: Kernel.Prelude.UTCTime,
                          description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          id :: Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding,
                          onboardingType :: Domain.Types.MerchantOnboarding.OnboardingType,
                          remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                          requestorId :: Kernel.Prelude.Text,
                          status :: Domain.Types.MerchantOnboarding.OnboardingStatus,
                          updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data GetFileResponse = GetFileResponse {fileBase64 :: Kernel.Prelude.Text, fileType :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data MerchantOnboardingAPI
    = MerchantOnboardingAPI {createdAt :: Kernel.Prelude.UTCTime,
                             description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             id :: Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding,
                             onboardingType :: Domain.Types.MerchantOnboarding.OnboardingType,
                             remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             requestorId :: Kernel.Prelude.Text,
                             status :: Domain.Types.MerchantOnboarding.OnboardingStatus,
                             steps :: [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep],
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data OnboardingStatus = INPROGRESS | COMPLETED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data OnboardingType = TICKET_MERCHANT_ONBOARDING | OTHER_ONBOARDING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data RequestorRole
    = TICKET_DASHBOARD_USER | TICKET_DASHBOARD_MERCHANT | TICKET_DASHBOARD_ADMIN | TICKET_DASHBOARD_APPROVER
    deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OnboardingStatus))

$(mkHttpInstancesForEnum (''OnboardingStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OnboardingType))

$(mkHttpInstancesForEnum (''OnboardingType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RequestorRole))

$(mkHttpInstancesForEnum (''RequestorRole))

