{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.CommonDriverOnboardingDocuments where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Image
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Types.Documents
import qualified Tools.Beam.UtilsTH



data CommonDriverOnboardingDocuments
    = CommonDriverOnboardingDocuments {documentData :: Kernel.Prelude.Text,
                                       documentImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
                                       documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
                                       driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
                                       id :: Kernel.Types.Id.Id Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments,
                                       merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                       merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                                       rejectReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                       verificationStatus :: Kernel.Types.Documents.VerificationStatus,
                                       createdAt :: Kernel.Prelude.UTCTime,
                                       updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



