{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Image where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH
import qualified Tools.Error

data Image = Image
  { documentExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError,
    id :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    imageType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rcId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reviewerEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    s3Path :: Kernel.Prelude.Text,
    verificationStatus :: Kernel.Prelude.Maybe Kernel.Types.Documents.VerificationStatus,
    workflowTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SelfieFetchStatus = APPROVED | NEEDS_REVIEW deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SelfieFetchStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''SelfieFetchStatus)
