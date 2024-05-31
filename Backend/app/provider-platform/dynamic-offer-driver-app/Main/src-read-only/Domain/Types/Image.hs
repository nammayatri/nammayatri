{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Image where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Tools.Error

data Image = Image
  { documentExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError,
    id :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    imageType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rcId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    s3Path :: Kernel.Prelude.Text,
    verificationStatus :: Kernel.Prelude.Maybe Kernel.Types.Documents.VerificationStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
