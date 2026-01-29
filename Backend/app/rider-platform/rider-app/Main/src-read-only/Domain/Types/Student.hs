{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Student where

import Data.Aeson
import qualified Domain.Types.College
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Student = Student
  { collegeId :: Kernel.Types.Id.Id Domain.Types.College.College,
    collegeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    destinationStop :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    graduationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Student.Student,
    intermediateStops :: [Kernel.Prelude.Text],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    sourceStop :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentAge :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentName :: Kernel.Prelude.Text,
    studentPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    verificationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verificationStatus :: Domain.Types.Student.VerificationStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VerificationStatus = EXPIRED | PENDING | VERIFIED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''VerificationStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''VerificationStatus))
