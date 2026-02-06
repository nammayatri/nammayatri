{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Student where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Organization
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Student = Student
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Data.Time.UTCTime,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Student.Student,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    organizationId :: Kernel.Types.Id.Id Domain.Types.Organization.Organization,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.Student.RoutePair],
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime,
    verificationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    verificationStatus :: Domain.Types.Student.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RoutePair = RoutePair
  { destinationLat :: Kernel.Prelude.Double,
    destinationLon :: Kernel.Prelude.Double,
    sourceLat :: Kernel.Prelude.Double,
    sourceLon :: Kernel.Prelude.Double,
    tripType :: Domain.Types.Student.TripType
  }
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data TripType = FORWARD | RETURN deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data VerificationStatus = PENDING | VERIFIED | REJECTED | EXPIRED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''VerificationStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''VerificationStatus))
