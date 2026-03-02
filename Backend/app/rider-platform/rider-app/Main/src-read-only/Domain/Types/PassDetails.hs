{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PassDetails where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassOrganization
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PassDetails = PassDetails
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    applicableRouteIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    createdAt :: Data.Time.UTCTime,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passEnum :: Domain.Types.PassType.PassEnum,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    registerNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.PassDetails.RoutePair],
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime,
    verificationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RoutePair = RoutePair {destStageName :: Kernel.Prelude.Text, destStopName :: Kernel.Prelude.Text, srcStageName :: Kernel.Prelude.Text, srcStopName :: Kernel.Prelude.Text}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data VerificationStatus = PENDING | VERIFIED | REJECTED | EXPIRED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''VerificationStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''VerificationStatus)
