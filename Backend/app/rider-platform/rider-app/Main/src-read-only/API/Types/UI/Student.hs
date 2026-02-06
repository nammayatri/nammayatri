{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Student where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.Organization
import qualified Domain.Types.Student
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data GetOrganizationResp = GetOrganizationResp
  { organizationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    organizationId :: Kernel.Types.Id.Id Domain.Types.Organization.Organization,
    organizationName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StudentActivateReq = StudentActivateReq {destinationLat :: Kernel.Prelude.Double, destinationLon :: Kernel.Prelude.Double, sourceLat :: Kernel.Prelude.Double, sourceLon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StudentDataResp = StudentDataResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    organizationId :: Kernel.Types.Id.Id Domain.Types.Organization.Organization,
    organizationName :: Kernel.Prelude.Text,
    routePairs :: [Domain.Types.Student.RoutePair],
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentId :: Kernel.Types.Id.Id Domain.Types.Student.Student
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StudentStatusResp = StudentStatusResp {remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text, verificationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime, verificationStatus :: Domain.Types.Student.VerificationStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StudentUpdateReq = StudentUpdateReq
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    organizationId :: Kernel.Types.Id.Id Domain.Types.Organization.Organization,
    routePairs :: [Domain.Types.Student.RoutePair],
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
