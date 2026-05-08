{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PassDetails where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.PassDetails
import qualified Domain.Types.PassOrganization
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data GetOrganizationResp = GetOrganizationResp {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text, id :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsDataResp = PassDetailsDataResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    gender :: Domain.Types.Person.Gender,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    passOrganizationName :: Kernel.Prelude.Text,
    pincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registerNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.PassDetails.RoutePair],
    selfImage :: Kernel.Prelude.Text,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validTill :: Data.Time.UTCTime,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsUpdateReq = PassDetailsUpdateReq
  { aadharNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    gender :: Domain.Types.Person.Gender,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    pincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registerNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeDetails :: [RouteDetails],
    selfImage :: Kernel.Prelude.Text,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassStatusResp = PassStatusResp {remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text, validTill :: Data.Time.UTCTime, verificationStatus :: Domain.Types.PassDetails.VerificationStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteDetails = RouteDetails {destStopId :: Kernel.Prelude.Text, destStopName :: Kernel.Prelude.Text, srcStopId :: Kernel.Prelude.Text, srcStopName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
