{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.PassOrganization where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified "this" Domain.Types.PassDetails
import qualified "this" Domain.Types.PassOrganization
import qualified "this" Domain.Types.PassType
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AssignDepotReq = AssignDepotReq {depotId :: Kernel.Prelude.Maybe Kernel.Prelude.Text, passOrganizationIds :: [Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AssignDepotReq where
  hideSecrets = Kernel.Prelude.identity

data GetOrganizationResp = GetOrganizationResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    depotId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    name :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsInfoResp = PassDetailsInfoResp
  { academicYearEnd :: Kernel.Prelude.Maybe Data.Time.Day,
    academicYearStart :: Kernel.Prelude.Maybe Data.Time.Day,
    address :: Kernel.Prelude.Maybe Domain.Types.PassDetails.PassDetailAddress,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Data.Time.UTCTime,
    department :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gender :: Domain.Types.Person.Gender,
    guardianMobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.PassDetails.RoutePair],
    selfImage :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile,
    updatedAt :: Data.Time.UTCTime,
    validTill :: Data.Time.UTCTime,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus,
    year :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsListResp = PassDetailsListResp {limit :: Kernel.Prelude.Int, offset :: Kernel.Prelude.Int, passDetails :: [PassDetailsInfoResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsVerification = PassDetailsVerification
  { academicYearEnd :: Kernel.Prelude.Maybe Data.Time.Day,
    academicYearStart :: Kernel.Prelude.Maybe Data.Time.Day,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassOrganizationUpdateReq = PassOrganizationUpdateReq {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text, name :: Kernel.Prelude.Text, passEnum :: Domain.Types.PassType.PassEnum}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassOrganizationUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data VerifyPassDetailsReq = VerifyPassDetailsReq {verifications :: [PassDetailsVerification]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyPassDetailsReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("passOrganization" :> (GetPassOrganizationGetPassOrganization :<|> GetPassOrganizationPassDetails :<|> PostPassOrganizationPassDetailsVerify :<|> PostPassOrganizationUpdate :<|> GetPassOrganizationGetOrganizations :<|> GetPassOrganizationPassDetailsDocument :<|> PostPassOrganizationAssignDepot))

type GetPassOrganizationGetPassOrganization = ("getPassOrganization" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get '[JSON] GetOrganizationResp)

type GetPassOrganizationPassDetails =
  ( "passDetails" :> Capture "passEnum" Kernel.Prelude.Text
      :> QueryParam
           "passOrganizationId"
           (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization)
      :> QueryParam "status" Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           PassDetailsListResp
  )

type PostPassOrganizationPassDetailsVerify = ("passDetails" :> "verify" :> ReqBody '[JSON] VerifyPassDetailsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPassOrganizationUpdate =
  ( "update" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] PassOrganizationUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetPassOrganizationGetOrganizations =
  ( "getOrganizations" :> Capture "passEnum" Kernel.Prelude.Text :> QueryParam "depotPersonId" (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Get
           '[JSON]
           [GetOrganizationResp]
  )

type GetPassOrganizationPassDetailsDocument =
  ( "passDetails" :> "document" :> Capture "documentId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile)
      :> Get
           '[JSON]
           Kernel.Prelude.Text
  )

type PostPassOrganizationAssignDepot =
  ( "assignDepot" :> Capture "depotPersonId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] AssignDepotReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data PassOrganizationAPIs = PassOrganizationAPIs
  { getPassOrganizationGetPassOrganization :: Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient GetOrganizationResp,
    getPassOrganizationPassDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient PassDetailsListResp,
    postPassOrganizationPassDetailsVerify :: VerifyPassDetailsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPassOrganizationUpdate :: Kernel.Types.Id.Id Domain.Types.Person.Person -> PassOrganizationUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPassOrganizationGetOrganizations :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> EulerHS.Types.EulerClient [GetOrganizationResp],
    getPassOrganizationPassDetailsDocument :: Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> EulerHS.Types.EulerClient Kernel.Prelude.Text,
    postPassOrganizationAssignDepot :: Kernel.Types.Id.Id Domain.Types.Person.Person -> AssignDepotReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkPassOrganizationAPIs :: (Client EulerHS.Types.EulerClient API -> PassOrganizationAPIs)
mkPassOrganizationAPIs passOrganizationClient = (PassOrganizationAPIs {..})
  where
    getPassOrganizationGetPassOrganization :<|> getPassOrganizationPassDetails :<|> postPassOrganizationPassDetailsVerify :<|> postPassOrganizationUpdate :<|> getPassOrganizationGetOrganizations :<|> getPassOrganizationPassDetailsDocument :<|> postPassOrganizationAssignDepot = passOrganizationClient

data PassOrganizationUserActionType
  = GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION
  | GET_PASS_ORGANIZATION_PASS_DETAILS
  | POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY
  | POST_PASS_ORGANIZATION_UPDATE
  | GET_PASS_ORGANIZATION_GET_ORGANIZATIONS
  | GET_PASS_ORGANIZATION_PASS_DETAILS_DOCUMENT
  | POST_PASS_ORGANIZATION_ASSIGN_DEPOT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PassOrganizationUserActionType])
