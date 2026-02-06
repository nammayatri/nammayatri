{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Organization where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified "this" Domain.Types.Organization
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Student
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data OrganizationUpdateReq = OrganizationUpdateReq
  { organizationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    organizationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Organization.Organization),
    organizationName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets OrganizationUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data StudentInfoResp = StudentInfoResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Data.Time.UTCTime,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentId :: Kernel.Types.Id.Id Domain.Types.Student.Student,
    updatedAt :: Data.Time.UTCTime,
    verificationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    verificationStatus :: Domain.Types.Student.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StudentVerification = StudentVerification
  { graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    studentId :: Kernel.Types.Id.Id Domain.Types.Student.Student,
    verificationStatus :: Domain.Types.Student.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StudentsListResp = StudentsListResp {offset :: Kernel.Prelude.Int, students :: [StudentInfoResp], totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyStudentsReq = VerifyStudentsReq {studentVerifications :: [StudentVerification]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyStudentsReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("organization" :> (GetOrganizationGetOrganizationId :<|> GetOrganizationStudentOrganization :<|> PostOrganizationStudentVerify :<|> PostOrganizationOrganizationUpdate))

type GetOrganizationGetOrganizationId =
  ( "getOrganizationId" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Get
           ('[JSON])
           ((Kernel.Types.Id.Id Domain.Types.Organization.Organization))
  )

type GetOrganizationStudentOrganization =
  ( "student" :> "organization" :> Capture "organizationId" (Kernel.Types.Id.Id Domain.Types.Organization.Organization)
      :> QueryParam
           "status"
           Domain.Types.Student.VerificationStatus
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           ('[JSON])
           StudentsListResp
  )

type PostOrganizationStudentVerify = ("student" :> "verify" :> ReqBody ('[JSON]) VerifyStudentsReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostOrganizationOrganizationUpdate =
  ( "organization" :> "update" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody ('[JSON]) OrganizationUpdateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data OrganizationAPIs = OrganizationAPIs
  { getOrganizationGetOrganizationId :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient (Kernel.Types.Id.Id Domain.Types.Organization.Organization)),
    getOrganizationStudentOrganization :: (Kernel.Types.Id.Id Domain.Types.Organization.Organization -> Kernel.Prelude.Maybe (Domain.Types.Student.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient StudentsListResp),
    postOrganizationStudentVerify :: (VerifyStudentsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postOrganizationOrganizationUpdate :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> OrganizationUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkOrganizationAPIs :: (Client EulerHS.Types.EulerClient API -> OrganizationAPIs)
mkOrganizationAPIs organizationClient = (OrganizationAPIs {..})
  where
    getOrganizationGetOrganizationId :<|> getOrganizationStudentOrganization :<|> postOrganizationStudentVerify :<|> postOrganizationOrganizationUpdate = organizationClient

data OrganizationUserActionType
  = GET_ORGANIZATION_GET_ORGANIZATION_ID
  | GET_ORGANIZATION_STUDENT_ORGANIZATION
  | POST_ORGANIZATION_STUDENT_VERIFY
  | POST_ORGANIZATION_ORGANIZATION_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''OrganizationUserActionType)])
