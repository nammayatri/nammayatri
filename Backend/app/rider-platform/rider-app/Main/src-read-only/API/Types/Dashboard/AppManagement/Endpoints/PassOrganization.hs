{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.AppManagement.Endpoints.PassOrganization where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified "this" Domain.Types.PassOrganization
import qualified Data.Time
import qualified "this" Domain.Types.PassDetails
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.PassType
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data PassDetailsInfoResp = PassDetailsInfoResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Data.Time.UTCTime,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime,
    verificationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsListResp = PassDetailsListResp {offset :: Kernel.Prelude.Int, passDetails :: [PassDetailsInfoResp], totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsVerification = PassDetailsVerification
  { graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassOrganizationUpdateReq = PassOrganizationUpdateReq
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization),
    name :: Kernel.Prelude.Text,
    passEnum :: Domain.Types.PassType.PassEnum
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetOrganizationResp = GetOrganizationResp
  { id :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    name :: Kernel.Prelude.Text,
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassOrganizationUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data VerifyPassDetailsReq = VerifyPassDetailsReq {verifications :: [PassDetailsVerification]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyPassDetailsReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("passOrganization" :> (GetPassOrganizationGetPassOrganizationId :<|> GetPassOrganizationPassDetails :<|> PostPassOrganizationPassDetailsVerify :<|> PostPassOrganizationUpdate :<|> GetPassOrganizationGetOrganizations))

type GetPassOrganizationGetPassOrganizationId =
  ( "getPassOrganizationId" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Get
           ('[JSON])
           ((Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization))
  )

type GetPassOrganizationPassDetails =
  ( "passDetails" :> Capture "passOrganizationId" (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization)
      :> QueryParam
           "status"
           Domain.Types.PassDetails.VerificationStatus
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           ('[JSON])
           PassDetailsListResp
  )

type PostPassOrganizationPassDetailsVerify = ("passDetails" :> "verify" :> ReqBody ('[JSON]) VerifyPassDetailsReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostPassOrganizationUpdate =
  ( "update" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody ('[JSON]) PassOrganizationUpdateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetPassOrganizationGetOrganizations = ("getOrganizations" :> Capture "passEnum" Domain.Types.PassType.PassEnum :> Get ('[JSON]) [GetOrganizationResp])

data PassOrganizationAPIs = PassOrganizationAPIs
  { getPassOrganizationGetPassOrganizationId :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization)),
    getPassOrganizationPassDetails :: (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> Kernel.Prelude.Maybe (Domain.Types.PassDetails.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient PassDetailsListResp),
    postPassOrganizationPassDetailsVerify :: (VerifyPassDetailsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postPassOrganizationUpdate :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> PassOrganizationUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getPassOrganizationGetOrganizations :: (Domain.Types.PassType.PassEnum -> EulerHS.Types.EulerClient [GetOrganizationResp])
  }

mkPassOrganizationAPIs :: (Client EulerHS.Types.EulerClient API -> PassOrganizationAPIs)
mkPassOrganizationAPIs passOrganizationClient = (PassOrganizationAPIs {..})
  where
    getPassOrganizationGetPassOrganizationId :<|> getPassOrganizationPassDetails :<|> postPassOrganizationPassDetailsVerify :<|> postPassOrganizationUpdate :<|> getPassOrganizationGetOrganizations = passOrganizationClient

data PassOrganizationUserActionType
    = GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION_ID
    | GET_PASS_ORGANIZATION_PASS_DETAILS
    | POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY
    | POST_PASS_ORGANIZATION_UPDATE
    | GET_PASS_ORGANIZATION_GET_ORGANIZATIONS
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PassOrganizationUserActionType)])
