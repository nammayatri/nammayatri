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
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data GetOrganizationResp = GetOrganizationResp {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text, id :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsInfoResp = PassDetailsInfoResp
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Data.Time.UTCTime,
    gender :: Domain.Types.Person.Gender,
    graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passDetailsId :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.PassDetails.RoutePair],
    selfImage :: Kernel.Prelude.Text,
    studentClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime,
    validTill :: Data.Time.UTCTime,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsListResp = PassDetailsListResp {limit :: Kernel.Prelude.Int, offset :: Kernel.Prelude.Int, passDetails :: [PassDetailsInfoResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsVerification = PassDetailsVerification
  { graduationDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
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

type API = ("passOrganization" :> (GetPassOrganizationGetPassOrganization :<|> GetPassOrganizationPassDetails :<|> PostPassOrganizationPassDetailsVerify :<|> PostPassOrganizationUpdate :<|> GetPassOrganizationGetOrganizations :<|> GetPassOrganizationPassDetailsMedia))

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

type GetPassOrganizationGetOrganizations = ("getOrganizations" :> Capture "passEnum" Kernel.Prelude.Text :> Get '[JSON] [GetOrganizationResp])

type GetPassOrganizationPassDetailsMedia = ("passDetails" :> "media" :> MandatoryQueryParam "filePath" Kernel.Prelude.Text :> Get '[JSON] Kernel.Prelude.Text)

data PassOrganizationAPIs = PassOrganizationAPIs
  { getPassOrganizationGetPassOrganization :: Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient GetOrganizationResp,
    getPassOrganizationPassDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient PassDetailsListResp,
    postPassOrganizationPassDetailsVerify :: VerifyPassDetailsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPassOrganizationUpdate :: Kernel.Types.Id.Id Domain.Types.Person.Person -> PassOrganizationUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPassOrganizationGetOrganizations :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient [GetOrganizationResp],
    getPassOrganizationPassDetailsMedia :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Prelude.Text
  }

mkPassOrganizationAPIs :: (Client EulerHS.Types.EulerClient API -> PassOrganizationAPIs)
mkPassOrganizationAPIs passOrganizationClient = (PassOrganizationAPIs {..})
  where
    getPassOrganizationGetPassOrganization :<|> getPassOrganizationPassDetails :<|> postPassOrganizationPassDetailsVerify :<|> postPassOrganizationUpdate :<|> getPassOrganizationGetOrganizations :<|> getPassOrganizationPassDetailsMedia = passOrganizationClient

data PassOrganizationUserActionType
  = GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION
  | GET_PASS_ORGANIZATION_PASS_DETAILS
  | POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY
  | POST_PASS_ORGANIZATION_UPDATE
  | GET_PASS_ORGANIZATION_GET_ORGANIZATIONS
  | GET_PASS_ORGANIZATION_PASS_DETAILS_MEDIA
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PassOrganizationUserActionType])
