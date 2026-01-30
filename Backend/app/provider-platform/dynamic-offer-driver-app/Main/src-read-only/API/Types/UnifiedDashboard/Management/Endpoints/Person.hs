{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UnifiedDashboard.Management.Endpoints.Person where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CreatePersonReq = CreatePersonReq
  { email :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    lastName :: Data.Text.Text,
    mobileCountryCode :: Data.Text.Text,
    mobileNumber :: Data.Text.Text,
    password :: Kernel.Prelude.Maybe Data.Text.Text,
    roleName :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreatePersonReq where
  hideSecrets = Kernel.Prelude.identity

newtype CreatePersonResp = CreatePersonResp {personId :: Kernel.Types.Id.Id Dashboard.Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SendOtpReq = SendOtpReq {mobileNumber :: Data.Text.Text, mobileCountryCode :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype SendOtpResp = SendOtpResp {otp :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (PostPersonPersonCreateHelper :<|> PostPersonRegistrationSendOtpHelper)

type PostPersonCreate = ("person" :> "create" :> ReqBody ('[JSON]) CreatePersonReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostPersonPersonCreateHelper = ("person" :> "create" :> ReqBody ('[JSON]) CreatePersonReq :> Post ('[JSON]) CreatePersonResp)

type PostUserLoginSendOtp = ("user" :> "login" :> "sendOtp" :> ReqBody ('[JSON]) SendOtpReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostPersonRegistrationSendOtpHelper = ("registration" :> "sendOtp" :> ReqBody ('[JSON]) SendOtpReq :> Post ('[JSON]) SendOtpResp)

data PersonAPIs = PersonAPIs {postPersonCreate :: (CreatePersonReq -> EulerHS.Types.EulerClient CreatePersonResp), postUserLoginSendOtp :: (SendOtpReq -> EulerHS.Types.EulerClient SendOtpResp)}

mkPersonAPIs :: (Client EulerHS.Types.EulerClient API -> PersonAPIs)
mkPersonAPIs personClient = (PersonAPIs {..})
  where
    postPersonCreate :<|> postUserLoginSendOtp = personClient

data PersonUserActionType
  = POST_PERSON_CREATE
  | POST_USER_LOGIN_SEND_OTP
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PersonUserActionType)])
