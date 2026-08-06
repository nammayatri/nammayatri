{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management.Endpoints.Registration where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.Merchant
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data LoginReq = LoginReq {email :: Kernel.Prelude.Maybe Data.Text.Text, mobileNumber :: Kernel.Prelude.Maybe Data.Text.Text, mobileCountryCode :: Kernel.Prelude.Maybe Data.Text.Text, password :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LoginRes = LoginRes {token :: Data.Text.Text, city :: Kernel.Types.Beckn.Context.City, merchantId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyOtpReq = VerifyOtpReq {mobileNumber :: Data.Text.Text, mobileCountryCode :: Data.Text.Text, otp :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (PostUserLogin :<|> PostUserLoginVerifyOtp)

type PostUserLogin = ("user" :> "login" :> ReqBody ('[JSON]) LoginReq :> Post ('[JSON]) LoginRes)

type PostUserLoginVerifyOtp = ("user" :> "login" :> "verifyOtp" :> ReqBody ('[JSON]) VerifyOtpReq :> Post ('[JSON]) LoginRes)

data RegistrationAPIs = RegistrationAPIs {postUserLogin :: (LoginReq -> EulerHS.Types.EulerClient LoginRes), postUserLoginVerifyOtp :: (VerifyOtpReq -> EulerHS.Types.EulerClient LoginRes)}

mkRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> RegistrationAPIs)
mkRegistrationAPIs registrationClient = (RegistrationAPIs {..})
  where
    postUserLogin :<|> postUserLoginVerifyOtp = registrationClient

data RegistrationUserActionType
  = POST_USER_LOGIN
  | POST_USER_LOGIN_VERIFY_OTP
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RegistrationUserActionType)])
