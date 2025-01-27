{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Registration where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Registration
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.RegistrationToken
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CustomerAuthReq = CustomerAuthReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text, otpChannel :: Kernel.Prelude.Maybe Domain.Action.UI.Registration.OTPChannel}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("registration" :> (PostRegistrationAuth :<|> PostRegistrationVerify :<|> PostRegistrationOtpResend :<|> PostRegistrationLogout))

type PostRegistrationAuth = ("auth" :> ReqBody '[JSON] CustomerAuthReq :> Post '[JSON] Domain.Action.UI.Registration.AuthRes)

type PostRegistrationVerify =
  ( Capture "authId" (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken) :> "verify"
      :> ReqBody
           '[JSON]
           Domain.Action.UI.Registration.AuthVerifyReq
      :> Post '[JSON] Domain.Action.UI.Registration.AuthVerifyRes
  )

type PostRegistrationOtpResend =
  ( "otp" :> Capture "authId" (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken) :> "resend"
      :> Post
           '[JSON]
           Domain.Action.UI.Registration.ResendAuthRes
  )

type PostRegistrationLogout = ("logout" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data RegistrationAPIs = RegistrationAPIs
  { postRegistrationAuth :: CustomerAuthReq -> EulerHS.Types.EulerClient Domain.Action.UI.Registration.AuthRes,
    postRegistrationVerify :: Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Domain.Action.UI.Registration.AuthVerifyReq -> EulerHS.Types.EulerClient Domain.Action.UI.Registration.AuthVerifyRes,
    postRegistrationOtpResend :: Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> EulerHS.Types.EulerClient Domain.Action.UI.Registration.ResendAuthRes,
    postRegistrationLogout :: Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> RegistrationAPIs)
mkRegistrationAPIs registrationClient = (RegistrationAPIs {..})
  where
    postRegistrationAuth :<|> postRegistrationVerify :<|> postRegistrationOtpResend :<|> postRegistrationLogout = registrationClient

data RegistrationUserActionType
  = POST_REGISTRATION_AUTH
  | POST_REGISTRATION_VERIFY
  | POST_REGISTRATION_OTP_RESEND
  | POST_REGISTRATION_LOGOUT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RegistrationUserActionType])
