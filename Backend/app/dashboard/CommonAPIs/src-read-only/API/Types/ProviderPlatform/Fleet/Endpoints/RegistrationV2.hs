{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2 where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.PaymentMode
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data FleetBankAccountLinkResp = FleetBankAccountLinkResp
  { chargesEnabled :: Kernel.Prelude.Bool,
    detailsSubmitted :: Kernel.Prelude.Bool,
    accountLink :: Kernel.Prelude.BaseUrl,
    accountUrlExpiry :: Kernel.Prelude.UTCTime,
    paymentMode :: Domain.Types.PaymentMode.PaymentMode
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetBankAccountResp = FleetBankAccountResp {chargesEnabled :: Kernel.Prelude.Bool, detailsSubmitted :: Kernel.Prelude.Bool, paymentMode :: Domain.Types.PaymentMode.PaymentMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerLoginReqV2 = FleetOwnerLoginReqV2 {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerLoginResV2 = FleetOwnerLoginResV2 {personId :: Kernel.Types.Id.Id Dashboard.Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerLoginResV2 where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerRegisterReqV2 = FleetOwnerRegisterReqV2
  { firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person),
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetType :: Kernel.Prelude.Maybe FleetType,
    businessLicenseNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessLicenseImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorReferralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminApprovalRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    setIsEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerRegisterResV2 = FleetOwnerRegisterResV2 {enabled :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerRegisterTReqV2 = FleetOwnerRegisterTReqV2
  { firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person),
    fleetType :: Kernel.Prelude.Maybe FleetType,
    operatorReferralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminApprovalRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    setIsEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerVerifyReqV2 = FleetOwnerVerifyReqV2 {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text, otp :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerVerifyResV2 = FleetOwnerVerifyResV2 {authToken :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetType
  = RENTAL_FLEET
  | NORMAL_FLEET
  | BUSINESS_FLEET
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("fleet" :> (PostRegistrationV2V2LoginOtpHelper :<|> PostRegistrationV2V2VerifyOtpHelper :<|> PostRegistrationV2V2RegisterHelper :<|> PostRegistrationV2RegisterBankAccountLinkHelper :<|> GetRegistrationV2RegisterBankAccountStatusHelper))

type PostRegistrationV2LoginOtp = ("v2" :> "login" :> "otp" :> ReqBody '[JSON] FleetOwnerLoginReqV2 :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationV2V2LoginOtpHelper =
  ( "v2" :> "login" :> "otp" :> MandatoryQueryParam "enabled" Kernel.Prelude.Bool :> ReqBody '[JSON] FleetOwnerLoginReqV2
      :> Post
           '[JSON]
           FleetOwnerLoginResV2
  )

type PostRegistrationV2VerifyOtp = ("v2" :> "verify" :> "otp" :> ReqBody '[JSON] FleetOwnerVerifyReqV2 :> Post '[JSON] FleetOwnerVerifyResV2)

type PostRegistrationV2V2VerifyOtpHelper = ("v2" :> "verify" :> "otp" :> ReqBody '[JSON] FleetOwnerVerifyReqV2 :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationV2Register = ("v2" :> "register" :> ReqBody '[JSON] FleetOwnerRegisterReqV2 :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationV2V2RegisterHelper =
  ( "v2" :> "register" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerRegisterReqV2
      :> Post
           '[JSON]
           FleetOwnerRegisterResV2
  )

type PostRegistrationV2RegisterBankAccountLink =
  ( "register" :> "bankAccount" :> "link" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "paymentMode"
           Domain.Types.PaymentMode.PaymentMode
      :> Post '[JSON] FleetBankAccountLinkResp
  )

type PostRegistrationV2RegisterBankAccountLinkHelper =
  ( "register" :> "bankAccount" :> "link" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "paymentMode"
           Domain.Types.PaymentMode.PaymentMode
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Post '[JSON] FleetBankAccountLinkResp
  )

type GetRegistrationV2RegisterBankAccountStatus = ("register" :> "bankAccount" :> "status" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> Get '[JSON] FleetBankAccountResp)

type GetRegistrationV2RegisterBankAccountStatusHelper =
  ( "register" :> "bankAccount" :> "status" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get '[JSON] FleetBankAccountResp
  )

data RegistrationV2APIs = RegistrationV2APIs
  { postRegistrationV2LoginOtp :: Kernel.Prelude.Bool -> FleetOwnerLoginReqV2 -> EulerHS.Types.EulerClient FleetOwnerLoginResV2,
    postRegistrationV2VerifyOtp :: FleetOwnerVerifyReqV2 -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postRegistrationV2Register :: Kernel.Prelude.Text -> FleetOwnerRegisterReqV2 -> EulerHS.Types.EulerClient FleetOwnerRegisterResV2,
    postRegistrationV2RegisterBankAccountLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.PaymentMode.PaymentMode -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetBankAccountLinkResp,
    getRegistrationV2RegisterBankAccountStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetBankAccountResp
  }

mkRegistrationV2APIs :: (Client EulerHS.Types.EulerClient API -> RegistrationV2APIs)
mkRegistrationV2APIs registrationV2Client = (RegistrationV2APIs {..})
  where
    postRegistrationV2LoginOtp :<|> postRegistrationV2VerifyOtp :<|> postRegistrationV2Register :<|> postRegistrationV2RegisterBankAccountLink :<|> getRegistrationV2RegisterBankAccountStatus = registrationV2Client

data RegistrationV2UserActionType
  = POST_REGISTRATION_V2_LOGIN_OTP
  | POST_REGISTRATION_V2_VERIFY_OTP
  | POST_REGISTRATION_V2_REGISTER
  | POST_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_LINK
  | GET_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_STATUS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RegistrationV2UserActionType])
