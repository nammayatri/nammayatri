{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SocialLogin where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data EmailAuthV2Req = EmailAuthV2Req {email :: Kernel.Prelude.Text, merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EmailAuthV2Res = EmailAuthV2Res {attempts :: Kernel.Prelude.Int, authId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EmailAuthVerifyV2Req = EmailAuthVerifyV2Req {deviceToken :: Kernel.Prelude.Text, otp :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LinkedSocialAccount = LinkedSocialAccount {email :: Kernel.Prelude.Maybe Kernel.Prelude.Text, isPrivateEmail :: Kernel.Prelude.Bool, oauthProvider :: OAuthProvider, validTill :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LinkedSocialAccountsRes = LinkedSocialAccountsRes {accounts :: [LinkedSocialAccount]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OAuthProvider
  = Google
  | IOS
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialLinkReq = SocialLinkReq {authorizationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, nonce :: Kernel.Prelude.Maybe Kernel.Prelude.Text, oauthProvider :: OAuthProvider, tokenId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialLoginReq = SocialLoginReq
  { email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enableOtpLessRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    oauthProvider :: OAuthProvider,
    registrationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    registrationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    tokenId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialLoginRes = SocialLoginRes {isNew :: Kernel.Prelude.Bool, token :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialLoginStatus
  = LOGGED_IN
  | LINK_REQUIRED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialLoginV2Req = SocialLoginV2Req
  { authorizationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    nonce :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    oauthProvider :: OAuthProvider,
    tokenId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialLoginV2Res = SocialLoginV2Res {linkToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text, status :: SocialLoginStatus, token :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialUnlinkReq = SocialUnlinkReq {oauthProvider :: OAuthProvider}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SocialUpdateProfileReq = SocialUpdateProfileReq
  { email :: Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
