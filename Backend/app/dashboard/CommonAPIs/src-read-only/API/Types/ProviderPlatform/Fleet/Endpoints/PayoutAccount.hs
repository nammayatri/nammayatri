{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.PayoutAccount where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.PaymentMode
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data PayoutAccountReq = PayoutAccountReq
  { accountType :: PayoutAccountType,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.PaymentMode.PaymentMode,
    bankAccountNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankIfscCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PayoutAccountReq where
  hideSecrets = Kernel.Prelude.identity

data PayoutAccountResp = PayoutAccountResp
  { accountType :: PayoutAccountType,
    bankVerifyResp :: Kernel.Prelude.Maybe Kernel.External.Verification.Interface.Types.VerifyAsyncResp,
    bankInfoResp :: Kernel.Prelude.Maybe Kernel.External.Verification.Types.BankAccountVerificationResponse,
    upiRegistrationResp :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.DriverRegistration.PayoutRegistrationRes,
    upiStatusResp :: Kernel.Prelude.Maybe Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutAccountType
  = BANK
  | UPI
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("fleet" :> PostPayoutAccountPayoutAccountHelper)

type PostPayoutAccount = ("payout" :> "account" :> ReqBody ('[JSON]) PayoutAccountReq :> Post ('[JSON]) PayoutAccountResp)

type PostPayoutAccountPayoutAccountHelper = ("payout" :> "account" :> Capture "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) PayoutAccountReq :> Post ('[JSON]) PayoutAccountResp)

newtype PayoutAccountAPIs = PayoutAccountAPIs {postPayoutAccount :: (Kernel.Prelude.Text -> PayoutAccountReq -> EulerHS.Types.EulerClient PayoutAccountResp)}

mkPayoutAccountAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAccountAPIs)
mkPayoutAccountAPIs payoutAccountClient = (PayoutAccountAPIs {..})
  where
    postPayoutAccount = payoutAccountClient

data PayoutAccountUserActionType
  = POST_PAYOUT_ACCOUNT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON PayoutAccountUserActionType where
  toJSON (POST_PAYOUT_ACCOUNT) = Data.Aeson.String "POST_PAYOUT_ACCOUNT"

instance FromJSON PayoutAccountUserActionType where
  parseJSON (Data.Aeson.String "POST_PAYOUT_ACCOUNT") = pure POST_PAYOUT_ACCOUNT
  parseJSON _ = fail "POST_PAYOUT_ACCOUNT expected"

$(Data.Singletons.TH.genSingletons [(''PayoutAccountUserActionType)])
