{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.ProviderPlatform.Fleet.Endpoints.PayoutAccount where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.PaymentMode
import qualified Kernel.External.Verification.Interface.Types
import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Kernel.External.Verification.Types
import qualified Kernel.External.Payout.Interface.Types
import qualified EulerHS.Types
import qualified Kernel.Types.HideSecrets
import qualified Data.Singletons.TH



data PayoutAccountReq
    = PayoutAccountReq {accountType :: PayoutAccountType,
                        fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                        paymentMode :: Kernel.Prelude.Maybe Domain.Types.PaymentMode.PaymentMode,
                        bankAccountNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                        bankIfscCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets PayoutAccountReq
    where hideSecrets = Kernel.Prelude.identity
data PayoutAccountResp
    = PayoutAccountResp {accountType :: PayoutAccountType,
                         bankVerifyResp :: Kernel.Prelude.Maybe Kernel.External.Verification.Interface.Types.VerifyAsyncResp,
                         upiRegistrationResp :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.DriverRegistration.PayoutRegistrationRes}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data PayoutAccountStatusReq
    = PayoutAccountStatusReq {accountType :: PayoutAccountType,
                              fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                              bankRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                              upiOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets PayoutAccountStatusReq
    where hideSecrets = Kernel.Prelude.identity
data PayoutAccountStatusResp
    = PayoutAccountStatusResp {accountType :: PayoutAccountType,
                               bankInfoResp :: Kernel.Prelude.Maybe Kernel.External.Verification.Types.BankAccountVerificationResponse,
                               upiStatusResp :: Kernel.Prelude.Maybe Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data PayoutAccountType
    = BANK | UPI
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("fleet" :> (PostPayoutAccountPayoutAccountHelper :<|> PostPayoutAccountPayoutAccountStatusHelper))
type PostPayoutAccount = ("payout" :> "account" :> ReqBody ('[JSON]) PayoutAccountReq :> Post ('[JSON]) PayoutAccountResp)
type PostPayoutAccountPayoutAccountHelper = ("payout" :> "account" :> Capture "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) PayoutAccountReq :> Post ('[JSON]) PayoutAccountResp)
type PostPayoutAccountStatus = ("payout" :> "account" :> "status" :> ReqBody ('[JSON]) PayoutAccountStatusReq :> Post ('[JSON]) PayoutAccountStatusResp)
type PostPayoutAccountPayoutAccountStatusHelper = ("payout" :> "account" :> "status" :> Capture "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) PayoutAccountStatusReq :> Post ('[JSON])
                                                                                                                                                                                      PayoutAccountStatusResp)
data PayoutAccountAPIs
    = PayoutAccountAPIs {postPayoutAccount :: (Kernel.Prelude.Text -> PayoutAccountReq -> EulerHS.Types.EulerClient PayoutAccountResp),
                         postPayoutAccountStatus :: (Kernel.Prelude.Text -> PayoutAccountStatusReq -> EulerHS.Types.EulerClient PayoutAccountStatusResp)}
mkPayoutAccountAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAccountAPIs)
mkPayoutAccountAPIs payoutAccountClient = (PayoutAccountAPIs {..})
                        where postPayoutAccount :<|> postPayoutAccountStatus = payoutAccountClient
data PayoutAccountUserActionType
    = POST_PAYOUT_ACCOUNT | POST_PAYOUT_ACCOUNT_STATUS
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PayoutAccountUserActionType)])

