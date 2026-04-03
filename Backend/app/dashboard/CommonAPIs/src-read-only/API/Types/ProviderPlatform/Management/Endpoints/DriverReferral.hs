{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.ProviderPlatform.Management.Endpoints.DriverReferral where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Kernel.Types.HideSecrets
import qualified Data.Singletons.TH
import qualified Kernel.ServantMultipart
import qualified Data.ByteString.Lazy



data FailureReasons
    = FailureReasons {driverId :: Kernel.Prelude.Text, failureReason :: Kernel.Prelude.Text}
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data LinkReport
    = LinkReport {successFullyLinked :: Kernel.Prelude.Int, failures :: [FailureReasons]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
newtype ReferralLinkPasswordUpdateAPIReq
  = ReferralLinkPasswordUpdateAPIReq {referralLinkPassword :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets ReferralLinkPasswordUpdateAPIReq
    where hideSecrets = Kernel.Prelude.identity
newtype ReferralLinkReq
  = ReferralLinkReq {file :: Kernel.Prelude.FilePath}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets ReferralLinkReq
    where hideSecrets = Kernel.Prelude.identity
type API = ("driverReferral" :> (PostDriverReferralReferralOpsPassword :<|> PostDriverReferralLinkReferral))
type PostDriverReferralReferralOpsPassword = ("referralOpsPassword" :> ReqBody ('[JSON]) ReferralLinkPasswordUpdateAPIReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
type PostDriverReferralLinkReferral = ("linkReferral" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp ReferralLinkReq :> Post ('[JSON]) LinkReport)
data DriverReferralAPIs
    = DriverReferralAPIs {postDriverReferralReferralOpsPassword :: (ReferralLinkPasswordUpdateAPIReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
                          postDriverReferralLinkReferral :: ((Data.ByteString.Lazy.ByteString, ReferralLinkReq) -> EulerHS.Types.EulerClient LinkReport)}
mkDriverReferralAPIs :: (Client EulerHS.Types.EulerClient API -> DriverReferralAPIs)
mkDriverReferralAPIs driverReferralClient = (DriverReferralAPIs {..})
                         where postDriverReferralReferralOpsPassword :<|> postDriverReferralLinkReferral = driverReferralClient
data DriverReferralUserActionType
    = POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD | POST_DRIVER_REFERRAL_LINK_REFERRAL
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DriverReferralUserActionType)])

