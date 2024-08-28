{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.DriverReferral where

import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data FailureReasons = FailureReasons {driverId :: Kernel.Prelude.Text, failureReason :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LinkReport = LinkReport {successFullyLinked :: Kernel.Prelude.Int, failures :: [API.Types.ProviderPlatform.Management.DriverReferral.FailureReasons]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ReferralLinkPasswordUpdateAPIReq = ReferralLinkPasswordUpdateAPIReq {referralLinkPassword :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ReferralLinkPasswordUpdateAPIReq where
  hideSecrets = Kernel.Prelude.identity

newtype ReferralLinkReq = ReferralLinkReq {file :: Kernel.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ReferralLinkReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("driverReferral" :> (PostDriverReferralReferralOpsPassword :<|> PostDriverReferralLinkReferral))

type PostDriverReferralReferralOpsPassword =
  ( "referralOpsPassword" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkPasswordUpdateAPIReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverReferralLinkReferral =
  ( "linkReferral"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkReq
      :> Post '[JSON] API.Types.ProviderPlatform.Management.DriverReferral.LinkReport
  )

data DriverReferralAPIs = DriverReferralAPIs
  { postDriverReferralReferralOpsPassword :: API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkPasswordUpdateAPIReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverReferralLinkReferral ::
      ( Data.ByteString.Lazy.ByteString,
        API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkReq
      ) ->
      EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.DriverReferral.LinkReport
  }

mkDriverReferralAPIs :: (Client EulerHS.Types.EulerClient API -> DriverReferralAPIs)
mkDriverReferralAPIs driverReferralClient = (DriverReferralAPIs {..})
  where
    postDriverReferralReferralOpsPassword :<|> postDriverReferralLinkReferral = driverReferralClient

data DriverReferralEndpointDSL
  = PostDriverReferralReferralOpsPasswordEndpoint
  | PostDriverReferralLinkReferralEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
