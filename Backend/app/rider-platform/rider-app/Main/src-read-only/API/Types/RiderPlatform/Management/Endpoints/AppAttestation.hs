{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.AppAttestation where

import qualified AppAttestation.Admin
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("appAttestation" :> (GetAppAttestationConfig :<|> PostAppAttestationConfig :<|> PostAppAttestationIosKeyRevoke))

type GetAppAttestationConfig = ("config" :> Get ('[JSON]) AppAttestation.Admin.AttestationConfigRes)

type PostAppAttestationConfig = ("config" :> ReqBody ('[JSON]) AppAttestation.Admin.AttestationConfigUpdateReq :> Post ('[JSON]) AppAttestation.Admin.AttestationConfigRes)

type PostAppAttestationIosKeyRevoke = ("iosKey" :> "revoke" :> ReqBody ('[JSON]) AppAttestation.Admin.RevokeIosKeyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data AppAttestationAPIs = AppAttestationAPIs
  { getAppAttestationConfig :: (EulerHS.Types.EulerClient AppAttestation.Admin.AttestationConfigRes),
    postAppAttestationConfig :: (AppAttestation.Admin.AttestationConfigUpdateReq -> EulerHS.Types.EulerClient AppAttestation.Admin.AttestationConfigRes),
    postAppAttestationIosKeyRevoke :: (AppAttestation.Admin.RevokeIosKeyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkAppAttestationAPIs :: (Client EulerHS.Types.EulerClient API -> AppAttestationAPIs)
mkAppAttestationAPIs appAttestationClient = (AppAttestationAPIs {..})
  where
    getAppAttestationConfig :<|> postAppAttestationConfig :<|> postAppAttestationIosKeyRevoke = appAttestationClient

data AppAttestationUserActionType
  = GET_APP_ATTESTATION_CONFIG
  | POST_APP_ATTESTATION_CONFIG
  | POST_APP_ATTESTATION_IOS_KEY_REVOKE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''AppAttestationUserActionType)])
