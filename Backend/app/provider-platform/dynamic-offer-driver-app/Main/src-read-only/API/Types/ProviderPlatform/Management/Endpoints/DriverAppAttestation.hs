{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.DriverAppAttestation where

import qualified AppAttestation.Admin
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("driverAppAttestation" :> (GetDriverAppAttestationConfig :<|> PostDriverAppAttestationConfig :<|> PostDriverAppAttestationIosKeyRevoke))

type GetDriverAppAttestationConfig = ("config" :> Get ('[JSON]) AppAttestation.Admin.AttestationConfigRes)

type PostDriverAppAttestationConfig = ("config" :> ReqBody ('[JSON]) AppAttestation.Admin.AttestationConfigUpdateReq :> Post ('[JSON]) AppAttestation.Admin.AttestationConfigRes)

type PostDriverAppAttestationIosKeyRevoke = ("iosKey" :> "revoke" :> ReqBody ('[JSON]) AppAttestation.Admin.RevokeIosKeyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data DriverAppAttestationAPIs = DriverAppAttestationAPIs
  { getDriverAppAttestationConfig :: (EulerHS.Types.EulerClient AppAttestation.Admin.AttestationConfigRes),
    postDriverAppAttestationConfig :: (AppAttestation.Admin.AttestationConfigUpdateReq -> EulerHS.Types.EulerClient AppAttestation.Admin.AttestationConfigRes),
    postDriverAppAttestationIosKeyRevoke :: (AppAttestation.Admin.RevokeIosKeyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkDriverAppAttestationAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAppAttestationAPIs)
mkDriverAppAttestationAPIs driverAppAttestationClient = (DriverAppAttestationAPIs {..})
  where
    getDriverAppAttestationConfig :<|> postDriverAppAttestationConfig :<|> postDriverAppAttestationIosKeyRevoke = driverAppAttestationClient

data DriverAppAttestationUserActionType
  = GET_DRIVER_APP_ATTESTATION_CONFIG
  | POST_DRIVER_APP_ATTESTATION_CONFIG
  | POST_DRIVER_APP_ATTESTATION_IOS_KEY_REVOKE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DriverAppAttestationUserActionType)])
