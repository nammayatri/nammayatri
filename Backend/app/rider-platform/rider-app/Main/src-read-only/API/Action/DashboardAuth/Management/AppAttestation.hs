{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.AppAttestation
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.AppAttestation
import qualified AppAttestation.Admin
import qualified Control.Lens
import qualified Domain.Action.Dashboard.AppAttestation
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("appAttestation" :> (GetAppAttestationConfig :<|> PostAppAttestationConfig :<|> PostAppAttestationIosKeyRevoke))

type GetAppAttestationConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/APP_ATTESTATION/GET_APP_ATTESTATION_CONFIG"
      :> API.Types.RiderPlatform.Management.AppAttestation.GetAppAttestationConfig
  )

type PostAppAttestationConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/APP_ATTESTATION/POST_APP_ATTESTATION_CONFIG"
      :> API.Types.RiderPlatform.Management.AppAttestation.PostAppAttestationConfig
  )

type PostAppAttestationIosKeyRevoke =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/APP_ATTESTATION/POST_APP_ATTESTATION_IOS_KEY_REVOKE"
      :> API.Types.RiderPlatform.Management.AppAttestation.PostAppAttestationIosKeyRevoke
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAppAttestationConfig merchantId city :<|> postAppAttestationConfig merchantId city :<|> postAppAttestationIosKeyRevoke merchantId city

getAppAttestationConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler AppAttestation.Admin.AttestationConfigRes)
getAppAttestationConfig a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a1 a3 a2
        Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.AppAttestation.getAppAttestationConfig a3 a2
    )

postAppAttestationConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> AppAttestation.Admin.AttestationConfigUpdateReq -> Environment.FlowHandler AppAttestation.Admin.AttestationConfigRes)
postAppAttestationConfig a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/APP_ATTESTATION/POST_APP_ATTESTATION_CONFIG" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppAttestation.postAppAttestationConfig a4 a3 a1
    )

postAppAttestationIosKeyRevoke :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> AppAttestation.Admin.RevokeIosKeyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAppAttestationIosKeyRevoke a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/APP_ATTESTATION/POST_APP_ATTESTATION_IOS_KEY_REVOKE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppAttestation.postAppAttestationIosKeyRevoke a4 a3 a1
    )
