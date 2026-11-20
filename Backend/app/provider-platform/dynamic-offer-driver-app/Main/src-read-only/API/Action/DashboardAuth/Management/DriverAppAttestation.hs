{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DriverAppAttestation
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverAppAttestation
import qualified AppAttestation.Admin
import qualified Control.Lens
import qualified Domain.Action.Dashboard.Management.DriverAppAttestation
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

type API = ("driverAppAttestation" :> (GetDriverAppAttestationConfig :<|> PostDriverAppAttestationConfig :<|> PostDriverAppAttestationIosKeyRevoke))

type GetDriverAppAttestationConfig =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/GET_DRIVER_APP_ATTESTATION_CONFIG"
      :> API.Types.ProviderPlatform.Management.DriverAppAttestation.GetDriverAppAttestationConfig
  )

type PostDriverAppAttestationConfig =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/POST_DRIVER_APP_ATTESTATION_CONFIG"
      :> API.Types.ProviderPlatform.Management.DriverAppAttestation.PostDriverAppAttestationConfig
  )

type PostDriverAppAttestationIosKeyRevoke =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/POST_DRIVER_APP_ATTESTATION_IOS_KEY_REVOKE"
      :> API.Types.ProviderPlatform.Management.DriverAppAttestation.PostDriverAppAttestationIosKeyRevoke
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverAppAttestationConfig merchantId city :<|> postDriverAppAttestationConfig merchantId city :<|> postDriverAppAttestationIosKeyRevoke merchantId city

getDriverAppAttestationConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler AppAttestation.Admin.AttestationConfigRes)
getDriverAppAttestationConfig a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a1 a3 a2
        Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.Management.DriverAppAttestation.getDriverAppAttestationConfig a3 a2
    )

postDriverAppAttestationConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> AppAttestation.Admin.AttestationConfigUpdateReq -> Environment.FlowHandler AppAttestation.Admin.AttestationConfigRes)
postDriverAppAttestationConfig a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/POST_DRIVER_APP_ATTESTATION_CONFIG" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.DriverAppAttestation.postDriverAppAttestationConfig a4 a3 a1
    )

postDriverAppAttestationIosKeyRevoke :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> AppAttestation.Admin.RevokeIosKeyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAppAttestationIosKeyRevoke a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/POST_DRIVER_APP_ATTESTATION_IOS_KEY_REVOKE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.DriverAppAttestation.postDriverAppAttestationIosKeyRevoke a4 a3 a1
    )
