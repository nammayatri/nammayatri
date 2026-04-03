{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.ProviderPlatform.Management.DriverReferral 
( API,
handler )
where
import EulerHS.Prelude hiding (sortOn)
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common hiding (INFO)
import Storage.Beam.CommonInstances ()
import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified API.Types.ProviderPlatform.Management
import qualified Domain.Action.ProviderPlatform.Management.DriverReferral
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.APISuccess
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("driverReferral" :> (PostDriverReferralReferralOpsPassword :<|> PostDriverReferralLinkReferral))
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverReferralReferralOpsPassword merchantId city :<|> postDriverReferralLinkReferral merchantId city
type PostDriverReferralReferralOpsPassword = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                                      ('DSL)
                                                      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DRIVER_REFERRAL) / ('API.Types.ProviderPlatform.Management.DriverReferral.POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD)) :> API.Types.ProviderPlatform.Management.DriverReferral.PostDriverReferralReferralOpsPassword)
type PostDriverReferralLinkReferral = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                               ('DSL)
                                               (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DRIVER_REFERRAL) / ('API.Types.ProviderPlatform.Management.DriverReferral.POST_DRIVER_REFERRAL_LINK_REFERRAL)) :> API.Types.ProviderPlatform.Management.DriverReferral.PostDriverReferralLinkReferral)
postDriverReferralReferralOpsPassword :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverReferralReferralOpsPassword merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverReferral.postDriverReferralReferralOpsPassword merchantShortId opCity apiTokenInfo req
postDriverReferralLinkReferral :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverReferral.LinkReport)
postDriverReferralLinkReferral merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverReferral.postDriverReferralLinkReferral merchantShortId opCity apiTokenInfo req



