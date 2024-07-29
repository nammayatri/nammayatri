{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.DriverReferral
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified Domain.Action.ProviderPlatform.Management.DriverReferral
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driverReferral" :> (PostDriverReferralReferralOpsPassword :<|> PostDriverReferralLinkReferral))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverReferralReferralOpsPassword merchantId city :<|> postDriverReferralLinkReferral merchantId city

type PostDriverReferralReferralOpsPassword =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'REFERRAL
      'REFERRAL_PROGRAM_PASSWORD_UPDATE
      :> API.Types.ProviderPlatform.Management.DriverReferral.PostDriverReferralReferralOpsPassword
  )

type PostDriverReferralLinkReferral =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'REFERRAL
      'REFERRAL_PROGRAM_LINK_CODE
      :> API.Types.ProviderPlatform.Management.DriverReferral.PostDriverReferralLinkReferral
  )

postDriverReferralReferralOpsPassword :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverReferralReferralOpsPassword merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverReferral.postDriverReferralReferralOpsPassword merchantShortId opCity apiTokenInfo req

postDriverReferralLinkReferral :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverReferral.LinkReport)
postDriverReferralLinkReferral merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverReferral.postDriverReferralLinkReferral merchantShortId opCity apiTokenInfo req
