{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Translation
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Translation
import qualified Control.Lens
import qualified Domain.Action.Dashboard.Management.Translation
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = "translation" :> (GetTranslationDomains :<|> GetTranslationKeys)

type GetTranslationDomains =
  DashboardUserAuth
    'DRIVER_OFFER_BPP_MANAGEMENT
    "PROVIDER_MANAGEMENT/TRANSLATION/GET_TRANSLATION_DOMAINS"
    :> API.Types.ProviderPlatform.Management.Translation.GetTranslationDomains

type GetTranslationKeys =
  DashboardUserAuth
    'DRIVER_OFFER_BPP_MANAGEMENT
    "PROVIDER_MANAGEMENT/TRANSLATION/GET_TRANSLATION_KEYS"
    :> API.Types.ProviderPlatform.Management.Translation.GetTranslationKeys

handler :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getTranslationDomains merchantId city :<|> getTranslationKeys merchantId city

getTranslationDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Translation.TranslationDomainsResp))
getTranslationDomains a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a1 a3 a2
        Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.Management.Translation.getTranslationDomains a3 a2
    )

getTranslationKeys :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Translation.TranslationKeysResp)))
getTranslationKeys a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.Translation.getTranslationKeys a4 a3 a1
    )
