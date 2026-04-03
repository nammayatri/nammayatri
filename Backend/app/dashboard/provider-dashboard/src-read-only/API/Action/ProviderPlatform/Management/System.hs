{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.ProviderPlatform.Management.System 
( API,
handler )
where
import EulerHS.Prelude hiding (sortOn)
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common hiding (INFO)
import Storage.Beam.CommonInstances ()
import qualified API.Types.ProviderPlatform.Management.System
import qualified API.Types.ProviderPlatform.Management
import qualified Domain.Action.ProviderPlatform.Management.System
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.APISuccess
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("system" :> PostSystemRunQuery)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSystemRunQuery merchantId city
type PostSystemRunQuery = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                   ('DSL)
                                   (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SYSTEM) / ('API.Types.ProviderPlatform.Management.System.POST_SYSTEM_RUN_QUERY)) :> API.Types.ProviderPlatform.Management.System.PostSystemRunQuery)
postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.System.QueryData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.System.postSystemRunQuery merchantShortId opCity apiTokenInfo req



