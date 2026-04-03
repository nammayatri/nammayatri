{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.RiderPlatform.Management.System 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified API.Types.RiderPlatform.Management.System
import qualified API.Types.RiderPlatform.Management
import qualified Domain.Action.RiderPlatform.Management.System
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.APISuccess
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("system" :> PostSystemRunQuery)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSystemRunQuery merchantId city
type PostSystemRunQuery = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                   ('DSL)
                                   (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.SYSTEM) / ('API.Types.RiderPlatform.Management.System.POST_SYSTEM_RUN_QUERY)) :> API.Types.RiderPlatform.Management.System.PostSystemRunQuery)
postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.System.QueryData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.System.postSystemRunQuery merchantShortId opCity apiTokenInfo req



