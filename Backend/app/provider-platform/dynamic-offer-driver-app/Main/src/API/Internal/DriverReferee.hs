module API.Internal.DriverReferee
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverReferee as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id Merchant)
    :> ( "referee"
           :> Header "token" Text
           :> ReqBody '[JSON] Domain.RefereeLinkInfoReq
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  linkReferee

linkReferee :: Id Merchant -> Maybe Text -> Domain.RefereeLinkInfoReq -> FlowHandler APISuccess
linkReferee merchantId apiKey = withFlowHandlerAPI . Domain.linkReferee merchantId apiKey
