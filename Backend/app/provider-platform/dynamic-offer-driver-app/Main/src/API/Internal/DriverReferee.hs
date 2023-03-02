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
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> ( "referee"
           :> SignatureAuth "Authorization"
           :> ReqBody '[JSON] Domain.RefereeLinkInfoReq
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  linkReferee

linkReferee :: Id Merchant -> SignatureAuthResult -> Domain.RefereeLinkInfoReq -> FlowHandler APISuccess
linkReferee merchantId sa = withFlowHandlerAPI . Domain.linkReferee merchantId sa
