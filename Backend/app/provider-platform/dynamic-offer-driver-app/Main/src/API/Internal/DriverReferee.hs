module API.Internal.DriverReferee
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverReferee as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.RefereeLink as DRL
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id Merchant)
    :> ( "referee"
           :> Header "token" Text
           :> ReqBody '[JSON] Domain.RefereeLinkInfoReq
           :> Post '[JSON] DRL.LinkRefereeRes
       )

handler :: FlowServer API
handler =
  linkReferee

linkReferee :: Id Merchant -> Maybe Text -> Domain.RefereeLinkInfoReq -> FlowHandler DRL.LinkRefereeRes
linkReferee merchantId apiKey = withFlowHandlerAPI . Domain.linkReferee merchantId apiKey
