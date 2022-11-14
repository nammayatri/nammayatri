module API.UI.Webengage
  ( API,
    handler,
    WE.WebengageRes (..),
  )
where

import Beckn.Utils.Common
import qualified Domain.Action.UI.Webengage as WE
import Environment
import EulerHS.Prelude hiding (id)
import Servant

type API =
  "webengage"
    :> "1"
    :> "customer"
    :> ReqBody '[JSON] WE.WebengageReq
    :> Post '[JSON] WE.WebengageRes

handler :: WE.WebengageReq -> FlowHandler WE.WebengageRes
handler = sendSms

sendSms :: WE.WebengageReq -> FlowHandler WE.WebengageRes
sendSms = withFlowHandlerAPI . WE.callInfobip
