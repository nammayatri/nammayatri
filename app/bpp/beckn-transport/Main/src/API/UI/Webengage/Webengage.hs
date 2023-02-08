module API.UI.Webengage.Webengage
  ( API,
    handler,
    WE.WebengageRes (..),
  )
where

import qualified Domain.Action.UI.Webengage.Webengage as WE
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Servant

type API =
  "webengage"
    :> "2"
    :> "transporter"
    :> ReqBody '[JSON] WE.WebengageReq
    :> Post '[JSON] WE.WebengageRes

handler :: WE.WebengageReq -> FlowHandler WE.WebengageRes
handler = sendSms

sendSms :: WE.WebengageReq -> FlowHandler WE.WebengageRes
sendSms = withFlowHandlerAPI . WE.callInfobip
