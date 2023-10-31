module API.UI.City where

import qualified Domain.Action.UI.City as DAUC
import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Utils.Error.FlowHandling (withFlowHandlerAPI)
import Servant

type API =
  "city"
    :> ReqBody '[JSON] DAUC.GetCityReq
    :> Post '[JSON] DAUC.GetCityResp

handler :: FlowServer API
handler = getCity

getCity :: DAUC.GetCityReq -> FlowHandler DAUC.GetCityResp
getCity = withFlowHandlerAPI . DAUC.getCity
