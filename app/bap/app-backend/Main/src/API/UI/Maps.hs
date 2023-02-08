module API.UI.Maps
  ( API,
    handler,
    DMaps.AutoCompleteReq,
    DMaps.AutoCompleteResp,
    DMaps.GetPlaceDetailsReq,
    DMaps.GetPlaceDetailsResp,
    DMaps.GetPlaceNameReq,
    DMaps.GetPlaceNameResp,
  )
where

import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Logging
import Servant
import Tools.Auth

type API =
  "maps"
    :> ( "autoComplete"
           :> TokenAuth
           :> ReqBody '[JSON] DMaps.AutoCompleteReq
           :> Post '[JSON] DMaps.AutoCompleteResp
           :<|> "getPlaceDetails"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.GetPlaceDetailsReq
             :> Post '[JSON] DMaps.GetPlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.GetPlaceNameReq
             :> Post '[JSON] DMaps.GetPlaceNameResp
       )

handler :: FlowServer API
handler =
  autoComplete
    :<|> getPlaceDetails
    :<|> getPlaceName

autoComplete :: Id Person.Person -> DMaps.AutoCompleteReq -> FlowHandler DMaps.AutoCompleteResp
autoComplete personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.autoComplete personId

getPlaceDetails :: Id Person.Person -> DMaps.GetPlaceDetailsReq -> FlowHandler DMaps.GetPlaceDetailsResp
getPlaceDetails personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceDetails personId

getPlaceName :: Id Person.Person -> DMaps.GetPlaceNameReq -> FlowHandler DMaps.GetPlaceNameResp
getPlaceName personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceName personId
