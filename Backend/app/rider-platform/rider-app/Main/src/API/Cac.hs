module API.Cac where

import qualified Domain.Action.Cac as Domain
import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

type API = "typeCheck" :> ReqBody '[JSON] Domain.CacTypeValidationReq :> Post '[JSON] Domain.CacTypeValidationResp

handler :: FlowServer API
handler = typeCheckHandler

typeCheckHandler :: Domain.CacTypeValidationReq -> FlowHandler Domain.CacTypeValidationResp
typeCheckHandler = withFlowHandlerAPI . Domain.typeCheckHandler
