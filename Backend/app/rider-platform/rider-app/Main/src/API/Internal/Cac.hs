-- | Kept after haskell-cac removal for backward compatibility: older CAC
-- tooling may still call /internal/typeCheck; dropping the route would 404.
-- Auth and real type validation are gone — always succeed.
module API.Internal.Cac where

import qualified Domain.Action.Internal.Cac as Domain
import Environment (FlowHandler, FlowServer)
import qualified Environment as Fields
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

type API =
  "typeCheck"
    :> Header "Authorization" String
    :> ReqBody '[JSON] Domain.CacTypeValidationReq
    :> Post '[JSON] Domain.CacTypeValidationResp

handler :: FlowServer API
handler = typeCheckHandler

typeCheckHandler :: Maybe String -> Domain.CacTypeValidationReq -> FlowHandler Domain.CacTypeValidationResp
typeCheckHandler _token req = withFlowHandlerAPI $ Domain.typeCheckHandler req
