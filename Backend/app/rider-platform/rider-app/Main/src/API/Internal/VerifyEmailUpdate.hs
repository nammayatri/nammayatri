module API.Internal.VerifyEmailUpdate
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.VerifyEmailUpdate as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "verifyEmailUpdate"
    :> Capture "merchantShortId" Text
    :> Header "api-key" Text
    :> ReqBody '[JSON] Domain.VerifyEmailUpdateReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = verifyEmailUpdate

verifyEmailUpdate :: Text -> Maybe Text -> Domain.VerifyEmailUpdateReq -> FlowHandler APISuccess
verifyEmailUpdate merchantShortId apiKey = withFlowHandlerAPI . Domain.verifyEmailUpdate apiKey merchantShortId
