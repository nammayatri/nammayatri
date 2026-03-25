module API.Internal.UpdateCancellationFeeStatus where

import qualified Domain.Action.Internal.UpdateCancellationFeeStatus as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "updateCancellationFeeStatus"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.UpdateCancellationFeeStatusReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = updateCancellationFeeStatus

updateCancellationFeeStatus :: Maybe Text -> Domain.UpdateCancellationFeeStatusReq -> FlowHandler APISuccess
updateCancellationFeeStatus token req = withFlowHandlerAPI $ Domain.updateCancellationFeeStatus token req
