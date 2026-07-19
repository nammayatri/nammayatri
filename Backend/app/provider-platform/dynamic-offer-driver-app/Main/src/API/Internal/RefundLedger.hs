module API.Internal.RefundLedger
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.RefundLedger as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "rideId" (Id Ride)
    :> "refundLedger"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.RefundLedgerReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  refundLedger

refundLedger :: Id Ride -> Maybe Text -> Domain.RefundLedgerReq -> FlowHandler APISuccess
refundLedger rideId apiKey req = withFlowHandlerAPI $ Domain.refundLedger rideId req apiKey
