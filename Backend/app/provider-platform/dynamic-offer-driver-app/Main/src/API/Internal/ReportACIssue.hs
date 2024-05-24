module API.Internal.ReportACIssue
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.ReportACIssue as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

-- Deprecated (Replaced By ReportIssue API)
type API =
  Capture "rideId" (Id Ride)
    :> "reportACIssue"
    :> Header "token" Text
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  reportACIssue

reportACIssue :: Id Ride -> Maybe Text -> FlowHandler APISuccess
reportACIssue rideId = withFlowHandlerAPI . Domain.reportACIssue rideId
