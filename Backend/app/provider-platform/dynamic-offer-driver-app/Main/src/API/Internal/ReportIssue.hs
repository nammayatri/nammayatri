module API.Internal.ReportIssue
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.ReportIssue as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as Common
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "rideId" (Id Ride)
    :> "reportIssue"
    :> MandatoryQueryParam "issueType" Common.IssueReportType
    :> Header "token" Text
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  reportIssue

reportIssue :: Id Ride -> Common.IssueReportType -> Maybe Text -> FlowHandler APISuccess
reportIssue rideId issueReportType = withFlowHandlerAPI . Domain.reportIssue rideId issueReportType
