{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.FRFSAlerts 
( API.Types.RiderPlatform.Management.FRFSAlerts.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.FRFSAlerts
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Data.Text
import qualified API.Types.RiderPlatform.Management.FRFSAlerts



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.FRFSAlerts.API)
handler merchantId city = getFRFSAlertsFrfsLiveMetrics merchantId city
getFRFSAlertsFrfsLiveMetrics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSAlerts.LiveMetricsResponse)
getFRFSAlertsFrfsLiveMetrics a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSAlerts.getFRFSAlertsFrfsLiveMetrics a5 a4 a3 a2 a1



