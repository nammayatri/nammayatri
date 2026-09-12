{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.FRFSAlerts
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSAlerts
import qualified Data.Text
import qualified Domain.Action.Dashboard.FRFSAlerts
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("fRFSAlerts" :> GetFRFSAlertsFrfsLiveMetrics)

type GetFRFSAlertsFrfsLiveMetrics =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_ALERTS/GET_FRFS_ALERTS_FRFS_LIVE_METRICS"
      :> API.Types.RiderPlatform.Management.FRFSAlerts.GetFRFSAlertsFrfsLiveMetrics
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSAlertsFrfsLiveMetrics merchantId city

getFRFSAlertsFrfsLiveMetrics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSAlerts.LiveMetricsResponse)
getFRFSAlertsFrfsLiveMetrics a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSAlerts.getFRFSAlertsFrfsLiveMetrics a6 a5 a3 a2 a1
