{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.FRFSAlerts
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.FRFSAlerts
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.Management.FRFSAlerts
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("fRFSAlerts" :> GetFRFSAlertsFrfsLiveMetrics)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSAlertsFrfsLiveMetrics merchantId city

type GetFRFSAlertsFrfsLiveMetrics =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.FRFS_ALERTS) / ('API.Types.RiderPlatform.Management.FRFSAlerts.GET_FRFS_ALERTS_FRFS_LIVE_METRICS))
      :> API.Types.RiderPlatform.Management.FRFSAlerts.GetFRFSAlertsFrfsLiveMetrics
  )

getFRFSAlertsFrfsLiveMetrics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSAlerts.LiveMetricsResponse)
getFRFSAlertsFrfsLiveMetrics merchantShortId opCity apiTokenInfo from to modes = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSAlerts.getFRFSAlertsFrfsLiveMetrics merchantShortId opCity apiTokenInfo from to modes
