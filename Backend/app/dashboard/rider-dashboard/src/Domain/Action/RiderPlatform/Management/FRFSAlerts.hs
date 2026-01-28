{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.FRFSAlerts (getFRFSAlertsFrfsLiveMetrics) where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.FRFSAlerts
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getFRFSAlertsFrfsLiveMetrics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Environment.Flow API.Types.RiderPlatform.Management.FRFSAlerts.LiveMetricsResponse)
getFRFSAlertsFrfsLiveMetrics merchantShortId opCity apiTokenInfo from to modes = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSAlertsDSL.getFRFSAlertsFrfsLiveMetrics) from to modes
