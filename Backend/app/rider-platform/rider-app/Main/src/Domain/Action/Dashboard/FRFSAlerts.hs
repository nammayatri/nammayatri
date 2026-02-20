{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.FRFSAlerts (getFRFSAlertsFrfsLiveMetrics) where

import qualified API.Types.RiderPlatform.Management.FRFSAlerts as Common
import Data.Maybe (maybeToList)
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Action.UI.GetFRFSLiveAlerts as LiveMetrics
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Auth
import Tools.Error

getFRFSAlertsFrfsLiveMetrics ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Context.City ->
  Maybe Kernel.Prelude.UTCTime ->
  Maybe Kernel.Prelude.UTCTime ->
  Maybe Data.Text.Text ->
  Environment.Flow Common.LiveMetricsResponse
getFRFSAlertsFrfsLiveMetrics merchantShortId opCity mbFrom mbTo mbModes = do
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)

  let modes = maybeToList mbModes -- Convert single mode to list

  -- Call the existing UI logic
  liveMetrics <- LiveMetrics.getLiveMetrics merchantOpCity.id mbFrom mbTo modes

  -- Convert from UI response to Dashboard response
  pure $
    Common.LiveMetricsResponse
      { hourlyBookings = map convertHourlyBooking (LiveMetrics.hourlyBookings liveMetrics),
        liveStatus = map convertModeStatus (LiveMetrics.liveStatus liveMetrics),
        downtimeInfo = map convertDowntimeInfo (LiveMetrics.downtimeInfo liveMetrics)
      }
  where
    convertHourlyBooking hb =
      Common.HourlyBooking
        { time = LiveMetrics.time hb,
          mode = LiveMetrics.mode (hb :: LiveMetrics.HourlyBooking),
          count = LiveMetrics.count hb
        }

    convertModeStatus ms =
      Common.ModeStatus
        { mode = LiveMetrics.mode (ms :: LiveMetrics.ModeStatus),
          status = LiveMetrics.status ms
        }

    convertDowntimeInfo di =
      Common.VehicleDowntimeInfo
        { vehicleType = LiveMetrics.vehicleType di,
          lastOutageStartTime = LiveMetrics.lastOutageStartTime di,
          lastRecoveryTime = LiveMetrics.lastRecoveryTime di,
          isCurrentlyUp = LiveMetrics.isCurrentlyUp di
        }
