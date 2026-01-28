{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.FRFSAlerts where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

data HourlyBooking = HourlyBooking {count :: Kernel.Prelude.Int, mode :: Data.Text.Text, time :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveMetricsResponse = LiveMetricsResponse {downtimeInfo :: [VehicleDowntimeInfo], hourlyBookings :: [HourlyBooking], liveStatus :: [ModeStatus]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ModeStatus = ModeStatus {mode :: Data.Text.Text, status :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleDowntimeInfo = VehicleDowntimeInfo
  { isCurrentlyUp :: Kernel.Prelude.Int,
    lastOutageStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastRecoveryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleType :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("fRFSAlerts" :> GetFRFSAlertsFrfsLiveMetrics)

type GetFRFSAlertsFrfsLiveMetrics =
  ( "frfs" :> "liveMetrics" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "to" Kernel.Prelude.UTCTime :> QueryParam "modes" Data.Text.Text
      :> Get
           ('[JSON])
           LiveMetricsResponse
  )

newtype FRFSAlertsAPIs = FRFSAlertsAPIs {getFRFSAlertsFrfsLiveMetrics :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> EulerHS.Types.EulerClient LiveMetricsResponse)}

mkFRFSAlertsAPIs :: (Client EulerHS.Types.EulerClient API -> FRFSAlertsAPIs)
mkFRFSAlertsAPIs fRFSAlertsClient = (FRFSAlertsAPIs {..})
  where
    getFRFSAlertsFrfsLiveMetrics = fRFSAlertsClient

data FRFSAlertsUserActionType
  = GET_FRFS_ALERTS_FRFS_LIVE_METRICS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON FRFSAlertsUserActionType where
  toJSON (GET_FRFS_ALERTS_FRFS_LIVE_METRICS) = Data.Aeson.String "GET_FRFS_ALERTS_FRFS_LIVE_METRICS"

instance FromJSON FRFSAlertsUserActionType where
  parseJSON (Data.Aeson.String "GET_FRFS_ALERTS_FRFS_LIVE_METRICS") = pure GET_FRFS_ALERTS_FRFS_LIVE_METRICS
  parseJSON _ = fail "GET_FRFS_ALERTS_FRFS_LIVE_METRICS expected"

$(Data.Singletons.TH.genSingletons [(''FRFSAlertsUserActionType)])
