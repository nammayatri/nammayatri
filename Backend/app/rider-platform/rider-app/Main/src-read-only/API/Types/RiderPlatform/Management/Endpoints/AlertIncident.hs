{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.AlertIncident where

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

data AlertIncidentInfo = AlertIncidentInfo
  { alertGroup :: Kernel.Prelude.Maybe Data.Text.Text,
    alertName :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Data.Text.Text,
    downtimeSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    externalURL :: Kernel.Prelude.Maybe Data.Text.Text,
    firingTime :: Kernel.Prelude.UTCTime,
    id :: Data.Text.Text,
    isManuallyEntered :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rca :: Kernel.Prelude.Maybe Data.Text.Text,
    receiver :: Kernel.Prelude.Maybe Data.Text.Text,
    resolvedTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceName :: Data.Text.Text,
    severity :: Kernel.Prelude.Maybe Data.Text.Text,
    status :: IncidentStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlertIncidentsResponse = AlertIncidentsResponse {incidents :: [AlertIncidentInfo], totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncidentStatus
  = FIRING
  | RESOLVED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("alertIncident" :> GetAlertIncidentAlertsIncidents)

type GetAlertIncidentAlertsIncidents = ("alerts" :> "incidents" :> QueryParam "fromTime" Kernel.Prelude.UTCTime :> QueryParam "toTime" Kernel.Prelude.UTCTime :> Get '[JSON] AlertIncidentsResponse)

newtype AlertIncidentAPIs = AlertIncidentAPIs {getAlertIncidentAlertsIncidents :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient AlertIncidentsResponse}

mkAlertIncidentAPIs :: (Client EulerHS.Types.EulerClient API -> AlertIncidentAPIs)
mkAlertIncidentAPIs alertIncidentClient = (AlertIncidentAPIs {..})
  where
    getAlertIncidentAlertsIncidents = alertIncidentClient

data AlertIncidentUserActionType
  = GET_ALERT_INCIDENT_ALERTS_INCIDENTS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON AlertIncidentUserActionType where
  toJSON GET_ALERT_INCIDENT_ALERTS_INCIDENTS = Data.Aeson.String "GET_ALERT_INCIDENT_ALERTS_INCIDENTS"

instance FromJSON AlertIncidentUserActionType where
  parseJSON (Data.Aeson.String "GET_ALERT_INCIDENT_ALERTS_INCIDENTS") = pure GET_ALERT_INCIDENT_ALERTS_INCIDENTS
  parseJSON _ = fail "GET_ALERT_INCIDENT_ALERTS_INCIDENTS expected"

$(Data.Singletons.TH.genSingletons [''AlertIncidentUserActionType])
