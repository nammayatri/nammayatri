module Lib.BmsBackend.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

newtype BmsBackendConfig = BmsBackendConfig {url :: BaseUrl}
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data ReportEventReq = ReportEventReq
  { idempotencyKey :: Text,
    alertType :: Text,
    busId :: Text,
    depotId :: Maybe Text,
    description :: Maybe Text,
    reportedByPersonId :: Maybe Text,
    reportedByName :: Maybe Text,
    -- | Which city/feed this bus belongs to — bms-backend's
    -- TelemetryEvent/Incident gtfs_id. Resolved by the caller from
    -- IntegratedBPPConfig.feedKey; bms-backend has no merchant/city
    -- context of its own to derive it from a bus_id.
    gtfsId :: Maybe Text,
    -- | The bus's current route, if known. Resolved by the caller from the
    -- GIMS current-trip lookup it already makes to identify the reporting
    -- crew's bus — not derived here.
    routeId :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON ReportEventReq where
  toJSON ReportEventReq {..} =
    object
      [ "idempotency_key" .= idempotencyKey,
        "alert_type" .= alertType,
        "bus_id" .= busId,
        "depot_id" .= depotId,
        "description" .= description,
        "reported_by_person_id" .= reportedByPersonId,
        "reported_by_name" .= reportedByName,
        "gtfs_id" .= gtfsId,
        "route_id" .= routeId
      ]

instance FromJSON ReportEventReq where
  parseJSON = withObject "ReportEventReq" $ \o ->
    ReportEventReq
      <$> o .: "idempotency_key"
      <*> o .: "alert_type"
      <*> o .: "bus_id"
      <*> o .:? "depot_id"
      <*> o .:? "description"
      <*> o .:? "reported_by_person_id"
      <*> o .:? "reported_by_name"
      <*> o .:? "gtfs_id"
      <*> o .:? "route_id"
