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
    reportedByName :: Maybe Text
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
        "reported_by_name" .= reportedByName
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
