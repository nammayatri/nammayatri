module Core.Spec.OnSearch.Route where

import Beckn.Prelude

data Route = Route
  { id :: Text,
    route_code :: Text,
    start_stop :: Text,
    end_stop :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
