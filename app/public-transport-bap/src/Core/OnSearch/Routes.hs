module Core.OnSearch.Routes where

import Beckn.Prelude

data Routes = Routes
  { id :: Text,
    start_stop :: Text,
    end_stop :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)