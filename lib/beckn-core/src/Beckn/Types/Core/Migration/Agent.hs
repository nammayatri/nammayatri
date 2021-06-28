module Beckn.Types.Core.Migration.Agent (Agent (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Beckn.Types.Core.Migration.Name (Name)
import Beckn.Types.Core.Migration.Tags (Tags)
import EulerHS.Prelude

data Agent = Agent
  { name :: Maybe Name,
    image :: Maybe Image,
    dob :: Maybe Text, -- format: date
    gender :: Maybe Text,
    cred :: Text,
    phone :: Maybe Text,
    email :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
