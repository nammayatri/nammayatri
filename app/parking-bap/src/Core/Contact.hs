module Core.Contact where

import Beckn.Prelude

data Contact = Contact
  { phone :: Text,
    email :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
