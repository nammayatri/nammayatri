module Types.Beckn.Contact (Contact (..)) where

import EulerHS.Prelude

newtype Contact = Contact
  { phone :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
