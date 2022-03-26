module Types.Beckn.Person (Person (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Person = Person
  { name :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
