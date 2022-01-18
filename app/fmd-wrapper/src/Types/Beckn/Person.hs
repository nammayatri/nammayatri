module Types.Beckn.Person (Person (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Beckn.Name (Name)

newtype Person = Person
  { name :: Name
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
