module Types.Beckn.FulfillmentDetails where

import EulerHS.Prelude hiding (State, id, state)
import Types.Beckn.Contact (Contact)
import Types.Beckn.Location (Location)
import Types.Beckn.Person (Person)

data FulfillmentDetails = FulfillmentDetails
  { location :: Location,
    instructions :: Maybe DescriptorInfo,
    contact :: Contact,
    person :: Person
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype DescriptorInfo = DescriptorInfo
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
