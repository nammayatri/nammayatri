module Types.Beckn.FulfillmentDetails where

import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Types.Beckn.Contact (Contact)
import Types.Beckn.Location (Location)
import Types.Beckn.Person (Person)

data FulfillmentDetails = FulfillmentDetails
  { location :: Location,
    instructions :: Maybe DescriptorInfo,
    contact :: Contact,
    person :: Person
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DescriptorInfo = DescriptorInfo
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema DescriptorInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
