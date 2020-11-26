module Beckn.Types.Core.Migration.Customer (Customer (..)) where

import Beckn.Types.Core.Migration.Person (Person)
import Data.Aeson (withObject, (.:))
import EulerHS.Prelude

-- allOf union
data Customer
  = SingleCustomer Person
  | GroupCustomer [Person]
  deriving (Generic, Show)

instance FromJSON Customer where
  parseJSON = withObject "Customer" $ \v ->
    v .: "type" >>= \case
      SINGLE -> SingleCustomer <$> v .: "individual"
      GROUP -> GroupCustomer <$> v .: "group"

instance ToJSON Customer where
  toJSON = genericToJSON stripAllLensPrefixOptions

data CustomerType = SINGLE | GROUP
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
