module Beckn.Types.Core.Migration.Customer where

import Beckn.Types.Core.Migration.Person (Person)
import Data.Aeson (object, withObject, (.:), (.=))
import EulerHS.Prelude hiding ((.=))

-- allOf union
data Customer
  = SingleCustomer SingleCustomer'
  | GroupCustomer GroupCustomer'
  deriving (Eq, Generic, Show)

newtype SingleCustomer' = SingleCustomer' {info :: Person}
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data GroupCustomer' = GroupCustomer'
  { primary :: Person,
    count :: Integer
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance FromJSON Customer where
  parseJSON = withObject "Customer" $ \v ->
    v .: "type" >>= \case
      SINGLE -> SingleCustomer <$> v .: "individual"
      GROUP -> GroupCustomer <$> v .: "group"

instance ToJSON Customer where
  toJSON (SingleCustomer person) =
    object
      [ "type" .= SINGLE,
        "individual" .= person
      ]
  toJSON (GroupCustomer groupCustomer) =
    object
      [ "type" .= GROUP,
        "group" .= groupCustomer
      ]

data CustomerType = SINGLE | GROUP
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
