module Types.Beckn.API.Confirm (module Types.Beckn.API.Confirm, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Data.Aeson (withObject, (.!=), (.:), (.:?))
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Address as Reexport (Address (..))
import Types.Beckn.Billing as Reexport (Billing (..))
import Types.Beckn.Contact as Reexport (Contact (..))
import Types.Beckn.DecimalValue as Reexport
  ( DecimalValue (..),
    convertAmountToDecimalValue,
    convertDecimalValueToAmount,
  )
import Types.Beckn.FulfillmentDetails as Reexport (DescriptorInfo (..), FulfillmentDetails (..))
import Types.Beckn.Gps as Reexport (Gps (..))
import Types.Beckn.Location as Reexport (Location (..))
import Types.Beckn.Name as Reexport (Name (..))
import Types.Beckn.Order (OrderItem)
import Types.Beckn.Order as Reexport (OrderItem (..))
import Types.Beckn.Person as Reexport (Person (..))

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] (BecknReq OrderObject)
    :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

newtype OrderObject = OrderObject
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Order = Order
  { items :: [OrderItem],
    billing :: Billing,
    fulfillment :: Fulfillment,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Fulfillment = Fulfillment
  { tracking :: Bool,
    start :: FulfillmentDetails,
    end :: FulfillmentDetails
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON Fulfillment where
  parseJSON = withObject "Fulfillment" $ \o ->
    Fulfillment
      <$> o .:? "tracking" .!= False
      <*> o .: "start"
      <*> o .: "end"

newtype Payment = Payment
  { params :: Params
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Params = Params
  { transaction_id :: Text,
    amount :: Maybe DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)
