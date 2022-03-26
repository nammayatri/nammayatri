module Types.Beckn.API.OnConfirm (module Types.Beckn.API.OnConfirm, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State, id, state)
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Billing as Reexport (Billing (..))
import Types.Beckn.Contact as Reexport (Contact (..))
import Types.Beckn.DecimalValue as Reexport
  ( DecimalValue (..),
    convertAmountToDecimalValue,
    convertDecimalValueToAmount,
  )
import Types.Beckn.Fulfillment as Reexport (Fulfillment (..))
import Types.Beckn.Location as Reexport (Location (..))
import Types.Beckn.Order as Reexport (OrderItem (..), Status (..))
import Types.Beckn.Payment as Reexport (Params (..), Payment (..), PaymentType (..))
import Types.Beckn.Person as Reexport (Person (..))

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OrderObject)
    :> Post '[JSON] AckResponse

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy

newtype OrderObject = OrderObject
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Order = Order
  { id :: Text,
    state :: Status,
    items :: [OrderItem],
    billing :: Billing,
    fulfillment :: Fulfillment,
    payment :: Payment,
    updated_at :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)
