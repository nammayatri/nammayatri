module Types.Beckn.API.OnStatus (module Types.Beckn.API.OnStatus, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Billing as Reexport (Billing (..))
import Types.Beckn.Contact as Reexport (Contact (..))
import Types.Beckn.DecimalValue as Reexport
  ( DecimalValue (..),
    convertAmountToDecimalValue,
    convertDecimalValueToAmount,
  )
import Types.Beckn.Fulfillment as Reexport (Fulfillment (..))
import Types.Beckn.FulfillmentDetails as Reexport (DescriptorInfo (..), FulfillmentDetails (..))
import Types.Beckn.Location as Reexport (Location (..))
import Types.Beckn.Name as Reexport (Name (..))
import Types.Beckn.Order as Reexport (Order (..), OrderItem (..), OrderObject (..))
import Types.Beckn.Payment as Reexport (Params (..), Payment (..), PaymentType (..))
import Types.Beckn.Person as Reexport (Person (..))

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OrderObject)
    :> Post '[JSON] AckResponse

onStatusAPI :: Proxy OnStatusAPI
onStatusAPI = Proxy
