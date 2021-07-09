module Beckn.Types.Core.Migration.API.Init where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq)
import Beckn.Types.Core.Migration.Billing (Billing)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.ItemQuantity (Quantity)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Quotation (Quotation)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type InitAPI =
  "init"
    :> ReqBody '[JSON] (BecknReq InitOrderObj)
    :> Post '[JSON] AckResponse

initAPI :: Proxy InitAPI
initAPI = Proxy

newtype InitOrderObj = InitOrderObj {order :: InitOrder}
  deriving (Generic, Show, FromJSON, ToJSON)

data InitOrder = InitOrder
  { provider :: InitOrderProvider,
    items :: [InitOrderItem],
    add_ons :: [IdObject],
    offers :: [IdObject],
    billing :: Billing,
    fulfillment :: Fulfillment
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data InitOrderProvider = InitOrderProvider
  { id :: Text,
    locations :: [IdObject]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data InitOrderItem = InitOrderItem
  { id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnInitAPI =
  "on_init"
    :> ReqBody '[JSON] (BecknCallbackReq InitializedObject)
    :> Post '[JSON] AckResponse

onInitAPI :: Proxy OnInitAPI
onInitAPI = Proxy

newtype InitializedObject = InitializedObject
  { initialized :: Initialized
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Initialized = Initialized
  { provider :: Maybe IdObject,
    provider_location :: Maybe IdObject,
    items :: Maybe [InitOrderItem],
    add_ons :: Maybe [IdObject],
    offers :: Maybe [IdObject],
    billing :: Maybe Billing,
    fulfillment :: Maybe Fulfillment,
    quote :: Maybe Quotation,
    payment :: Maybe Payment
  }
  deriving (Generic, Show, FromJSON, ToJSON)
