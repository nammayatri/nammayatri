module Beckn.Types.Core.Migration.API.Init where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq)
import Beckn.Types.Core.Migration.Billing (Billing)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Quotation (Quotation)
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseFail)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type InitAPI =
  "init"
    :> ReqBody '[JSON] (BecknReq InitOrder)
    :> Post '[JSON] AckResponse

initAPI :: Proxy InitAPI
initAPI = Proxy

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
  { id :: IdObject,
    locations :: [IdObject]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data InitOrderItem = InitOrderItem
  { id :: Text,
    quantity :: Int
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON InitOrderItem where
  parseJSON = withObject "InitOrderItem" $ \obj -> do
    quantity <- obj .: "quantity"
    unless (quantity >= 0) $ parseFail "Expected quantity to be >= 0."
    itemId <- obj .: "id"
    pure $ InitOrderItem itemId quantity

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
