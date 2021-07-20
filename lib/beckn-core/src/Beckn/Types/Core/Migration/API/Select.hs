module Beckn.Types.Core.Migration.API.Select where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq)
import Beckn.Types.Core.Migration.AddOn (AddOn)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.ItemQuantity (ItemQuantity)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Quotation (Quotation)
import Beckn.Utils.JSON (uniteObjects)
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseFail)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type SelectAPI =
  "select"
    :> ReqBody '[JSON] (BecknReq SelectedObject)
    :> Post '[JSON] AckResponse

selectAPI :: Proxy SelectAPI
selectAPI = Proxy

newtype SelectedObject = SelectedObject
  { order :: SelectedOrder
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SelectedOrder = SelectedOrder
  { items :: [SelectedItem],
    add_ons :: Maybe [IdObject],
    offers :: Maybe [IdObject]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SelectedItem = SelectedItem
  { id :: Text,
    quantity :: Int
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON SelectedItem where
  parseJSON = withObject "RatingInfo" $ \obj -> do
    quantity <- obj .: "quantity"
    unless (quantity >= 0) $ parseFail "Expected quantity to be >= 0."
    itemId <- obj .: "id"
    pure $ SelectedItem itemId quantity

type OnSelectAPI =
  "on_select"
    :> ReqBody '[JSON] (BecknCallbackReq SelectedCallbackObject)
    :> Post '[JSON] AckResponse

onSelectAPI :: Proxy OnSelectAPI
onSelectAPI = Proxy

newtype SelectedCallbackObject = SelectedCallbackObject
  { order :: SelectedOrderCallback
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SelectedOrderCallback = SelectedOrderCallback
  { items :: Maybe [SelectedCallbackItem],
    add_ons :: Maybe [AddOn],
    offers :: Maybe [Offer],
    quote :: Maybe Quotation
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SelectedCallbackItem = SelectedCallbackItem Item ItemQuantity
  deriving (Generic, Show)

instance FromJSON SelectedCallbackItem where
  parseJSON obj = SelectedCallbackItem <$> parseJSON obj <*> parseJSON obj

instance ToJSON SelectedCallbackItem where
  toJSON (SelectedCallbackItem item itemQuantity) = uniteObjects [toJSON item, toJSON itemQuantity]
