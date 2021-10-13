module Beckn.Types.Core.Migration.Intent where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Beckn.Utils.Example
import EulerHS.Prelude hiding (id)

data Intent = Intent
  { provider :: Maybe ProviderInfo,
    fulfillment :: Maybe FulFillmentInfo,
    payment :: Maybe Payment,
    category :: Maybe Category,
    offer :: Maybe Offer,
    item :: Maybe Item,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show)

emptyIntent :: Intent
emptyIntent =
  Intent
    { provider = Nothing,
      fulfillment = Nothing,
      payment = Nothing,
      category = Nothing,
      offer = Nothing,
      item = Nothing,
      tags = Nothing
    }

instance Example Intent where
  example =
    Intent
      { provider = Nothing,
        fulfillment = example,
        payment = Nothing,
        category = Nothing,
        offer = Nothing,
        item = Nothing,
        tags = Nothing
      }

data ProviderInfo = ProviderInfo
  { id :: Maybe Text,
    descriptor :: Maybe DescriptorName,
    category_id :: Maybe Text,
    locations :: Maybe [IdObject]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype DescriptorName = DescriptorName {name :: Text}
  deriving (Generic, FromJSON, ToJSON, Show)

data FulFillmentInfo = FulFillmentInfo
  { id :: Maybe Text,
    start :: Maybe LocationAndTime,
    end :: Maybe LocationAndTime,
    tags :: Maybe Tags,
    vehicle :: Maybe Vehicle
  }
  deriving (Generic, FromJSON, ToJSON, Show)

emptyFulFillmentInfo :: FulFillmentInfo
emptyFulFillmentInfo =
  FulFillmentInfo
    { id = Nothing,
      start = Nothing,
      end = Nothing,
      tags = Nothing,
      vehicle = Nothing
    }

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { id = Nothing,
        start = example,
        end = example,
        tags = Nothing,
        vehicle = Nothing
      }

data LocationAndTime = LocationAndTime
  { location :: Maybe Location,
    time :: Maybe Time
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example LocationAndTime where
  example =
    LocationAndTime
      { location = example,
        time = Nothing
      }
