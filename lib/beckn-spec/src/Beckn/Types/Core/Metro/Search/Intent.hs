module Beckn.Types.Core.Metro.Search.Intent where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Metro.Search.Category (Category)
import Beckn.Types.Core.Metro.Search.Item (Item)
import Beckn.Types.Core.Metro.Search.Location (Location)
import Beckn.Types.Core.Metro.Search.Offer (Offer)
import Beckn.Types.Core.Metro.Search.Payment (Payment)
import Beckn.Types.Core.Metro.Search.Tags (Tags)
import Beckn.Types.Core.Metro.Search.Time (Time)
import Beckn.Types.Core.Metro.Search.Vehicle (Vehicle)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
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
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DescriptorName = DescriptorName {name :: Text}
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data FulFillmentInfo = FulFillmentInfo
  { id :: Maybe Text,
    start :: Maybe LocationAndTime,
    end :: Maybe LocationAndTime,
    tags :: Maybe Tags,
    vehicle :: Maybe Vehicle
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

emptyLocationAndTime :: LocationAndTime
emptyLocationAndTime =
  LocationAndTime
    { location = Nothing,
      time = Nothing
    }

instance Example LocationAndTime where
  example =
    LocationAndTime
      { location = example,
        time = Nothing
      }
