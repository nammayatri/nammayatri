{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Intent where

import Beckn.Types.Core.Metro.Search.Category (Category)
import Beckn.Types.Core.Metro.Search.Item (Item)
import Beckn.Types.Core.Metro.Search.Location (Location)
import Beckn.Types.Core.Metro.Search.Offer (Offer)
import Beckn.Types.Core.Metro.Search.Payment (Payment)
import Beckn.Types.Core.Metro.Search.Tags (Tags)
import Beckn.Types.Core.Metro.Search.Time (Time)
import Beckn.Types.Core.Metro.Search.Vehicle (Vehicle)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common (IdObject)
import Kernel.Utils.Example (Example (..))

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
