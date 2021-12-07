module Core.OnSearch.Provider (Provider (..)) where

import Beckn.Prelude hiding (exp)
import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import Core.Item (Item)
import Core.OnSearch.Location (Location)

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    category_id :: Maybe Text,
    time :: Maybe Time,
    categories :: Maybe [Category],
    fulfillments :: Maybe [Fulfillment],
    payments :: Maybe [Payment],
    locations :: Maybe [Location],
    offers :: Maybe [Offer],
    items :: Maybe [Item],
    exp :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Provider where
  example =
    Provider
      { id = Nothing,
        descriptor = Nothing,
        category_id = Nothing,
        time = Nothing,
        categories = Nothing,
        fulfillments = Nothing,
        payments = Nothing,
        locations = Nothing,
        offers = Nothing,
        items = Nothing,
        exp = Nothing,
        tags = Nothing
      }
