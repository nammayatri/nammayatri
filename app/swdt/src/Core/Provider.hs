module Core.Provider  where

-- import Beckn.Types.Core.Migration.Category (Category)
import Core.Descriptor
import Core.Fulfillment
import Core.Item
import Core.Location
-- import Beckn.Types.Core.Migration.Offer (Offer)
-- import Beckn.Types.Core.Migration.Payment (Payment)
-- import Beckn.Types.Core.Migration.Tags (Tags)
-- import Beckn.Types.Core.Migration.Time (Time)
-- import Beckn.Utils.Example
-- import EulerHS.Prelude hiding (exp, id)

import Beckn.Utils.Example
import Data.Aeson
import Data.Text
import GHC.Generics

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    --            category_id :: Maybe Text,
    --            time :: Maybe Time,
    --            categories :: Maybe [Category],
    fulfillments :: Maybe [Fulfillment],
    --            payments :: Maybe [Payment],
    locations :: Maybe [Location],
    --            offers :: Maybe [Offer],
    items :: Maybe [Item]
    --            exp :: Maybe Text,
    --            tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Provider where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance Example Provider where
  example =
    Provider
      { id = Just "SWTD",
        descriptor = Just emptyDescriptor {name = Just "State Water Transport Department"},

        --     category_id = Nothing,
        --      time = Nothing,
        --      categories = Nothing,
        fulfillments = Nothing,
        --      payments = Nothing,
        locations = Nothing,
        --      offers = Nothing,
        items = Nothing
        --      exp = Nothing,
        --      tags = Nothing
      }