-- module Beckn.Types.Core.Migration.Quotation where
module Core.Quotation where

-- import Beckn.Types.Core.Migration.Duration
import Core.Price
-- import Core.DecimalValue
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Types.Core.Migration.Duration
import Beckn.Utils.Example
--        import EulerHS.Prelude
import Data.Aeson
import Data.Text
import GHC.Generics

options :: Options
options = defaultOptions { omitNothingFields = True }

data Quotation = Quotation
  { price :: Price,
    breakup :: Maybe [BreakupItem],
    ttl :: Maybe Duration
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Quotation where
  toJSON = genericToJSON options

data BreakupItem = BreakupItem
  { title :: Maybe Text,
    price :: Maybe Price
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON BreakupItem where
  toJSON = genericToJSON options

instance Example Quotation where
  example =
    Quotation
      { price = Price "INR" (DecimalValue "30"),
        breakup = Just [BreakupItem (Just "One Way Ticket") (Just (Price "INR" (DecimalValue "30")))],
        ttl = Just (Duration "How long inventory would be blocked")
      }


example_quotation :: Quotation
example_quotation = example

