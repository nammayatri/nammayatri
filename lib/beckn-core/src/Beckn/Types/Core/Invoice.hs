module Beckn.Types.Core.Invoice where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Payment
import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import Data.Time
import EulerHS.Prelude hiding (id)

data Invoice = Invoice
  { id :: Text,
    _type :: Text, -- "PROFORMA", "DRAFT", "COMMERCIAL"
    date :: UTCTime,
    billing :: Maybe Billing,
    signatory :: Maybe Person,
    tax_number :: Text,
    order_id :: Text,
    payment :: Payment,
    breakup :: [BreakupItem],
    descriptor :: Maybe Descriptor,
    created_at :: UTCTime,
    updated_at :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Invoice where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Invoice where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data BreakupItem = BreakupItem
  { item_id :: Text,
    offer_id :: Text,
    quantity :: Integer,
    amount :: MonetaryValue
  }
  deriving (Generic, Show)

instance FromJSON BreakupItem where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON BreakupItem where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Invoice where
  example =
    Invoice
      { id = idExample,
        _type = "COMMERCIAL",
        date = example,
        billing = Nothing,
        signatory = Nothing,
        tax_number = "MAJSU8723F",
        order_id = "28323",
        payment = example,
        breakup = [],
        descriptor = example,
        created_at = example,
        updated_at = example
      }
