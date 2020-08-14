module Beckn.Types.Core.Invoice where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Payment
import Beckn.Types.Core.Person
import Beckn.Utils.Common
import Data.Text
import Data.Time
import EulerHS.Prelude

data Invoice = Invoice
  { _id :: Text,
    _type :: Text, -- "PROFORMA", "DRAFT", "COMMERCIAL"
    _date :: UTCTime,
    _billing :: Maybe Billing,
    _signatory :: Maybe Person,
    _tax_number :: Text,
    _order_id :: Text,
    _payment :: Payment,
    _breakup :: [BreakupItem],
    _descriptor :: Maybe Descriptor,
    _created_at :: UTCTime,
    _updated_at :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Invoice where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Invoice where
  toJSON = genericToJSON stripAllLensPrefixOptions

data BreakupItem = BreakupItem
  { _item_id :: Text,
    _offer_id :: Text,
    _quantity :: Integer,
    _amount :: MonetaryValue
  }
  deriving (Generic, Show)

instance FromJSON BreakupItem where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON BreakupItem where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Invoice where
  example =
    Invoice
      { _id = idExample,
        _type = "COMMERCIAL",
        _date = example,
        _billing = Nothing,
        _signatory = Nothing,
        _tax_number = "MAJSU8723F",
        _order_id = "28323",
        _payment = example,
        _breakup = [],
        _descriptor = example,
        _created_at = example,
        _updated_at = example
      }
