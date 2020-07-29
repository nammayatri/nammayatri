module Beckn.Types.Core.Invoice where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Payment
import Beckn.Types.Core.Person
import Beckn.Utils.Common
import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude

data Invoice = Invoice
  { _id :: Text,
    _type :: Text, -- "PROFORMA", "DRAFT", "COMMERCIAL"
    _date :: LocalTime,
    _billing :: Maybe Billing,
    _signatory :: Maybe Person,
    _tax_number :: Text,
    _order_id :: Text,
    _payment :: Payment,
    _breakup :: [BreakupItem],
    _descriptor :: Maybe Descriptor,
    _created_at :: LocalTime,
    _updated_at :: LocalTime
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
        _date = defaultLocalTime,
        _billing = Nothing,
        _signatory = Nothing,
        _tax_number = "MAJSU8723F",
        _order_id = "28323",
        _payment = example,
        _breakup = [],
        _descriptor = example,
        _created_at = defaultLocalTime,
        _updated_at = defaultLocalTime
      }
