module Beckn.Types.Core.Price where

import Beckn.Types.Amount
import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Price = Price
  { currency :: Text,
    value :: Maybe DecimalValue,
    estimated_value :: Maybe DecimalValue,
    computed_value :: Maybe DecimalValue,
    listed_value :: Maybe DecimalValue,
    offered_value :: Maybe DecimalValue,
    minimum_value :: Maybe DecimalValue,
    maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Price where
  example =
    Price
      { currency = "INR",
        value = example,
        estimated_value = example,
        computed_value = example,
        listed_value = example,
        offered_value = example,
        minimum_value = example,
        maximum_value = example
      }

emptyPrice :: Price
emptyPrice =
  Price
    { currency = "INR",
      value = Nothing,
      estimated_value = Nothing,
      computed_value = Nothing,
      listed_value = Nothing,
      offered_value = Nothing,
      minimum_value = Nothing,
      maximum_value = Nothing
    }

amountToPrice :: Amount -> Price
amountToPrice price =
  emptyPrice
    { value = Just $ convertAmountToDecimalValue price
    }

priceToAmount :: Price -> Maybe Amount
priceToAmount Price {..} =
  convertDecimalValueToAmount
    =<< value
    <|> computed_value
    <|> listed_value
    <|> offered_value
    <|> estimated_value
    <|> minimum_value
    <|> maximum_value
