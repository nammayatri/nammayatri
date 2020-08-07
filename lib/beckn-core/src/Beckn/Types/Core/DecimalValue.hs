module Beckn.Types.Core.DecimalValue where

import Beckn.Types.Core.Amount
import Beckn.Utils.Common
import Data.Text hiding (head, length)
import EulerHS.Prelude

data DecimalValue = DecimalValue
  { _integral :: Text,
    _fractional :: Maybe Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON DecimalValue where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON DecimalValue where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example DecimalValue where
  example =
    DecimalValue
      { _integral = "10",
        _fractional = Just "50"
      }

convertDecimalValueToAmount :: DecimalValue -> Maybe Amount
convertDecimalValueToAmount (DecimalValue integral (Just fractional)) =
  amountFromString $ integral <> "." <> fractional
convertDecimalValueToAmount (DecimalValue integral Nothing) =
  amountFromString integral

convertAmountToDecimalValue :: Amount -> DecimalValue
convertAmountToDecimalValue amount =
  case split (== '.') amountString of
    [integral, fractional] -> DecimalValue integral (Just fractional)
    [integral] -> DecimalValue integral Nothing
    _ -> error $ "Cannot convert " <> amountString <> " to a DecimalValue."
  where
    amountString = amountToString amount
