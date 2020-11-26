module Beckn.Types.Core.Migration.DecimalValue
  ( DecimalValue (..),
    convertDecimalValueToAmount,
    convertAmountToDecimalValue,
  )
where

import Beckn.Types.Amount
  ( Amount,
    amountFromString,
    amountToString,
  )
import Data.Aeson (withText)
import Data.Text (split)
import EulerHS.Prelude

data DecimalValue = DecimalValue
  { _integral :: Text,
    _fractional :: Maybe Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON DecimalValue where
  parseJSON = withText "DecimalValue" (maybe fail' pure . stringToDecimalValue)
    where
      fail' = fail "DecimalValue parsing error"

instance ToJSON DecimalValue where
  toJSON = genericToJSON stripAllLensPrefixOptions

convertDecimalValueToAmount :: DecimalValue -> Maybe Amount
convertDecimalValueToAmount (DecimalValue integral (Just fractional)) =
  amountFromString $ integral <> "." <> fractional
convertDecimalValueToAmount (DecimalValue integral Nothing) =
  amountFromString integral

convertAmountToDecimalValue :: Amount -> DecimalValue
convertAmountToDecimalValue = fromMaybe fail' . stringToDecimalValue . amountToString -- FIXME
  where
    fail' = error "stringToDecimalValue conversion error"

stringToDecimalValue :: Text -> Maybe DecimalValue
stringToDecimalValue text =
  case split (== '.') text of
    [integral, fractional] -> Just $ DecimalValue integral (Just fractional)
    [integral] -> Just $ DecimalValue integral Nothing
    _ -> Nothing
