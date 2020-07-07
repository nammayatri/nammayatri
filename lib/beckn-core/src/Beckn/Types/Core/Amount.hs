{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Functions rationalToString, validate
-- are only exported for testing purposes.
module Beckn.Types.Core.Amount
  ( Amount,
    rationalToString,
    validate,
  )
where

import Control.Lens.Operators
import Data.Char
import Data.Proxy
import qualified Data.Ratio as R
import Data.Swagger
import qualified Data.Text as T
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import qualified Money as M

newtype Amount = Amount Rational
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Num, Real, Fractional)

maxPrecisionWord8 :: Word8
maxPrecisionWord8 = 30

maxPrecision :: Int
maxPrecision = fromIntegral maxPrecisionWord8

message :: Text
message =
  "Maximum allowed precision (total number of digits) is "
    <> show maxPrecision

-- Functions rationalToString and amountToString should only be used
-- for serialization. They don't perform any proper rounding and simply
-- stop generating digits when at the maximum precision.
rationalToString :: Int -> Rational -> Maybe String
rationalToString precision rational
  | length intPart > precision = Nothing
  | otherwise = Just result
  where
    result =
      if fracPrecision <= 0 || null fracPart
        then intPart
        else intPart <> "." <> take fracPrecision fracPart
    numerator = R.numerator rational
    denominator = R.denominator rational
    sign = if numerator < 0 then "-" else ""
    intPart = sign <> show quotient
    fracPrecision = precision - length intPart
    fracPart = expand remainder
    (quotient, remainder) = abs numerator `quotRem` denominator
    expand currentRemainder
      | currentRemainder == 0 = ""
      | otherwise = show digit <> expand nextRemainder
      where
        (digit, nextRemainder) = (10 * currentRemainder) `quotRem` denominator

amountToString :: Amount -> Text
amountToString value =
  maybe
    (error ("Cannot convert " <> show value <> " to a string. " <> message))
    T.pack
    (rationalToString maxPrecision (toRational value))

amountFromString :: Text -> Maybe Amount
amountFromString amountString =
  Amount . toRational <$> M.denseFromDecimal decimalConf amountString
  where
    -- The exact value passed in DecimalConf.decimalConf_digits to
    -- denseFromDecimal does not matter, but it should be large enough to
    -- make sure there is no precision loss.
    decimalConf =
      M.defaultDecimalConf
        { M.decimalConf_digits = maxPrecisionWord8
        }

validate :: Int -> Text -> Bool
validate precision amountString =
  T.length amountString <= maxPossibleLength
    && countDigits amountString <= precision
  where
    -- Combined length of "-" and "."
    maxNonDigitLength = 2
    maxPossibleLength = T.length amountString + maxNonDigitLength
    countDigits = T.length . T.filter isDigit

instance ToJSON Amount where
  toJSON = toJSON . amountToString

instance FromJSON Amount where
  parseJSON value = do
    amountString <- parseJSON value
    unless (validate maxPrecision amountString) $ failText message
    maybe (parseError amountString) pure $ amountFromString amountString
    where
      parseError amountString =
        failText $ "Cannot parse " <> amountString <> " as a monetary amount."
      failText = fail . T.unpack

instance ToSchema Amount where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "amount") $
        schema
          & description
            ?~ "Monetary amount in a string representation \
               \with an optional leading \"-\" for negative numbers. \
               \Integer and fractional parts are separated with a dot."
              <> message
              <> " String format is used to prevent loss of precision."
          & paramSchema . format ?~ "[-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?"

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Amount where
  sqlValueSyntax = sqlValueSyntax . amountToString

instance FromBackendRow Postgres Amount where
  fromBackendRow = Amount <$> fromBackendRow
