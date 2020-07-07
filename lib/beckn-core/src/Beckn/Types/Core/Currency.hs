{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Functions rationalToString, validate
-- are only exported for testing purposes.
module Beckn.Types.Core.Currency
  ( Money,
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

newtype Money = Money Rational
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

-- Functions rationalToString and moneyToString should only be used
-- for serialization. They don't perform any proper rounding and simply
-- stop generating digits when at the maximum precision.
-- For all other needs denseToDecimal and discreteToDecimal
-- from safe-money should be used.
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

moneyToString :: Money -> Text
moneyToString value =
  maybe
    (error ("Cannot convert " <> show value <> " to a string. " <> message))
    T.pack
    (rationalToString maxPrecision (toRational value))

moneyFromString :: Text -> Maybe Money
moneyFromString moneyString =
  Money . toRational <$> M.denseFromDecimal decimalConf moneyString
  where
    -- The exact value passed in DecimalConf.decimalConf_digits to
    -- denseFromDecimal does not matter, but it should be large enough to
    -- make sure there is no precision loss.
    decimalConf =
      M.defaultDecimalConf
        { M.decimalConf_digits = maxPrecisionWord8
        }

validate :: Int -> Text -> Bool
validate precision moneyString =
  T.length moneyString <= maxPossibleLength
    && countDigits moneyString <= precision
  where
    -- Combined length of "-" and "."
    maxNonDigitLength = 2
    maxPossibleLength = T.length moneyString + maxNonDigitLength
    countDigits = T.length . T.filter isDigit

instance ToJSON Money where
  toJSON = toJSON . moneyToString

instance FromJSON Money where
  parseJSON value = do
    moneyString <- parseJSON value
    unless (validate maxPrecision moneyString) $ failText message
    maybe (parseError moneyString) pure $ moneyFromString moneyString
    where
      parseError moneyString =
        failText $ "Cannot parse " <> moneyString <> " as a currency value."
      failText = fail . T.unpack

instance ToSchema Money where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "money") $
        schema
          & description
            ?~ "Monetary value in a string representation \
               \with an optional leading \"-\" for negative numbers. \
               \Integer and fractional parts are separated with a dot."
              <> message
              <> " String format is used to prevent loss of precision."
          & paramSchema . format ?~ "[-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?"

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Money where
  sqlValueSyntax = sqlValueSyntax . moneyToString

instance FromBackendRow Postgres Money where
  fromBackendRow = Money <$> fromBackendRow
