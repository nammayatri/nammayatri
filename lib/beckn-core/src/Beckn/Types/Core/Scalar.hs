module Beckn.Types.Core.Scalar where

import Beckn.Types.Core.ScalarRange
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Scalar = Scalar
  { _type :: Maybe Text, -- ["CONSTANT", "VARIABLE"]
    value :: Maybe Double,
    estimated_value :: Maybe Double,
    computed_value :: Maybe Double,
    range :: Maybe ScalarRange,
    unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Scalar where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Scalar where
  example =
    Scalar
      { _type = Just "CONSTANT",
        value = Just 12.345,
        estimated_value = Just 12.345,
        computed_value = Just 12.345,
        range = example,
        unit = "meters"
      }

emptyScalar :: Text -> Scalar
emptyScalar unit =
  Scalar
    { _type = Nothing,
      value = Nothing,
      estimated_value = Nothing,
      computed_value = Nothing,
      range = Nothing,
      unit
    }

mkDistance :: Double -> Scalar
mkDistance dst = (emptyScalar "meters") {value = Just dst}

mkDuration :: Double -> Scalar
mkDuration t = (emptyScalar "seconds") {value = Just t}

scalarToDouble :: Scalar -> Maybe Double
scalarToDouble Scalar {..} =
  value
    <|> estimated_value
    <|> computed_value
