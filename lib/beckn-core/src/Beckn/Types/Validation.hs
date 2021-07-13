module Beckn.Types.Validation where

import qualified Data.Either.Validation as V
import EulerHS.Prelude

type Validation = V.Validation [ValidationDescription] ()

data ValidationDescription = ValidationDescription
  { fieldName :: [Text], -- field.subfield ~ ["field", "subfield"]
    expectation :: Text
  }
  deriving (Generic, ToJSON, Show)

type Validate a = a -> Validation
