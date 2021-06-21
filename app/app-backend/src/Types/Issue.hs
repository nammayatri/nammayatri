module Types.Issue (Issue (..)) where

import Beckn.Types.Predicate
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Text
import EulerHS.Prelude

data Issue = Issue
  { reason :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON Issue where
  parseJSON = genericParseJsonWithValidation "Issue" validateIssue

validateIssue :: Validate Issue
validateIssue Issue {..} =
  sequenceA_
    [ validate "reason" reason $ LengthInRange 2 255 `And` text,
      validateMaybe "description" description $ LengthInRange 2 255 `And` text
    ]
  where
    text = star $ alphanum \/ " " \/ ","
