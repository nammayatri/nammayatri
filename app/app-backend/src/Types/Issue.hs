module Types.Issue where

import Beckn.Types.Predicate
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Text
import EulerHS.Prelude

data Issue = Issue
  { reason :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

validateIssue :: Validate Issue
validateIssue Issue {..} =
  sequenceA_
    [ validateField "reason" reason $ LengthInRange 2 255 `And` text,
      validateField "description" description $ InMaybe $ LengthInRange 2 255 `And` text
    ]
  where
    text = star $ alphanum \/ " " \/ ","
