module Types.Issue where

import Beckn.Types.Predicate
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude

data Issue = Issue
  { reason :: Text,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

validateIssue :: Validate Issue
validateIssue Issue {..} =
  sequenceA_
    [ validateField "reason" reason $ LengthInRange 2 255 `And` text,
      validateField "description" description $ LengthInRange 2 255 `And` text
    ]
  where
    text = star $ alphanum \/ " " \/ ","
