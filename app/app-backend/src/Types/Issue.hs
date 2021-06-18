module Types.Issue (Issue (..)) where

import Beckn.Types.Validation.Predicate
import qualified Beckn.Types.Validation.Regex as R
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
    text = R.Many (R.Any $ R.alphanum <> [R.space, R.ExactChar ','])
