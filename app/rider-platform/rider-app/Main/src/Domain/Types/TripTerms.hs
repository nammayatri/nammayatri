module Domain.Types.TripTerms where

import Data.Text as T
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.GenericPretty (PrettyShow)

-- Non empty list here?
data TripTerms = TripTerms
  { id :: Id TripTerms,
    descriptions :: [Text]
  }
  deriving (Generic, Show, PrettyShow)

-- descriptions on Tabular level is separated with '|' symbol
-- On Domain level it's list
intercalateDescriptions :: [Text] -> Text
intercalateDescriptions = T.intercalate "|"

splitDescriptions :: Text -> [Text]
splitDescriptions = T.splitOn "|"
