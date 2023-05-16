{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON)

-- descriptions on Tabular level is separated with '|' symbol
-- On Domain level it's list
intercalateDescriptions :: [Text] -> Text
intercalateDescriptions = T.intercalate "|"

splitDescriptions :: Text -> [Text]
splitDescriptions = T.splitOn "|"
