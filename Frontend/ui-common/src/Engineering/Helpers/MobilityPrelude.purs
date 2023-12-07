{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Mobility.Prelude where

import Data.String (null,Pattern(..), contains, joinWith, toLower, take, toUpper, drop, trim)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import PrestoDOM as PD
import Prelude
import Data.Array (elem)
import Data.Foldable (foldl)

has :: String -> String -> Boolean
has msg errorPattern = contains (Pattern errorPattern) msg

-- | Capitalize the first `Char` in a `String`
capitalize :: String -> String
capitalize str = (take 1 str # toUpper) <> drop 1 str

strToBool :: String -> Boolean
strToBool x = toLower x == "true"

toBool :: String -> Boolean
toBool x = toLower x == "true"

maybeToBool :: Maybe String -> Boolean
maybeToBool val = toBool $ fromMaybe "" val

boolToVisibility :: Boolean -> PD.Visibility
boolToVisibility true = PD.VISIBLE
boolToVisibility false = PD.GONE

catMaybeStrings :: Array (Maybe String) -> String
catMaybeStrings arr = 
  trim $ foldl 
    (\acc x -> maybe acc (\a -> acc <> a <> " ") x) "" arr
