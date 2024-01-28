{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Mobility.Prelude where

import Data.String (null,Pattern(..), contains, joinWith, toLower, take, toUpper, drop, trim, split)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import PrestoDOM as PD
import Prelude
import Data.Array (elem, cons)
import Data.Array as DA
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

boolToInvisibility :: Boolean -> PD.Visibility
boolToInvisibility true = PD.VISIBLE
boolToInvisibility false = PD.INVISIBLE

catMaybeStrings :: Array (Maybe String) -> String
catMaybeStrings arr = 
  trim $ foldl 
    (\acc x -> maybe acc (\a -> acc <> a <> " ") x) "" arr

-- | Capitalize the first letter of each `Word` in a `Sentence` and lower case the rest.
spaceSeparatedPascalCase :: String -> String
spaceSeparatedPascalCase inputStr =
  let 
    splitedArray = split (Pattern " ") inputStr
  in 
    trim $ foldl (\acc item -> acc <> capitalize' item) "" splitedArray
  where 
    capitalize' :: String -> String
    capitalize' str = capitalize $ toLower str <> " "

boolToInt :: Boolean -> Int
boolToInt bool = if bool then 1 else 0
  
caseInsensitiveCompare :: String -> String -> Boolean
caseInsensitiveCompare str1 str2 = 
  toLower(str1) == toLower(str2)

groupAdjacent :: forall a. Array a -> Array (Array a)
groupAdjacent [] = []
groupAdjacent x = cons (DA.take 2 x) (groupAdjacent (DA.drop 2 x))
