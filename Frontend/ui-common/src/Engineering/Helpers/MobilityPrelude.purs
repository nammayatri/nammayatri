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
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import PrestoDOM as PD
import Prelude
import Data.Array as DA
import Data.Foldable (foldl)
import Data.Number (pi, cos)
import Data.Tuple (Tuple(..))
import Data.Int (pow)
import Data.Function.Uncurried (Fn3(..), runFn3)
import Data.String as DS
import Effect (Effect)

foreign import swapElements :: forall a. Fn3 Int Int (Array a) (Array a)

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

toggleVisibility :: PD.Visibility -> PD.Visibility
toggleVisibility visibility = if visibility == PD.VISIBLE then PD.GONE else PD.VISIBLE

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
groupAdjacent x = DA.cons (DA.take 2 x) (groupAdjacent (DA.drop 2 x))

sortAccToDayName arr = DA.sortBy (\a b -> compare (dayToIndex a) (dayToIndex b)) arr

dayToIndex :: String -> Int
dayToIndex day =
  case day of
    "Mon" -> 0
    "Tue" -> 1
    "Wed" -> 2
    "Thu" -> 3
    "Fri" -> 4
    "Sat" -> 5
    "Sun" -> 6
    _ -> 7

pseudoRandomInt :: Int -> Tuple Int Int
pseudoRandomInt seed =
  let newSeed = (seed * 1103515245 + 12345) `mod` (2 `pow` 31)
      value = newSeed `mod` 100
  in Tuple newSeed value

calculateNearbyLocation :: Number -> Number -> {nearByLat:: Number,nearByLon:: Number}
calculateNearbyLocation lat lon =
    let
        distanceInDegrees = 50.0 / 111111.0
        newLat = lat + distanceInDegrees
        newLon = lon + distanceInDegrees / cos (lat * pi / 180.0)
    in
        {nearByLat: newLat, nearByLon: newLon}

shuffleArray :: forall a. Int -> Array a -> Array a
shuffleArray seed arr =
  let len = DA.length arr
      shuffle :: Int -> Int -> Array a -> Array a
      shuffle _ 0 array = array
      shuffle currentSeed i array =
        let Tuple newSeed randomIndex = pseudoRandomInt currentSeed
            j = randomIndex `mod` i
            swapped = runFn3 swapElements i j array
        in shuffle newSeed (i - 1) swapped
  in shuffle seed (len - 1) arr
findStringWithPrefix :: String -> Array String -> Array String
findStringWithPrefix prefix arr = DA.filter (\item -> startsWith prefix item) arr

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) (DS.toLower str) == (DS.toLower prefix)

noView :: forall w . PD.PrestoDOM (Effect Unit) w
noView = PD.textView [ PD.width $ PD.V 0 , PD.height $ PD.V 0, PD.visibility PD.GONE]

getNumberWithSuffix :: Int -> String
getNumberWithSuffix n
  | n `mod` 100 >= 11 && n `mod` 100 <= 13 = show n <> "th"
  | otherwise = show n <> case n `mod` 10 of
      1 -> "st"
      2 -> "nd"
      3 -> "rd"
      _ -> "th"
strToMaybe :: String -> Maybe String
strToMaybe str = 
  case str of
    "" -> Nothing
    _ -> Just str
