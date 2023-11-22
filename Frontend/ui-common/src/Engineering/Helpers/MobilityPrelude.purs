module Engineering.Helpers.MobilityPrelude where

import Data.String (null,Pattern(..), contains, joinWith, toLower, take, toUpper, drop)
import Data.Maybe (Maybe(..), fromMaybe)
import PrestoDOM as PD
import Prelude
import Data.Array (elem)

isStrEmpty :: String -> Boolean
isStrEmpty = null

has :: String -> String -> Boolean
has msg errorPattern = contains (Pattern errorPattern) msg

equalIgnoreCase :: String -> String -> Boolean
equalIgnoreCase val1 val2 = (toLower val1) == (toLower val2)

isPresent :: String -> Array String -> Boolean
isPresent key array =
      (toLower key) `elem` (map (\item -> toLower item) array)

-- | Capitalize the first `Char` in a `String`
capitalize :: String -> String
capitalize str = (take 1 str # toUpper) <> drop 1 str

-- | Join an `Array` of `String`s with spaces
unwords :: Array String -> String
unwords = joinWith " "

toBool :: String -> Boolean
toBool x = toLower x == "true"

fromMaybeString :: Maybe String -> String
fromMaybeString = fromMaybe ""

maybeToBool :: Maybe String -> Boolean
maybeToBool val = toBool $ fromMaybe "" val

maybeArr :: forall a. Maybe (Array a) -> Array a
maybeArr = fromMaybe []

isJustTrue :: Maybe Boolean -> Boolean
isJustTrue (Just true) = true
isJustTrue _ = false

isJustFalse :: Maybe Boolean -> Boolean
isJustFalse (Just false) = true
isJustFalse _ = true

boolToVisibility :: Boolean -> PD.Visibility
boolToVisibility true = PD.VISIBLE
boolToVisibility false = PD.GONE