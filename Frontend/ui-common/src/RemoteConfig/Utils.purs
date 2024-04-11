{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.RemoteConfig.Utils where

import Common.RemoteConfig.Types (RemoteConfig, RCCarousel(..))
import DecodeUtil (decodeForeignObject, parseJSON)
import Data.String (null)
import Data.Maybe (Maybe(..))
import Prelude (not, ($), (==))
import Data.Maybe (fromMaybe)
import Data.Array (elem, filter, uncons)
import Data.Array as DA

foreign import fetchRemoteConfigString :: String -> String

foreign import fetchRemoteConfig :: forall a. String -> a

foreign import isWhiteListed :: String -> Array String -> Boolean

defaultRemoteConfig :: forall a. RemoteConfig (Array a)
defaultRemoteConfig =
  { bangalore: []
  , kolkata: []
  , chennai: []
  , tumakuru: []
  , mysore: []
  , kochi: []
  , delhi: []
  , hyderabad: []
  , mumbai: []
  , coimbatore: []
  , pondicherry: []
  , goa: []
  , pune: []
  , tamilnaducities: []
  , default: []
  , config: Nothing
  }

carouselConfigData :: String -> String -> String -> String -> String -> Array RCCarousel
carouselConfigData city configKey default userId categoryFilter =
  let
    remoteConfig = fetchRemoteConfigString configKey

    parseVal = if not null remoteConfig then remoteConfig else fetchRemoteConfigString default

    decodedConfg = decodeForeignObject (parseJSON parseVal) defaultRemoteConfig
  in
    filterWhiteListedConfigs userId $ filterCategoryBasedCarousel categoryFilter $ getCityBasedConfig decodedConfg city

-- Each RCCarousel has a category field which is an array of strings, If the array is empty I want to include that RCCarousel in output array, but if it has some values I want to match check `elem` if the categoryFilter is present in the array or not. If it is present then include that RCCarousel in the output array.
filterCategoryBasedCarousel :: String -> Array RCCarousel -> Array RCCarousel
filterCategoryBasedCarousel allowedFilter configs =
  let
    filteredConfigs = filter (\x -> validateConfig x) configs
  in
    filteredConfigs
  where
  validateConfig :: RCCarousel -> Boolean
  validateConfig (RCCarousel config) =
    let
      categoryList = fromMaybe [] config.categoryFilter
    in
      if DA.null categoryList then true else elem allowedFilter categoryList

fetchWhiteListedUser :: String -> Array String
fetchWhiteListedUser configKey = fetchRemoteConfig configKey

filterWhiteListedConfigs :: String -> Array RCCarousel -> Array RCCarousel
filterWhiteListedConfigs userId configs =
  let
    whiteListedConfigs = filter (\x -> validateConfig x) configs
  in
    whiteListedConfigs
  where
  validateConfig :: RCCarousel -> Boolean
  validateConfig (RCCarousel config) =
    let
      whiteListedUserListArray = fromMaybe [] config.whitelist
    in
      if DA.null whiteListedUserListArray then true else validateUser whiteListedUserListArray

  validateUser :: Array String -> Boolean
  validateUser parameterList = case uncons parameterList of
    Just { head: x, tail: xs } ->
      let
        userList = fetchWhiteListedUser x
      in
        if isWhiteListed userId userList then true else validateUser xs -- TODO:: Need to check why it's not working within PS and replace with Map for optimisation
    Nothing -> false

getCityBasedConfig :: forall a. RemoteConfig a -> String -> a
getCityBasedConfig config city = case city of
  "bangalore" -> config.bangalore
  "kolkata" -> config.kolkata
  "chennai" -> config.chennai
  "mysore" -> config.mysore
  "tumakuru" -> config.tumakuru
  "kochi" -> config.kochi
  "delhi" -> config.delhi
  "hyderabad" -> config.hyderabad
  "mumbai" -> config.mumbai
  "coimbatore" -> config.coimbatore
  "pondicherry" -> config.pondicherry
  "goa" -> config.goa
  "pune" -> config.pune
  "tamilnaducities" -> config.tamilnaducities
  _ -> config.default
