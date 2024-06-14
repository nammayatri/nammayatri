module RemoteConfig.Utils where

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON, decodeForeignAny)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Common.RemoteConfig (fetchRemoteConfigString, getCityBasedConfig, defaultRemoteConfig)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import Control.Monad.Except (runExcept)
import Data.Function (on)
import Data.String as DS
import Common.Types.App
import RemoteConfig.Types
import Data.Array as DA
import Locale.Utils(getLanguageLocale)
import Constants (languageKey)
import DecodeUtil

safetyVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_videos_" <> language)
        value = decodeForeignAny (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

safetyBannerVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyBannerVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_banner_videos_" <> language)
        value = decodeForeignAny (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

pickupInstructions :: String -> String -> String -> Array PickupInstructions
pickupInstructions placeName gateName language = do
    let config = fetchRemoteConfigString ("pickup_instructions_" <> language)
        pickupPlaces = decodeForeignObject (parseJSON config) defPlaces
        compareStrings = on (==) DS.trim
        locationsArr = pickupPlaces.locations
        myMbSpecialLocation = DA.find (\el -> compareStrings el.name placeName) locationsArr
    case myMbSpecialLocation of
        Just specialLocation -> do
            let gates = specialLocation.gates
                myMbgate = DA.find (\el -> compareStrings el.gateName gateName) gates
            case myMbgate of
                Just gate -> gate.images
                Nothing -> []
        Nothing -> []

defPlaces :: SpecialLocationsOb
defPlaces = {
    locations : []
}

getFamousDestinations :: String -> Array FamousDestination
getFamousDestinations city = do
    let langConfig = fetchRemoteConfigString $ "famous_destinations" <> (getLanguage $ getLanguageLocale languageKey)
        config = if not $ DS.null langConfig
                   then langConfig 
                   else fetchRemoteConfigString "famous_destinations_en"
        value = decodeForeignAny (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city
  where
    getLanguage lang = 
      let language = DS.toLower $ DS.take 2 lang
      in if not (DS.null language) then "_" <> language else "_en"