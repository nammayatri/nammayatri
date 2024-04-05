module Helpers.Images where

import Prelude
import Data.Maybe (Maybe(..))

data CityBasedImage 
    = HOME_SCREEN_WELCOME
    | EMPTY_SUGGESTIONS



getBangaloreImage :: CityBasedImage -> Maybe String
getBangaloreImage image = case image of
    HOME_SCREEN_WELCOME -> Just "ny_ic_home_illustration_blr"
    EMPTY_SUGGESTIONS -> Just "ny_ic_empty_search_blr"

getKolkataImage :: CityBasedImage -> Maybe String
getKolkataImage image = case image of
    HOME_SCREEN_WELCOME -> Just "ny_ic_home_illustration_kol"
    EMPTY_SUGGESTIONS -> Just "ny_ic_empty_search_kol"

getParisImage :: CityBasedImage -> Maybe String
getParisImage image = Nothing

getCochinImage :: CityBasedImage -> Maybe String
getCochinImage image = case image of
    HOME_SCREEN_WELCOME -> Just "ny_ic_home_illustration_koc"
    EMPTY_SUGGESTIONS -> Just "ny_ic_empty_search_koc"

getDelhiImage :: CityBasedImage -> Maybe String
getDelhiImage image = case image of
    HOME_SCREEN_WELCOME -> Just "ny_ic_home_illustration_del"
    EMPTY_SUGGESTIONS -> Just "ny_ic_empty_search_del"

getHyderabadImage :: CityBasedImage -> Maybe String
getHyderabadImage image = case image of
    HOME_SCREEN_WELCOME -> Just "ny_ic_home_illustration_hyd"
    EMPTY_SUGGESTIONS -> Just "ny_ic_empty_search_hyd"

getMumbaiImage :: CityBasedImage -> Maybe String
getMumbaiImage image = Nothing

getChennaiImage :: CityBasedImage -> Maybe String
getChennaiImage image = case image of
    HOME_SCREEN_WELCOME -> Just "ny_ic_home_illustration_che"
    EMPTY_SUGGESTIONS -> Just "ny_ic_empty_search_che"

getCoimbatoreImage :: CityBasedImage -> Maybe String
getCoimbatoreImage image = Nothing

getPondicherryImage :: CityBasedImage -> Maybe String
getPondicherryImage image = Nothing 

getGoaImage :: CityBasedImage -> Maybe String
getGoaImage image = Nothing

getPuneImage :: CityBasedImage -> Maybe String
getPuneImage image = Nothing

getMysoreImage :: CityBasedImage -> Maybe String
getMysoreImage image = Nothing

getTumakuruImage :: CityBasedImage -> Maybe String
getTumakuruImage image = Nothing

getDefaultImage :: CityBasedImage -> String
getDefaultImage image = case image of
    HOME_SCREEN_WELCOME ->  "ny_ic_home_illustration_blr"
    EMPTY_SUGGESTIONS ->  "ny_ic_empty_search_blr"