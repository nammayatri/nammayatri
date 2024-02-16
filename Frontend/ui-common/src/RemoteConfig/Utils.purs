{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Common.RemoteConfig.Utils where

import Common.RemoteConfig.Types (RemoteConfig, RCCarousel)
import DecodeUtil (decodeForeignObject, parseJSON)
import Data.String (null)
import Data.Maybe (Maybe(..))
import Prelude (not)

foreign import fetchRemoteConfigString :: String -> String


defaultRemoteConfig :: forall a. RemoteConfig (Array a)
defaultRemoteConfig ={
    bangalore : [],
    kolkata : [],
    chennai : [],
    tumakuru : [],
    mysore : [],
    kochi : [],
    delhi : [],
    hyderabad : [],
    mumbai : [],
    coimbatore : [],
    pondicherry : [],
    goa : [],
    pune : [],
    default : [],
    config : Nothing
}

carouselConfigData :: String -> String -> String -> Array RCCarousel 
carouselConfigData city configKey default = 
    let remoteConfig = fetchRemoteConfigString configKey
        parseVal = if not null remoteConfig then remoteConfig else fetchRemoteConfigString default
        decodedConfg = decodeForeignObject (parseJSON parseVal) defaultRemoteConfig
    in getCityBasedConfig decodedConfg city


getCityBasedConfig :: forall a. RemoteConfig a -> String -> a
getCityBasedConfig config city = 
    case city of
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
        _ -> config.default
