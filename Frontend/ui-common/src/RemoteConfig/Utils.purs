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

foreign import fetchRemoteConfigString :: String -> String


defaultRemoteConfig :: RemoteConfig (Array RCCarousel)
defaultRemoteConfig ={
    bangalore : [],
    kolkata : []
}

carouselConfigData :: String -> String -> Array RCCarousel
carouselConfigData city configKey = 
    let remoteConfig = fetchRemoteConfigString configKey --("driver_carousel_banner" <> language)
        decodedConfg = decodeForeignObject (parseJSON remoteConfig) defaultRemoteConfig
    in getCityBasedConfig decodedConfg city
    where 
      getCityBasedConfig :: RemoteConfig (Array RCCarousel) -> String -> Array RCCarousel
      getCityBasedConfig config city = 
          case city of
              "bangalore" -> config.bangalore
              "kolkata" -> config.kolkata
              _ -> []