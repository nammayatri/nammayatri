{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.GoogleConfig where

import qualified Kernel.External.Maps.Google.Config as GoogleMaps
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data GoogleCfgUnencrypted = GoogleCfgUnencrypted
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: Text,
    useAdvancedDirections :: Bool,
    googleRouteConfig :: GoogleMaps.GoogleRouteConfig,
    googlePlaceNewUrl :: BaseUrl,
    useNewPlaces :: Bool,
    googleAutocompleteParams :: Maybe [Text]
  }
  deriving (Generic, FromDhall)
