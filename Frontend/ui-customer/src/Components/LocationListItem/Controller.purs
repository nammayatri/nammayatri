{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.LocationListItem.Controller where

import Data.Maybe (Maybe(..))
import Screens.Types (LocationListItemState, Address)

data Action = OnClick LocationListItemState
            | SelectedCurrentLocation Number Number String
            | FavClick LocationListItemState


locationListStateObj :: LocationListItemState
locationListStateObj = { prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : true
  , title : ""
  , subTitle : ""
  , placeId : Nothing
  , lat : Nothing
  , lon : Nothing
  , description : ""
  , tag : ""
  , tagType : Nothing
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : dummyAddress
  , locationItemType : Nothing
  , distance : Nothing
  , showDistance : Just false
  , actualDistance : Nothing
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  , dynamicAction : Nothing
  , types : Nothing
  }

dummyAddress :: Address
dummyAddress = {
              "area" : Nothing
            , "state" : Nothing
            , "country" : Nothing
            , "building" : Nothing
            , "door" : Nothing
            , "street" : Nothing
            , "city" : Nothing
            , "areaCode" : Nothing
            , "ward" : Nothing
            , "placeId" : Nothing
            }
