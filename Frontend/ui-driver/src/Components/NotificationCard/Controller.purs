{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.NotificationCard.Controller where

data Action
  = Action1Click Int
  | Action2Click Int
  | IllutrationClick Int
  | NoAction

data CounterData
  = CounterData
    { icon :: String
    , value :: String
    }

getCounters :: String -> Array CounterData
getCounters dummy =
  [ CounterData { icon: "ny_ic_heart_grey,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_heart_grey.png", value: "likeCount" }
  , CounterData { icon: "ny_ic_eye,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_eye.png", value: "viewCount" }
  ]
