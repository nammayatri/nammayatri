{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RouteDetails.Controller where

import Prelude (class Eq, class Show )
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Screens.Types (Location)
import Data.Maybe

data Action
    = NoAction
    | CloseRouteDetailsView

type Config 
    = { start :: Stop
      , end :: Stop
      , stops :: Array Stop
      , busId :: String
      }
-- Location has place which has name and lat lons to show on map

type Stop 
    = { location :: Location
      , scheduledTime :: String
      }

config :: Config 
config = 
    { start : dummyStop
    , end : dummyStop
    , stops : []
    , busId : "2512761"
    }

dummyStop :: Stop
dummyStop =  
    { location : 
        { place : ""
        , lat : 0.0
        , lng :0.0
        , address: Just ""
        }
    , scheduledTime : ""
    }
            

