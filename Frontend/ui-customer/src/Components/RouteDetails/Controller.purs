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
            

