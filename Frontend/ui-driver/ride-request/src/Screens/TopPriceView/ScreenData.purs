module Screens.TopPriceView.ScreenData where

import Api.Types (SearchRequest(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM.List (ListItem)
import Unsafe.Coerce (unsafeCoerce)

initData :: TopPriceViewState
initData =
  { rideRequests: []
  , timer : 0.0
  , tabs:
      [ { currentProgress: 0.0
        , maxProgress: 1.0
        , startTime: 0.0
        , price : 0.0
        , id : Nothing
        , selected : true
        }
      , { currentProgress: 0.0
        , maxProgress: 1.0
        , startTime: 0.0
        , price : 0.0
        , id : Nothing
        , selected : false
        }
      , { currentProgress: 0.0
        , maxProgress: 1.0
        , startTime: 0.0
        , price : 0.0
        , id : Nothing
        , selected : false
        }
      ]
  }

type TopPriceViewState
  = { rideRequests :: Array SearchRequest
    , tabs :: Array TabTimers
    , timer :: Number
    }

type TabTimers
  = { currentProgress :: Number
    , startTime :: Number
    , maxProgress :: Number
    , price :: Number
    , id :: Maybe String
    , selected :: Boolean
    }
