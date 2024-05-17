module Screens.RideRequestPopUp.ScreenData where

import Api.Types (SearchRequest(..))
import Data.Maybe (Maybe(..))
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM.List (ListItem)
import Unsafe.Coerce (unsafeCoerce)

initData :: RideRequestPopUpScreenData
initData =
  { wasHolderCreated: false
  , holderView: Nothing
  , holderData: []
  , rideRequests: []
  , selectedRequest: 0
  , timer : 0.0
  , tabs:
      [ { currentProgress: 0.0
        , maxProgress: 1.0
        , startTime: 0.0
        , price : 0.0
        , id : Nothing
        , isSelected : true
        }
      , { currentProgress: 0.0
        , maxProgress: 1.0
        , startTime: 0.0
        , price : 0.0
        , id : Nothing
        , isSelected : false
        }
      , { currentProgress: 0.0
        , maxProgress: 1.0
        , startTime: 0.0
        , price : 0.0
        , id : Nothing
        , isSelected : false
        }
      ]
  }

type RideRequestPopUpScreenData
  = { wasHolderCreated :: Boolean
    , holderView :: Maybe ListItem
    , holderData :: Array PopupProps
    , rideRequests :: Array SearchRequest
    , selectedRequest :: Int
    , tabs :: Array TabTimers
    , timer :: Number
    }

type PopupProps
  = { tripPrice :: PropValue
    , tripDistance :: PropValue
    , pickupDistance :: PropValue
    , sourceArea :: PropValue
    , sourceFullAddress :: PropValue
    , sourcePincode :: PropValue
    , destinationArea :: PropValue
    , destinationFullAddress :: PropValue
    , destinationPincode :: PropValue
    }

type TabTimers
  = { currentProgress :: Number
    , startTime :: Number
    , maxProgress :: Number
    , price :: Number
    , id :: Maybe String
    , isSelected :: Boolean
    }
