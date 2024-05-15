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
  }

type RideRequestPopUpScreenData
  = { wasHolderCreated :: Boolean
    , holderView :: Maybe ListItem
    , holderData :: Array PopupProps
    , rideRequests :: Array SearchRequest
    , selectedRequest :: Int
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
