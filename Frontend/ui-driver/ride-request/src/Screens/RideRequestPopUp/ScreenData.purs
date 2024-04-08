module Screens.RideRequestPopUp.ScreenData where

import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM.List (ListItem)
import Unsafe.Coerce (unsafeCoerce)

initData :: RideRequestPopUpScreenData
initData =
  { wasHolderCreated: false
  , holderView: unsafeCoerce "{}"
  , holderData: []
  }

type RideRequestPopUpScreenData
  = { wasHolderCreated :: Boolean
    , holderView :: ListItem
    , holderData :: Array PopupProps
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
