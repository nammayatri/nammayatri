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
   , progress : 0.0
  }

type TopPriceViewState
  = { rideRequests :: Array SearchRequest
    , progress :: Number
    }