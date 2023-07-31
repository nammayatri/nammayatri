module Screens.DriverSavedLocationScreen.ScreenData where

import Prelude
import Screens.Types (DriverSavedLocationScreenState, SavedLocationScreenType(..))

initData :: DriverSavedLocationScreenState
initData = {
  data: {
    address : "",
    lat : 0.0,
    lon : 0.0
  }
, props : {
  viewType : GO_TO_LIST -- | LOCATE_ON_MAP | CONFIRM_LOCATION | ENABLE_GO_TO
  } 
}
