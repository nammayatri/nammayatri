module Screens.SavedLocationScreen.ScreenData where

import Screens.Types(SavedLocationScreenState)
import Data.Maybe(Maybe(..))

initData :: SavedLocationScreenState
initData = {
  data: {
    savedLocations : []
  , deleteTag : Nothing
  }
  , props : {
      showDeleteLocationModel : false
    , apiRespReceived : false
  }
}