module Screens.DriverCompleteProfileScreen.ScreenData where

import Screens.Types (DriverCompleteProfileScreenState)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons as EHC

initData :: DriverCompleteProfileScreenState
initData = {
  data : {
    pledge : []
  , vehicalOffer : []
  , languages : []
  , whyNy : [] 
  , homeTown : Nothing
  , calendarState:
    { calendarPopup: false
    , endDate: Nothing
    , selectedTimeSpan: dummyDateItem
    , startDate: Just dummyDateItem
    , weeks: []
    }
  , drivingSince : Nothing
  , addImagesState: addImagesState'
  , viewImageState: { image : "", imageName : Nothing}
  , uploadedImagesIds: []
  , addedImages: []
  }
, props : {
  showImageModel : false,
  showViewImageModel : false
  }
}

addImagesState' = {
  images: [],
  stateChanged: false,
  isLoading: false,
  imageMediaIds: []
}

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }