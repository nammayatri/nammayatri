module Screens.ChooseCityScreen.ScreenData where

import MerchantConfig.DefaultConfig as DC
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Prelude (map)
import Common.Types.App (YoutubeData, CarouselData)
import Foreign.Object (empty)
import Common.Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))

initData :: ChooseCityScreenState
initData = {
  data : {
    config : DC.config,
    locationSelected : "--"
  },
  props : {
    selectedLanguage: "EN_US",
    currentStage : ENABLE_PERMISSION,
    isLocationPermissionGiven : false,
    radioMenuFocusedLang : "",
    radioMenuFocusedCity : ""
  }
}

dummyYoutubeData :: YoutubeData
dummyYoutubeData = { videoTitle: ""
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: ""
  , videoType: ""
  , videoHeight : 0
  }