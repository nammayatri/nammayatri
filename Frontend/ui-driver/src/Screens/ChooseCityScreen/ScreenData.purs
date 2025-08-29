{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.ChooseCityScreen.ScreenData where

import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Prelude (map, (<>))
import Common.Types.App (YoutubeData, CarouselData)
import Foreign.Object (empty)
import Data.Maybe as Mb
import Common.Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import ConfigProvider

initData :: ChooseCityScreenState
initData = {
  data: {
    config : getAppConfig appConfig,
    locationSelected : Mb.Nothing,
    merchantOperatingCityConfig : [],
    logField : empty
  },
  props : {
    selectedLanguage: "EN_US",
    currentStage : ENABLE_PERMISSION,
    isLocationPermissionGiven : false,
    radioMenuFocusedLang : "",
    radioMenuFocusedCity : "",
    locationUnserviceable : false,
    locationDetectionFailed : false,
    isMockLocation : false,
    lat : 0.0,
    lon : 0.0,
    goBackToAddVehiclesScreen : false
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
  , showFullScreen : false
  , hideFullScreenButton : false
  }