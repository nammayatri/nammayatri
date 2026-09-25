module Screens.DocumentDetailsScreen.ScreenData where

import Screens.Types (DocumentDetailsScreenState)
import ConfigProvider

initData :: DocumentDetailsScreenState
initData = {
  data : {
    config: getAppConfig appConfig
  },
  props: { 

  }
}