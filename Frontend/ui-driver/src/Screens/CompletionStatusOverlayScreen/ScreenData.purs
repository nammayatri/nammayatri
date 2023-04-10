module Screens.CompletionStatusOverlayScreen.ScreenData where

import Screens.Types (CompletionStatusOverlayState)

initData :: CompletionStatusOverlayState
initData = {
  data: {
    time : 5000.0,
    mainText : "",
    subText : "",
    lottieUrl : "success_lottie"
  },
  props: {}
}
