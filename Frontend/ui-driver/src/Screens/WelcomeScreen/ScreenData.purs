module Screens.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)
import Foreign.Object (empty)
import Common.Styles.Colors as Color
import Prelude (map)
import Common.Types.App (YoutubeData, CarouselData)
import Language.Strings (getString)
import Language.Types(STR(..))
import ConfigProvider

initData :: WelcomeScreenState
initData = {
  data : { 
    logField : empty,
    config : getAppConfig appConfig
  }
}
