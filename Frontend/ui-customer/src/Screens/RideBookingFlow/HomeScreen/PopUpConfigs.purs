module Screens.HomeScreen.PopUpConfigs where

import Prelude
import Common.Styles.Colors as Color
import Debug
import Components.PopUpModal as PopUpModal
import PrestoDOM
import Screens.Types
import PrestoDOM.Types.DomAttributes
import Helpers.Utils 
import Engineering.Helpers.Commons 
import Language.Strings
import Language.Types


finalFareExcludesToll :: HomeScreenState -> PopUpModal.Config
finalFareExcludesToll lazyCheck = PopUpModal.config{
  margin = MarginHorizontal 24 24
, padding =  Padding 16 20 16 20
, gravity = CENTER
, buttonLayoutMargin = Margin 0 0 0 0
, cornerRadius = Corners 24.0 true true true true
, coverImageConfig{
    imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_toll_charges_excluded"
  , visibility = VISIBLE
  , height =  V 200
  , width = V $ (screenWidth unit) - 80
  }
, primaryText {
    text = getString FINAL_FARE_EXCLUDES_TOLL 
  , color = Color.black800
  , margin = Margin 0 24 0 0
  }
, secondaryText {
    text = getString COLLECT_TOLL_SEP 
  , margin = Margin 0 8 0 0
  }
, option1{
  visibility = false
}
, option2 {
    text = getString OK_GOT_IT 
  , margin = Margin 0 24 0 0
  }
}