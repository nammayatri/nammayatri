module Screens.OnBoardingSubscriptionScreen.ComponentConfig where

import Common.Styles.Colors as Color
import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton as PrimaryButton
import Font.Style (Style(..))
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (OnBoardingSubscriptionScreenState)


joinPlanButtonConfig :: OnBoardingSubscriptionScreenState -> PrimaryButton.Config
joinPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = getString PAY_TO_JOIN_THIS_PLAN }
      , height = (V 48)
      , cornerRadius = 8.0
      , id = "StartTrialPrimaryButton"
      , enableLoader = (JB.getBtnLoader "StartTrialPrimaryButton")
      , margin = (MarginBottom 16)
      }
  in primaryButtonConfig'

popupModalConfig :: OnBoardingSubscriptionScreenState -> PopUpModalConfig.Config
popupModalConfig state = PopUpModalConfig.config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , backgroundClickable = true
      , buttonLayoutMargin = MarginBottom 0
      , optionButtonOrientation = "VERTICAL"
    ,primaryText { visibility = GONE},
      option1 {
        text = getString CALL_SUPPORT
      , color = Color.yellow900
      , background = Color.black900
      , visibility = true
      , margin = MarginTop 16
      , width = MATCH_PARENT
      , image {
          imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_phone_filled_yellow"
          , height = V 16
          , width = V 16
          , visibility = VISIBLE
          , margin = MarginRight 8
        }
      },
    secondaryText {
      text = getString NEED_HELP_JOINING_THE_PLAN
      , color = Color.black700
      , margin = Margin 16 4 16 0
      , textStyle = SubHeading1
      },
    option2 { 
      visibility = true
      , text = getString CANCEL
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.white900
      , width = MATCH_PARENT
      , margin = Margin 0 0 0 0
    },
    dismissPopup = true
    }