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
import Resources.LocalizableV2.Strings (getStringV2)
import Helpers.Utils as HU
import Resources.LocalizableV2.Types

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
  , margin = MarginTop 24
  }
, secondaryText {
    text = getString COLLECT_TOLL_SEP 
  , margin = MarginTop 8
  }
, option1{
  visibility = false
}
, option2 {
    text = getString OK_GOT_IT 
  , margin = MarginTop 24
  }
}


tollChargesIncluded :: HomeScreenState -> PopUpModal.Config
tollChargesIncluded lazyCheck = PopUpModal.config{
  margin = MarginHorizontal 24 24
, padding =  Padding 16 20 16 20
, gravity = CENTER
, buttonLayoutMargin = Margin 0 0 0 0
, cornerRadius = Corners 24.0 true true true true
, coverImageConfig{
    imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_toll_charges_included"
  , visibility = VISIBLE
  , height =  V 200
  , width = V $ (screenWidth unit) - 80
  }
, primaryText {
    text = getString TOLL_CHARGES_INCLUDED_IN_FAIR
  , color = Color.black800
  , margin = MarginTop 24
  }
, secondaryText {
    text = getString PLEASE_DO_NOT_PAY_EXTRA_TO_DRIVER
  , margin = MarginTop 8
  }
, option1{
  visibility = false
}
, option2 {
    text = getString OK_GOT_IT
  , margin = MarginTop 24
  }
}

intercityBusPhoneNumberPermission :: HomeScreenState -> PopUpModal.Config
intercityBusPhoneNumberPermission state = PopUpModal.config{
  margin = MarginHorizontal 24 24
, padding =  Padding 16 20 16 20
, gravity = CENTER
, buttonLayoutMargin = Margin 0 0 0 0
, cornerRadius = Corners 24.0 true true true true
, backgroundClickable = true
, dismissPopup = true
, coverImageConfig{
    imageUrl = fetchImage APP_ASSET "ny_ic_rs_phone_permission"
  , visibility = VISIBLE
  , height =  V 83
  , width = V $ (screenWidth unit) - 80
  }
, primaryText {
    text = getString PHONE_NUMBER_PERMISSION 
  , color = Color.black800
  , margin = Margin 0 24 0 0
  }
, secondaryText {
    text = getString PHONE_NUMBER_PERMISSION_DESC 
  , margin = Margin 0 8 0 0
  }
, option1{
    text = getString DENY
    , margin = Margin 0 24 0 0
    , width = V  $ (screenWidth unit)/2 - 60
  }
, option2 {
    text = getString ALLOW
  , margin = Margin 12 24 0 0
  , width = V $ (screenWidth unit)/2 - 60
  }
}
bookAmbulanceModalConfig :: HomeScreenState -> PopUpModal.Config
bookAmbulanceModalConfig state = let
    config = PopUpModal.config
    popUpConf' = config {
      cornerRadius = Corners 15.0 true true true true,
      margin = Margin 16 16 16 16 ,
      gravity = CENTER,
      optionButtonOrientation = "VERTICAL",
      padding = Padding 16 0 16 16,
      buttonLayoutMargin = Margin 0 0 0 0,

     primaryText {
         text = getStringV2 ambulance_booking_disclaimer
        , margin = Margin 16 0 16 0
        , visibility = VISIBLE
        , gravity = CENTER
      },
      secondaryText {
        visibility = GONE
      },
      option1 {
        text = getStringV2 a_c
      , color = Color.yellow900
      , strokeColor = Color.white900
      , padding = Padding 15 10 15 10
      , visibility = true
      , margin = MarginTop 12
      , background = Color.black900
      , width = MATCH_PARENT
      , gravity = CENTER
      },
      option2 {
        text = getString CANCEL_BOOKING
      , color = Color.black900
      , strokeColor = Color.white900
      , padding = Padding 15 10 15 10
      , margin = MarginTop 0
      , width = MATCH_PARENT
      , background = Color.white900
      , gravity = CENTER
      },
      coverImageConfig {
           imageUrl =  HU.fetchImage HU.FF_ASSET "ny_ic_ambulance_agree"
          , visibility = VISIBLE
          , height = V 250
          , width = MATCH_PARENT
      }

    }
  in popUpConf'
