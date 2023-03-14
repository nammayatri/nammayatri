module Screens.RideHistoryScreen.ComponentConfig where


import Components.ErrorModal as ErrorModal
import Common.Types.App
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Styles.Colors as Color
import Screens.Types as ST

errorModalConfig :: ErrorModal.Config 
errorModalConfig = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_no_past_rides,https://assets.juspay.in/nammayatri/images/common/ny_ic_no_past_rides.png"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 61)
      }
    , errorConfig {
        text = (getString EMPTY_RIDES)
      , margin = (MarginBottom 7)  
      , color = Color.black900
      , textSize = FontSize.a_18
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , errorDescriptionConfig {
        text = (getString YOU_HAVE_NOT_TAKEN_A_TRIP_YET)
      , color = Color.black700
      , textSize = FontSize.a_14
      , fontStyle =  FontStyle.regular LanguageStyle
      }
    , buttonConfig {
        text = (getString BOOK_NOW)
      , margin = (Margin 16 0 16 24)
      , background = Color.black900
      , color = Color.yellow900
      , fontStyle = FontStyle.medium LanguageStyle
      , textSize = FontSize.a_16
      , visibility  = GONE
      }
    }
  in errorModalConfig' 