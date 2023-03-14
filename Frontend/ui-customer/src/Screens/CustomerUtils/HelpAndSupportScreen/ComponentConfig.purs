module Screens.CustomerUtils.HelpAndSupportScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.SourceToDestination as SourceToDestination
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App

sourceToDestinationConfig :: ST.HelpAndSupportScreenState -> SourceToDestination.Config
sourceToDestinationConfig state = let 
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = (Margin 0 13 16 0)
    , width = MATCH_PARENT
    , lineMargin = (Margin 4 6 0 0)
    , sourceMargin = (Margin 0 0 0 14)
    , sourceImageConfig {
        imageUrl = "ny_ic_green_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_green_circle.png"
      , margin = (MarginTop 5)
      }
    , sourceTextConfig {
        text = state.data.source
      , textSize = FontSize.a_13
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.medium LanguageStyle
      , ellipsize = true
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
      , margin = (MarginTop 4)
      }
    , destinationTextConfig {
        text = state.data.destination
      , textSize = FontSize.a_13
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.medium LanguageStyle
      , maxLines = 1
      , ellipsize = true
      }
    }
  in sourceToDestinationConfig'

apiErrorModalConfig :: ST.HelpAndSupportScreenState -> ErrorModal.Config 
apiErrorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_error_404,https://assets.juspay.in/nammayatri/images/user/ny_ic_error_404.png"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString ERROR_404)
      , margin = (MarginBottom 7)  
      , color = Color.black800
      , textSize = FontSize.a_18
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , errorDescriptionConfig {
        text = (getString PROBLEM_AT_OUR_END)
      , color = Color.black700
      , textSize = FontSize.a_14
      , margin = (Margin 16 0 16 0)
      , fontStyle =  FontStyle.regular LanguageStyle
      }
    , buttonConfig {
        text = (getString NOTIFY_ME)
      , margin = (Margin 16 0 16 16)
      , background = Color.black900
      , color = Color.yellow900
      , fontStyle = FontStyle.medium LanguageStyle
      , textSize = FontSize.a_16
      }
    }
  in errorModalConfig' 

callConfirmationPopup :: ST.HelpAndSupportScreenState -> PopUpModal.Config 
callConfirmationPopup state = let 
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { 
        text = (getString CONTACT_SUPPORT) 
      , margin = (Margin 0 20 0 20)
        },
      secondaryText { 
        visibility = GONE
        },
      option1 {
        text = (getString GO_BACK_)
      , fontSize = FontSize.a_16
      },
      option2 {
        text = (getString CALL)
      , fontSize = FontSize.a_16 
      }
    }
  in popUpConfig'

genericHeaderConfig :: ST.HelpAndSupportScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12)
      } 
    , padding = (Padding 0 5 0 5)
    , textConfig {
        text = (getString HELP_AND_SUPPORT)
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'
  
