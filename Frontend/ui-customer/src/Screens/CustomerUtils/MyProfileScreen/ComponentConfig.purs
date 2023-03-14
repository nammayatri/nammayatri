module Screens.CustomerUtils.MyProfileScreen.ComponentConfig where

import Screens.Types as ST 
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..))
import Styles.Colors as Color
import Common.Types.App

genericHeaderConfig :: ST.MyProfileScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , prefixImageConfig {
        height = (V 35)
      , width = (V 35)
      , margin = (Margin 10 17 16 15)
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/coomon/ny_ic_chevron_left.png"
      , padding = (Padding 5 5 5 5 )
      }
    , textConfig {
        text = if state.props.updateProfile then (getString UPDATE_PERSONAL_DETAILS) else (getString PERSONAL_DETAILS)
      , textSize = FontSize.a_18
      , color = Color.black
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    }
  in genericHeaderConfig'
  
nameEditTextConfig :: ST.MyProfileScreenState -> PrimaryEditText.Config
nameEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
        {
            margin = (Margin 16 30 16 0),
            topLabel {
                text = (getString NAME),
                textSize = FontSize.a_14,
                color = Color.black900,
                fontStyle = FontStyle.medium LanguageStyle
            },
            editText {
                text = state.data.name,
                textSize = FontSize.a_16,
                fontStyle = FontStyle.bold LanguageStyle,
                pattern = Just "[a-zA-Z ]*,30"
            },
            id = (EHC.getNewIDWithTag "NameEditText")
        }
    in primaryEditTextConfig'

updateButtonConfig :: ST.MyProfileScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    config = PrimaryButton.config
    updateButtonConfig' = config 
        { textConfig{ text = (getString UPDATE) }
        , height = (V 60)
        , cornerRadius = 0.0
        , margin = (Margin 0 0 0 0)
        , id = "PrimaryButtonUpdate"
        , enableLoader = (JB.getBtnLoader "PrimaryButtonUpdate")
        }
    in updateButtonConfig'

requestDeletePopUp :: ST.MyProfileScreenState -> PopUpModal.Config 
requestDeletePopUp state = let 
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { text =(getString DEL_ACCOUNT)},
      secondaryText { text = (getString ACCOUNT_DELETION_CONFIRMATION),
      padding = (Padding 36 0 36 0),
      color = Color.black600},
      option1 {
        text = (getString CANCEL_STR)
      , fontSize = FontSize.a_16
      },
      option2 {text = (getString YES_DELETE_IT)
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , fontSize = FontSize.a_16 }
     
    }
  in popUpConfig'

accountDeletedPopUp :: ST.MyProfileScreenState -> PopUpModal.Config 
accountDeletedPopUp state = let 
    config = PopUpModal.config 
    popUpConfig' = config {
      primaryText{ text= (getString REQUEST_SUBMITTED)},
      secondaryText{text = (getString WE_WILL_DELETE_YOUR_ACCOUNT),
      padding = (Padding 16 0 16 0),
      color = Color.black600},
      option1 {
        visibility = false
      },
      option2 {
        visibility = false
      }
    }
    in popUpConfig'