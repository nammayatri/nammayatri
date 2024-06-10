module Screens.DeleteAccountScreen.ComponentConfig where

import Screens.DeleteAccountScreen.ScreenData
import Components.PopUpModal as PopUpModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Engineering.Helpers.Commons as EHC
import Styles.Colors as Color
import Prelude
import PrestoDOM
import Language.Strings (getString)
import Font.Style as FontStyle
import Language.Types (STR(..))
import Helpers.Utils
import Data.Maybe
import JBridge (validateEmail)
import Data.String as DS
import ConfigProvider

genericHeaderConfig :: State -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = Margin 8 8 8 8
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      }
    , padding = PaddingVertical 5 5
    , textConfig {
        text = getString DEL_ACCOUNT
      , color = Color.black900
      }
    }
  in genericHeaderConfig'


primaryButtonConfig :: State -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = getString SUBMIT_REQUEST
        }
      , cornerRadius = 8.0
      , margin = Margin 16 0 16 (if EHC.safeMarginBottom == 0 then 24 else (EHC.safeMarginBottom))
      , id = "DeleteAccountButton"
      }
  in primaryButtonConfig'


emailPrimaryEditTextConfig :: State -> PrimaryEditText.Config
emailPrimaryEditTextConfig state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , margin = Margin 16 16 16 16
        , placeholder = "example@xyz.com"
        , textStyle = FontStyle.Body1
        }
      , background = Color.white900
      , topLabel
        { text = getString YOUR_EMAIL_ID
        , color = Color.black900
        }
      , showErrorLabel = not validateEmail state.data.email && DS.length state.data.email > 0
      , errorLabel
        { text = getString PLEASE_ENTER_A_VALID_EMAIL 
        , color = Color.textDanger }
      , margin = Margin 10 32 10 0
      }
    in primaryEditTextConfig'

descriptionPrimaryEditTextConfig :: State -> PrimaryEditText.Config
descriptionPrimaryEditTextConfig state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , margin = if EHC.os == "IOS" then Margin 10 16 10 10 else Margin 16 16 16 16
        , singleLine = false
        , placeholder = getString YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE
        , pattern = Just "[A-Za-z0-9,. ]*,300"
        , textStyle = FontStyle.Body1
        }
      , background = Color.white900
      , height = V 120
      , stroke = "1," <> if DS.length state.data.description >= 300 then Color.textDanger else Color.borderColorLight
      , topLabel
        { text = getString REASON_FOR_DELETING_ACCOUNT
        , color = Color.black900
        }  
      , margin = Margin 10 32 10 0
      , showErrorLabel = DS.length state.data.description >= 300
      , errorLabel
        { text = getString MAX_CHAR_LIMIT_REACHED <> " 300 " <> getString OF <> " 300"
        , color = Color.textDanger
        }
      }
    in primaryEditTextConfig'

requestDeletePopUp :: State -> PopUpModal.Config
requestDeletePopUp state = let
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { text = getString DEL_ACCOUNT },
      secondaryText { text = getString ACCOUNT_DELETION_CONFIRMATION,
      padding = PaddingHorizontal 36 36,
      color = Color.black600},
      option1 {
        text = getString CANCEL
      },
      option2 {text = getString YES_DELETE
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red }
     
    }
  in popUpConfig'

requestSubmittedPopUp :: State -> PopUpModal.Config
requestSubmittedPopUp state = do 
    let config = getAppConfig appConfig
    PopUpModal.config {
      primaryText{ text = getString REQUEST_SUBMITTED},
      secondaryText{text = getString WE_WILL_DELETE_YOUR_ACCOUNT,
      padding = PaddingHorizontal 16 16,
      color = Color.black600},
      option1 {
        visibility = false
      },
      option2 {
        text = getString OKAY_GOT_IT,
        margin = MarginHorizontal 16 16
      , background = config.primaryBackground
      , strokeColor = config.primaryBackground
      , color = config.primaryTextColor
      }
    }