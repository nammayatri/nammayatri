{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.HelpAndSupportScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.SourceToDestination as SourceToDestination
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Gravity(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Engineering.Helpers.Commons (os)
import Prelude
import Components.PrimaryEditText as PrimaryEditText
import Engineering.Helpers.Commons as EHC
import Data.Maybe (Maybe(..))
import Data.String as DS
import Components.PrimaryButton as PrimaryButton
import Storage (getValueToLocalStore, KeyStore(..))
import Helpers.Utils (validateEmail)
import Screens.HelpAndSupportScreen.Controller (isEmailPresent)

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
        , fontStyle = FontStyle.semiBold LanguageStyle
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

deleteGenericHeaderConfig :: ST.HelpAndSupportScreenState -> GenericHeader.Config
deleteGenericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = Margin 12 12 12 12
      }
    , padding = PaddingVertical 5 5
    , textConfig {
        text = getString DEL_ACCOUNT
      , textSize = FontSize.a_18
      , color = Color.black900
      , fontStyle = FontStyle.bold LanguageStyle
      }
    }
  in genericHeaderConfig'


primaryEditTextConfigEmail :: ST.HelpAndSupportScreenState -> PrimaryEditText.Config
primaryEditTextConfigEmail state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = if isEmailPresent FunctionCall then Color.black600 else Color.black800
        , textSize = FontSize.a_14
        , fontStyle = FontStyle.medium LanguageStyle
        , margin = Margin 16 16 16 16
        , placeholder = "example@xyz.com"
        , text = if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else ""
        , enabled = not isEmailPresent FunctionCall
        }
      , background = Color.white900
      , topLabel
        { text = getString YOUR_EMAIL_ID <> "*"
        , textSize = FontSize.a_12
        , color = Color.black900
        , fontStyle = FontStyle.regular LanguageStyle
        }
      , showErrorLabel = not validateEmail state.data.email && DS.length state.data.email > 0
      , errorLabel
        { text = getString PLEASE_ENTER_A_VALID_EMAIL
        , fontStyle = FontStyle.regular LanguageStyle
        , color = Color.textDanger }
      , margin = Margin 10 32 10 0
      }
    in primaryEditTextConfig'

primaryEditTextConfigDescription :: ST.HelpAndSupportScreenState -> PrimaryEditText.Config
primaryEditTextConfigDescription state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { color = Color.black800
        , textSize = FontSize.a_14
        , fontStyle = FontStyle.medium LanguageStyle
        , margin = if EHC.os == "IOS" then Margin 10 16 10 10 else Margin 16 16 16 16
        , singleLine = false
        , placeholder = getString YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE
        , pattern = Just "[A-Za-z0-9,. ]*,300"
        }
      , background = Color.white900
      , height = V 120
      , stroke = "1," <> if DS.length state.data.description >= 300 then Color.textDanger else Color.borderColorLight
      , topLabel
        { text = getString REASON_FOR_DELETING_ACCOUNT
        , textSize = FontSize.a_12
        , color = Color.black900
        , fontStyle = FontStyle.regular LanguageStyle
        }
      , margin = Margin 10 32 10 0
      , showErrorLabel = DS.length state.data.description >= 300
      , errorLabel
        { text = getString MAX_CHAR_LIMIT_REACHED <> " 300 " <> getString OF <> " 300"
        , fontStyle = FontStyle.regular LanguageStyle
        , color = Color.textDanger
        }
      }
    in primaryEditTextConfig'

primaryButtonConfigSubmitRequest :: ST.HelpAndSupportScreenState -> PrimaryButton.Config
primaryButtonConfigSubmitRequest state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = getString SUBMIT_REQUEST
        , color = if state.props.btnActive then Color.yellowRadler else Color.yellow800
        }
      , cornerRadius = 8.0
      , background = if state.props.btnActive then Color.black900 else Color.black500
      , isClickable = state.props.btnActive
      , margin = Margin 16 0 16 38
      , id = "ButtonDeleteAccount"
      }
  in primaryButtonConfig'

requestDeletePopUp :: ST.HelpAndSupportScreenState -> PopUpModal.Config
requestDeletePopUp state = let
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { text = getString DEL_ACCOUNT },
      secondaryText { text = getString ACCOUNT_DELETION_CONFIRMATION,
      padding = PaddingHorizontal 36 36,
      color = Color.black600},
      option1 {
        text = getString CANCEL_STR
      , fontSize = FontSize.a_16
      },
      option2 {text = getString YES_DELETE_IT
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , fontSize = FontSize.a_16 }

    }
  in popUpConfig'

accountDeletedPopUp :: ST.HelpAndSupportScreenState -> PopUpModal.Config
accountDeletedPopUp state = let
    config = PopUpModal.config
    popUpConfig' = config {
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
      }
    }
    in popUpConfig'

