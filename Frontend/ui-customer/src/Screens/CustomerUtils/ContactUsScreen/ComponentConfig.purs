{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerUtils.ContactUsScreen.ComponentConfig where

import Common.Types.App
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), isJust)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.ContactUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Debug

primaryButtonConfigSubmit :: ST.ContactUsScreenState -> PrimaryButton.Config
primaryButtonConfigSubmit state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString SUBMIT)
          , color = state.data.config.primaryTextColor
          , accessibilityHint = if state.props.btnActive then "Submit : Button" else "Submit Button Is Disabled"
          }
        , cornerRadius = 0.0
        , background = state.data.config.primaryBackground
        , isClickable = state.props.btnActive
        , alpha = if state.props.btnActive then 1.0 else 0.5
        , margin = (Margin 0 0 0 0)
        , id = "SubmitButtonContactUsScreen"
        , enableLoader = (JB.getBtnLoader "SubmitButtonContactUsScreen")
        }
  in
    primaryButtonConfig'

primaryEditTextConfigDescription :: ST.ContactUsScreenState -> PrimaryEditText.Config
primaryEditTextConfigDescription state =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { color = Color.black800
          , textStyle = FontStyle.Body1
          , margin = if EHC.os == "IOS" then (Margin 10 16 10 10) else (Margin 16 16 16 16)
          , singleLine = false
          , placeholder = (getString YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE)
          , pattern = Just "[A-Za-z0-9,. ]*,300"
          }
        , background = Color.white900
        , height = V 120
        , stroke = if (DS.length state.data.description >= 300) then ("1," <> Color.textDanger) else ("1," <> Color.borderColorLight)
        , topLabel
          { text = (getString DESCRIBE_YOUR_ISSUE)
          , color = Color.black800
          }
        , margin = (Margin 10 32 10 0)
        , showErrorLabel = (DS.length state.data.description >= 300)
        , errorLabel
          { text = ((getString MAX_CHAR_LIMIT_REACHED) <> " 300 " <> (getString OF) <> " 300")
          , color = Color.textDanger
          }
        }
  in
    primaryEditTextConfig'

primaryEditTextConfigEmail :: ST.ContactUsScreenState -> PrimaryEditText.Config
primaryEditTextConfigEmail state =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { color = Color.black800
          , textStyle = FontStyle.Body1
          , margin = (Margin 16 16 16 16)
          , placeholder = "example@xyz.com"
          }
        , background = Color.white900
        , showErrorLabel = (isJust state.data.errorMessage)
        , topLabel
          { text = (getString YOUR_EMAIL_ID)
          , color = Color.black800
          }
        , margin = (Margin 10 32 10 0)
        , errorLabel
          { text =
            case state.data.errorMessage of
              Just ST.EMAIL_EXISTS -> "Email already exists"
              Just ST.INVALID_EMAIL -> "Please enter a valid email"
              Just ST.EMAIL_CANNOT_BE_BLANK -> "This field is required"
              _ -> ""
          , color = Color.textDanger
          }
        }
  in
    primaryEditTextConfig'

primaryEditTextConfig :: ST.ContactUsScreenState -> PrimaryEditText.Config
primaryEditTextConfig state =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { color = Color.black800
          , textStyle = FontStyle.Body1
          , margin = if EHC.os == "ANDROID" then (Margin 16 4 16 4) else (Margin 16 16 16 4)
          , gravity = CENTER_VERTICAL
          , placeholder = (getString ACTUAL_FARE_WAS_HIGHER_THAN_WHAT_WAS_SHOWN)
          , pattern = Just "[a-zA-Z0-9 ]*,100"
          , padding = (Padding 0 0 0 0)
          , singleLine = false
          }
        , height = (V 54)
        , margin = (Margin 10 32 10 0)
        , background = Color.white900
        , topLabel
          { text = (getString SUBJECT)
          , color = Color.black800
          }
        , errorLabel
          { text = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
          , color = Color.textDanger
          }
        , showErrorLabel = ((DS.length state.data.subject) >= 100)
        , stroke = if ((DS.length state.data.subject) >= 100) then ("1," <> Color.textDanger) else ("1," <> Color.borderColorLight)
        }
  in
    primaryEditTextConfig'

primaryButtonConfig :: ST.ContactUsScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString GO_TO_HOME__)
          , color = state.data.config.primaryTextColor
          }
        , cornerRadius = 0.0
        , background = state.data.config.primaryBackground
        , margin = (Margin 0 0 0 0)
        , id = "GotoHomeThankyouScreen"
        , enableLoader = (JB.getBtnLoader "GotoHomeThankyouScreen")
        }
  in
    primaryButtonConfig'

genericHeaderConfig :: ST.ContactUsScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config

    genericHeaderConfig' =
      spy "config "
        config
          { height = WRAP_CONTENT
          , prefixImageConfig
            { height = V 25
            , width = V 25
            , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            }
          , padding = (Padding 0 5 0 5)
          , textConfig
            { text = (getString WRITE_TO_US)
            , color = Color.darkCharcoal
            }
          , suffixImageConfig
            { visibility = GONE
            }
          }
  in
    genericHeaderConfig'
