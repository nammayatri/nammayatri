module Screens.AccountSetUpScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((/=))
import PrestoDOM (Length(..), Margin(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App

primaryButtonConfig :: ST.AccountSetUpScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig { text = (getString CONTINUE) }
        , isClickable = state.props.btnActive
        , alpha = if state.props.btnActive then 1.0 else 0.4
        , margin = (Margin 0 0 0 0)
        , enableLoader = (JB.getBtnLoader "AccountSetupScreen")
        , id = "AccountSetupScreen"
        }
  in
    primaryButtonConfig'

primaryEditTextConfigName :: ST.AccountSetUpScreenState -> PrimaryEditText.Config
primaryEditTextConfigName state =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { color = Color.black800
          , placeholder = (getString ENTER_YOUR_NAME)
          , singleLine = true
          , fontStyle = if state.data.name /= "" then FontStyle.medium LanguageStyle else FontStyle.regular LanguageStyle
          , textSize = FontSize.a_14
          , pattern = Just "[a-zA-Z ]*,30"
          }
        , background = Color.white900
        , margin = (Margin 0 30 0 0)
        , topLabel
          { textSize = FontSize.a_12
          , text = (getString FULL_NAME)
          , color = Color.greyTextColor
          , fontStyle = FontStyle.regular LanguageStyle
          , alpha = 0.8
          }
        }
  in
    primaryEditTextConfig'

genericHeaderConfig :: GenericHeader.Config
genericHeaderConfig =
  let
    config = GenericHeader.config

    genericHeaderConfig' =
      config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
          , margin = (Margin 12 12 12 12)
          }
        }
  in
    genericHeaderConfig'

goBackPopUpModelConfig :: PopUpModal.Config
goBackPopUpModelConfig =
  let
    config' = PopUpModal.config

    popUpConfig =
      config'
        { primaryText { text = (getString GO_BACK_) }
        , secondaryText { text = (getString REGISTER_USING_DIFFERENT_NUMBER) }
        , option1 { text = (getString NO) }
        , option2 { text = (getString YES) }
        }
  in
    popUpConfig
