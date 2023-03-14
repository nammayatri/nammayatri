module Screens.CustomerUtils.SelectLanguageScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((==))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST 
import Styles.Colors as Color
import Common.Types.App

primaryButtonConfig :: ST.SelectLanguageScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
         {
          text = (getString UPDATE)
         } 
        , isClickable = state.props.btnActive
        , alpha = if state.props.btnActive then 1.0 else 0.6
        , margin = (Margin 0 0 0 0)
        , id = "UpdateLanguageButton"
        , enableLoader = (JB.getBtnLoader "UpdateLanguageButton")
      }
  in primaryButtonConfig'

menuButtonConfig :: ST.SelectLanguageScreenState -> ST.Language -> MenuButton.Config
menuButtonConfig state language = let  
    config = MenuButton.config
    menuButtonConfig' = config {
      titleConfig{
          text = language.name
      }
      ,subTitleConfig
      {
        text = language.subTitle
      }
      , id = language.value
      , isSelected = (language.value == state.props.selectedLanguage)
    }
    in menuButtonConfig'

genericHeaderConfig :: ST.SelectLanguageScreenState -> GenericHeader.Config 
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
        text = (getString LANGUAGE)
      , textSize = FontSize.a_18
      , color = Color.black
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'