{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Prelude ((<>))
import MerchantConfig.Types (Language)

primaryButtonConfig :: ST.SelectLanguageScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
         { text = (getString UPDATE)
         , accessibilityHint = if state.props.btnActive then "Update Button" else "Update Button Disabled: Change Language To Enable "
         , color = state.data.config.primaryTextColor
         } 
        , isClickable = state.props.btnActive
        , alpha = if state.props.btnActive then 1.0 else 0.6
        , margin = (Margin 0 0 0 0)
        , id = "UpdateLanguageButton"
        , enableLoader = (JB.getBtnLoader "UpdateLanguageButton")
        , background = state.data.config.primaryBackground
      }
  in primaryButtonConfig'

menuButtonConfig :: ST.SelectLanguageScreenState -> Language -> MenuButton.Config
menuButtonConfig state language = MenuButton.config {
      titleConfig{
          text = language.name
        , selectedTextStyle = FontStyle.ParagraphText
        , unselectedTextStyle = FontStyle.ParagraphText
       }
      , accessibilityHint = language.subTitle
      ,subTitleConfig
      {
        text = language.subTitle
      }
      , id = language.value
      , isSelected = (language.value == state.props.selectedLanguage)
      , radioButtonConfig {
        activeStroke = "2," <> state.data.config.primaryBackground
      , buttonColor = state.data.config.primaryBackground
      }
    }

genericHeaderConfig :: ST.SelectLanguageScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
      } 
    , textConfig {
        text = (getString LANGUAGE)
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'