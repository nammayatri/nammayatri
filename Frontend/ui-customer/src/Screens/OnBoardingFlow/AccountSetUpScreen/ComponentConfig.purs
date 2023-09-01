{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import Prelude ((/=), negate)
import PrestoDOM (Length(..), Margin(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Animation.Config (AnimConfig, animConfig)
import PrestoDOM.Animation as PrestoAnim
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Prelude ((<>))

primaryButtonConfig :: ST.AccountSetUpScreenState -> PrimaryButton.Config
primaryButtonConfig state = PrimaryButton.config
  { textConfig { text = "Let’s go!" 
    , color = state.data.config.primaryTextColor
    , accessibilityHint = "Let's go : Button"}
  , isClickable = state.props.btnActive
  , alpha = if state.props.btnActive then 1.0 else 0.4
  , margin = (Margin 0 0 0 0)
  , enableLoader = (JB.getBtnLoader "AccountSetupScreen")
  , id = "AccountSetupScreen"
  , background = state.data.config.primaryBackground
  }

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
          , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
          , margin = (Margin 12 12 12 12)
          }
        , background = Color.white900
        }
  in
    genericHeaderConfig'

goBackPopUpModelConfig :: ST.AccountSetUpScreenState -> PopUpModal.Config
goBackPopUpModelConfig state =
  let
    config' = PopUpModal.config

    popUpConfig =
      config'
        { primaryText { text = (getString GO_BACK_) }
        , secondaryText { text = (getString REGISTER_USING_DIFFERENT_NUMBER) }
        , option1 { 
            background = state.data.config.popupBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , text = (getString NO)
          }
        , option2 { 
            color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , text = (getString YES)
          }
        }
  in
    popUpConfig

translateFullYAnimWithDurationConfigs :: ST.AccountSetUpScreenState -> AnimConfig
translateFullYAnimWithDurationConfigs state = animConfig {
  fromScaleY = if state.props.genderOptionExpanded then 0.0 else 1.0
, toScaleY =if state.props.genderOptionExpanded then 1.0 else 0.0
, fromY = if state.props.genderOptionExpanded then -100 else  0
, toY = if state.props.genderOptionExpanded then 0 else -100
, repeatCount = (PrestoAnim.Repeat 0)
, ifAnim = state.props.expandEnabled
, duration = 200
}