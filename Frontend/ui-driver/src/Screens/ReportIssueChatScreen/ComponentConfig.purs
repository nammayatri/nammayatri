{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.ComponentConfig where

import Prelude
import Components.PrimaryEditText.Controller as PrimaryEditText
import Data.Maybe (Maybe(..))
import Font.Style (medium) as FontStyle
import Font.Size (a_14, a_16, a_18) as FontSize
import Common.Types.App (LazyCheck(..))
import PrestoDOM.Properties (placeHolder, singleLine)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM (Padding(..), Margin(..), Gravity(..), Visibility(..), Length(..))
import Styles.Colors (black500, black700, black900, blue800, primaryButtonColor, white900) as Color
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PopUpModal as PopUpModal
import Screens.Types (ReportIssueChatScreenState)
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.PrimaryButton (config, Config) as PrimaryButton

primaryEditTextConfig :: String -> PrimaryEditText.Config
primaryEditTextConfig dummy = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { height = V 120
      , margin = (Margin 0 0 0 0)
      , id = getNewIDWithTag "submit_chat_edit_text"
      , editText
        { placeholder = (getString $ REPORT_ISSUE_CHAT_PLACEHOLDER "GET_READY_FOR_YS_SUBSCRIPTION")
        , singleLine = false
        }
      }
    in primaryEditTextConfig'

primaryButtonConfig :: String -> PrimaryButtonConfig.Config
primaryButtonConfig text = let
  config' = PrimaryButtonConfig.config
  primaryButtonConfig' =
    config'
      {textConfig
      { text = text
      , color = Color.black700}
      , background = Color.white900
      , stroke = "1," <> Color.black500
      , width = V ((screenWidth unit/2)-30)
      , id = "Button1"
      }
  in primaryButtonConfig'


addAudioModelConfig :: ReportIssueChatScreenState -> PopUpModal.Config
addAudioModelConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        {
          editTextVisibility = VISIBLE
        -- , dismissPopupConfig { visibility = VISIBLE, height = V 12, width = V 12, margin = (Margin 0 21 22 0), padding = (Padding 8 8 8 8) }
        , eTextConfig { editText { placeholder = (getString ENTER_YOUR_COMMENT), text = (getString ENTER_YOUR_COMMENT), color = Color.black900 }, margin = (Margin 16 16 16 0) }
        , primaryText { text = (getString ADD_A_COMMENT) }
        , secondaryText { visibility = GONE }
        -- , option1 { visibility = false }
        , option2
          { text = (getString POST_COMMENT)
          , background = Color.white900
          , color = Color.blue800
          , strokeColor = Color.white900
          , padding = (Padding 16 0 16 0)
          -- , isClickable = state.commentBtnActive
          }
        -- , cornerRadius = (Corners 15.0 true true true true)
        }
  in
    popUpConfig'
    
doneButtonConfig :: ReportIssueChatScreenState -> PrimaryButton.Config
doneButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = (getString PLACE_CALL)
      , color = Color.primaryButtonColor}
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 60)
      , id = "ReportIssueChatScreenDonePrimaryButton"
      }
  in primaryButtonConfig'

cancelButtonConfig :: ReportIssueChatScreenState -> PrimaryButton.Config
cancelButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = (getString CANCEL)
      , color = Color.black700}
      , cornerRadius = 8.0
      , background = Color.white900
      , stroke = "1,"<>Color.black700
      , height = (V 60)
      , id = "ReportIssueChatScreenCancelPrimaryButton"
      }
  in primaryButtonConfig'
