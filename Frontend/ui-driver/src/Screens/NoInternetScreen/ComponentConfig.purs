{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NoInternetScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Styles.Colors as Color
import Engineering.Helpers.MobilityPrelude(isStrEmpty)

primaryButtonConfig :: String -> PrimaryButton.Config 
primaryButtonConfig triggertype = let
    config' = PrimaryButton.config 
    primaryButtonConfig' = config' 
      { textConfig 
        { text = if triggertype == "INTERNET_ACTION" then (getString TRY_AGAIN) else (getString GRANT_ACCESS)
        , color = Color.yellow900
        }
      , width = MATCH_PARENT 
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , id = "NoInternetScreenPrimaryButton"
      }
  in primaryButtonConfig'