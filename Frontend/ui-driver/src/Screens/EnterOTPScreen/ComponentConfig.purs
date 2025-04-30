{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Styles.Colors as Color
import Language.Strings
import PrestoDOM

import Common.Types.App as Common
import Language.Types (STR(..))
import Resource.Constants as Constant
import Prelude ((<>))
import Data.Maybe (Maybe(..))
import Font.Style as FontStyle
import JBridge as JB
import Common.Types.App
import Font.Size as FontSize
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Screens.Types as ST

primaryButtonViewConfig :: ST.EnterOTPScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString REGISTER) }
      , id = "PrimaryButtonOtpNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.6
      , height = (V 50)
      , cornerRadius = 12.0
      , margin = (Margin 16 0 16 0)
      }
  in primaryButtonConfig'


verifyOTPButtonConfig :: ST.EnterOTPScreenState -> PrimaryButton.Config
verifyOTPButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CONTINUE) }
      , id = "PrimaryButtonOTP"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.4
      , margin = (Margin 0 0 0 0 )
      , enableLoader = (JB.getBtnLoader "PrimaryButtonOTP")
      }
  in primaryButtonConfig'
