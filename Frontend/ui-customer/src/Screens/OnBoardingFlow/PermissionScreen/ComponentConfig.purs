{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OnBoardingFlow.PermissionScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((==))
import PrestoDOM (Length(..), Margin(..))
import Styles.Colors as Color
import Common.Types.App

errorModalConfig :: ErrorModal.Config 
errorModalConfig = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_offline,https://assets.juspay.in/nammayatri/images/common/ny_ic_offline.png"
      , height = V 124
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString YOU_ARE_OFFLINE)
      , margin = (MarginBottom 7)  
      , color = Color.black900
      , fontStyle = FontStyle.medium LanguageStyle
      }
    , errorDescriptionConfig {
        text = (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
      , color = Color.black700
      , fontStyle =  FontStyle.regular LanguageStyle
      }
    , buttonConfig {
        text = (getString TRY_AGAIN)
      , margin = (Margin 16 0 16 24)
      , background = Color.black900
      , color = Color.yellow900
      , fontStyle = FontStyle.medium LanguageStyle
      }
    }
  in errorModalConfig' 

primaryButtonConfig :: PrimaryButton.Config 
primaryButtonConfig  = let
    config' = PrimaryButton.config 
    primaryButtonConfig' = config' 
      { textConfig 
        { text = if EHC.os == "IOS" then (getString CONTINUE) else (getString GRANT_ACCESS)
        , fontStyle = FontStyle.bold LanguageStyle
        , textSize = FontSize.a_16
        , color = Color.yellow900
        }
      , width = MATCH_PARENT 
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , id = "PermissionScreenButton"
      , enableLoader = (JB.getBtnLoader "PermissionScreenButton")
      }
  in primaryButtonConfig'