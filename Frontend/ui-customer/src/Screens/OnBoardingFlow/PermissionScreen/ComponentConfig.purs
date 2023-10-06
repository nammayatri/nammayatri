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
import Components.PopUpModal as PopUpModal
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((==), (/=), (<>), ($))
import PrestoDOM (Length(..), Margin(..), Gravity(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Common.Types.App
import Screens.Types (PermissionScreenState)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Commons (os)

errorModalConfig :: PermissionScreenState -> ErrorModal.Config 
errorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_offline," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_offline.png"
      , height = V 124
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString YOU_ARE_OFFLINE)
      , margin = (MarginBottom 7)  
      , color = Color.black900
      }
    , errorDescriptionConfig {
        text = (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
      , color = Color.black700
      }
    , buttonConfig {
        text = (getString TRY_AGAIN)
      , margin = (Margin 16 0 16 24)
      , background = state.appConfig.primaryBackground
      , color = state.appConfig.primaryTextColor
      }
    }
  in errorModalConfig' 

primaryButtonConfig :: PermissionScreenState -> PrimaryButton.Config 
primaryButtonConfig  state = let
    config' = PrimaryButton.config 
    primaryButtonConfig' = config' 
      { textConfig 
        { text = getString if EHC.os == "IOS" then CONTINUE
                  else if (getValueToLocalStore PERMISSION_POPUP_TIRGGERED) /= "true" then ALLOW_LOCATION_ACCESS else GRANT_ACCESS
        , textStyle = FontStyle.Body7
        , accessibilityHint =  (getString if EHC.os == "IOS" then CONTINUE
                  else if (getValueToLocalStore PERMISSION_POPUP_TIRGGERED) /= "true" then ALLOW_LOCATION_ACCESS else GRANT_ACCESS) <> " Button"
        , color = state.appConfig.primaryTextColor
        }
      , width = MATCH_PARENT 
      , background = state.appConfig.primaryBackground
      , margin = (Margin 0 0 0 0)
      , id = "PermissionScreenButton"
      , enableLoader = (JB.getBtnLoader "PermissionScreenButton")
      }
  in primaryButtonConfig'

getLocationBlockerPopUpConfig :: PermissionScreenState -> PopUpModal.Config 
getLocationBlockerPopUpConfig state = let 
  config = PopUpModal.config 
  config' = config{
    cornerRadius = Corners 15.0 true true true true
  , gravity = CENTER
  , margin = MarginHorizontal 24 24
  , buttonLayoutMargin = Margin 0 0 0 0 
  , padding = Padding 16 20 16 20
  , topTitle {
      text = getString ENABLE_LOCATION_PERMISSION_TO
    , height = WRAP_CONTENT
    , width = MATCH_PARENT 
    , gravity = CENTER
    , visibility = VISIBLE
    }
  , primaryText {
      visibility = GONE
    }
  , secondaryText {
      text = getString PLEASE_ENABLE_LOCATION_PERMISSION
    , margin = MarginTop 20
    , padding = Padding 0 0 0 0
    }
  , coverImageConfig {
      imageUrl = if (os == "IOS") then "ny_ic_enable_location_in_settings_ios," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_enable_location_in_settings_ios.png"
                 else "ny_ic_enable_location_in_settings_android," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_enable_location_in_settings_android.png"
    , visibility = VISIBLE
    , width = V 280
    , height = V 200
    , margin = Margin 0 0 0 0 
    }
  , option1 {
      visibility = false
    }
  , option2 {
      visibility = false
    }
  }
  in config'