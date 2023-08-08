{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SubscriptionScreen.ComponentConfig where

import Language.Strings
import PrestoDOM

import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton as PrimaryButton
import Data.Maybe as Mb
import Font.Style (Style(..))
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (SubscribePopupType(..))
import Screens.Types as ST
import Styles.Colors as Color

clearDueButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
clearDueButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Clear Dues (₹100)" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 12 0 12)
      }
  in primaryButtonConfig'

switchPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
switchPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Switch to DAILY UNLIMITED plan" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      }
  in primaryButtonConfig'

resumeAutopayButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
resumeAutopayButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Resume Autopay" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      }
  in primaryButtonConfig'

joinPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
joinPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Join Plan" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      }
  in primaryButtonConfig'

pupupModalConfig:: ST.SubscriptionScreenState -> PopUpModalConfig.Config
pupupModalConfig state = let
    config = PopUpModalConfig.config
    popUpConf' = config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , dismisText = case state.props.popUpState of
                      Mb.Just CancelAutoPay -> Mb.Just "Cancel Autopay and Pay Manually"
                      _ -> Mb.Nothing
      , buttonLayoutMargin = MarginBottom 0
    ,primaryText {
        text = case state.props.popUpState of
                  Mb.Just SuccessPopup -> "Plan Activated Successfully"
                  Mb.Just FailedPopup -> "Payment Failed"
                  Mb.Just DuesClearedPopup -> "Dues Cleared Successfully"
                  Mb.Just CancelAutoPay -> "Not planning to take rides?"
                  Mb.Nothing -> ""
      , margin = (Margin 16 32 16 24)
      , visibility = VISIBLE
      , color = Color.black800
      , textStyle = Heading2
     },
      option1 {
        text = case state.props.popUpState of
                  Mb.Just SuccessPopup -> getString GOT_IT
                  Mb.Just FailedPopup -> "Retry Payment"
                  Mb.Just DuesClearedPopup -> getString GOT_IT
                  Mb.Just CancelAutoPay -> "Pause AutoPay"
                  Mb.Nothing -> ""
      , color = Color.yellow900
      , background = Color.black
      , visibility =true
      },
      coverImageConfig {
        imageUrl =  case state.props.popUpState of
          Mb.Just SuccessPopup -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just FailedPopup -> "ny_ic_payment_failed_banner,"
          Mb.Just DuesClearedPopup -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just CancelAutoPay -> "ny_ic_pause_autopay,"
          Mb.Nothing -> ""
      , visibility = VISIBLE
      , width = V 114
      , height = V 114
      },
      secondaryText {
        text = ""
      , color = Color.black600
      , margin = Margin 24 0 24 32
      , visibility = GONE
        },
      option2 {text = ""
      , color = Color.yellow900
      , strokeColor = Color.white900
      , margin = Margin 16 0 16 0
      , width = V 50
      , visibility =false
      , background = Color.black900
      }
    }
  in popUpConf'