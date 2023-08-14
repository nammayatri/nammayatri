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

import Common.Types.App (PaymentStatus(..))
import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Maybe as Mb
import Data.Semigroup ((<>))
import Font.Style (Style(..))
import Language.Types (STR(..))
import Prelude ((==), (/=), (&&))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (SubscribePopupType(..), PlanCardConfig(..))
import Screens.Types as ST
import Styles.Colors as Color

clearDueButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
clearDueButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString CLEAR_DUES) }
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
      { textConfig{ text = ((getString SWITCH_TO)<> " " <> (getSelectedAlternatePlan state) <> " " <> (getString PLAN)) }
      , isClickable = if state.data.myPlanData.planEntity.id == state.props.managePlanProps.selectedPlan then false else true
      , alpha = if state.data.myPlanData.planEntity.id == state.props.managePlanProps.selectedPlan then 0.6 else 1.0
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      }
  in primaryButtonConfig'

resumeAutopayButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
resumeAutopayButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString SETUP_AUTOPAY_STR) }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 16 16 16 16)
      }
  in primaryButtonConfig'

joinPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
joinPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = if state.props.paymentStatus == Mb.Just Pending then getString PAYMENT_PENDING else
                              case (getSelectedJoiningPlan state) of 
                                Mb.Just value -> ((getString PLANS)<> " " <> value <> " " <> (getString PLAN)) 
                                Mb.Nothing -> (getString TAP_A_PLAN_TO_VIEW_DETAILS) }
      , isClickable = if state.props.joinPlanProps.selectedPlan /= Mb.Nothing && state.props.paymentStatus /= Mb.Just Pending then true else false
      , alpha = if state.props.joinPlanProps.selectedPlan /=Mb.Nothing && state.props.paymentStatus /= Mb.Just Pending then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (MarginBottom 16)
      }
  in primaryButtonConfig'

pupupModalConfig :: ST.SubscriptionScreenState -> PopUpModalConfig.Config
pupupModalConfig state = let
    config = PopUpModalConfig.config
    popUpConf' = config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , backgroundClickable = false
      , dismisText = Mb.Nothing
      , buttonLayoutMargin = MarginBottom 0
    ,primaryText {
        text = case state.props.popUpState of
                  Mb.Just SuccessPopup -> (getString PLAN_ACTIVATED_SUCCESSFULLY)
                  Mb.Just FailedPopup -> (getString PAYMENT_FAILED)
                  Mb.Just DuesClearedPopup -> (getString DUES_CLEARED_SUCCESSFULLY)
                  Mb.Just CancelAutoPay -> (getString NOT_PLANNING_TO_TAKE_RIDES)
                  Mb.Nothing -> ""
      , margin = (Margin 16 32 16 24)
      , visibility = VISIBLE
      , color = Color.black800
      , textStyle = Heading2
     },
      option1 {
        text = case state.props.popUpState of
                  Mb.Just SuccessPopup -> getString GOT_IT
                  Mb.Just FailedPopup -> getString RETRY_PAYMENT_STR
                  Mb.Just DuesClearedPopup -> getString GOT_IT
                  Mb.Just CancelAutoPay -> getString PAUSE_AUTOPAY_STR
                  Mb.Nothing -> ""
      , color = Color.yellow900
      , background = Color.black
      , visibility =true
      },
      coverImageConfig {
        imageUrl =  case state.props.popUpState of
          Mb.Just SuccessPopup -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just FailedPopup -> "ny_failed,"
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
      option2 { visibility = false }
    }
  in popUpConf'


confirmCancelPopupConfig :: ST.SubscriptionScreenState -> PopUpModalConfig.Config
confirmCancelPopupConfig state = let
    config = PopUpModalConfig.config
    popUpConfig' = config {
      gravity = CENTER
    , cornerRadius = Corners 15.0 true true true true
    , margin = MarginHorizontal 16 16
    , backgroundColor =  Color.black9000
    , backgroundClickable = false
    , primaryText {
        text = "Do you want to cancel?"
      , margin = (Margin 16 24 16 0)
      },
      secondaryText {
        text = "You are about to cancel Autopay.\nYou will have to set up autopay again to resume automatic payments."
      , color = Color.black700
      , margin = (Margin 16 12 16 40)
        },
      option1 {
        text = "No"
      , color = Color.black900
      , strokeColor = Color.black700
      },
      option2 {text = "Yes, Cancel"
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , margin = (MarginLeft 12) 
      },
      coverImageConfig {
        imageUrl = "ny_ic_pause_autopay,"
      , visibility = VISIBLE
      , width = V 265
      , height = V 265
      }
    }
  in popUpConfig'

getSelectedAlternatePlan :: ST.SubscriptionScreenState -> String
getSelectedAlternatePlan state = do
  let plan = (DA.filter(\item -> item.id == state.props.managePlanProps.selectedPlan) state.data.managePlanData.alternatePlans)
  case plan DA.!! 0 of 
    Mb.Just value -> value.title
    Mb.Nothing -> state.data.myPlanData.planEntity.title

getSelectedJoiningPlan :: ST.SubscriptionScreenState -> Mb.Maybe String
getSelectedJoiningPlan state = do
  case state.props.joinPlanProps.selectedPlan of
    Mb.Just id -> do 
                    let plan = (DA.filter(\item -> item.id == id) state.data.joinPlanData.allPlans)
                    case plan DA.!! 0 of 
                      Mb.Just value -> Mb.Just value.title
                      Mb.Nothing -> Mb.Nothing
    Mb.Nothing -> Mb.Nothing
  
tryAgainButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
tryAgainButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = getString TRY_AGAIN }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = V 48
      , cornerRadius = 8.0
      , margin = Margin 16 16 16 16
      }
  in primaryButtonConfig'