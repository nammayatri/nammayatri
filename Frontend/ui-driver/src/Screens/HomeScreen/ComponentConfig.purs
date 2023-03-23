{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ComponentConfig where

import Components.CancelRide as CancelRide
import Components.PopUpModal as PopUpModal
import Components.RideActionModal as RideActionModal
import Components.StatsModel as StatsModel
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Helpers.Utils as HU
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import PrestoDOM.Types.DomAttributes as PTD
import Screens.Types as ST
import Styles.Colors as Color
import EN

--------------------------------- rideActionModalConfig -------------------------------------
rideActionModalConfig :: ST.HomeScreenState -> RideActionModal.Config
rideActionModalConfig state = let 
  config = RideActionModal.config
  rideActionModalConfig' = config {
    startRideActive = if state.props.currentStage == ST.RideAccepted then true else false,
    totalDistance = if state.data.activeRide.distance <= 0.0 then "0.0" else if(state.data.activeRide.distance < 1000.0) then HU.parseFloat (state.data.activeRide.distance) 2 <> " m" else HU.parseFloat((state.data.activeRide.distance / 1000.0)) 2 <> " km",
    customerName = if DS.length (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)) < 4 
                      then (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)) <> " " <> (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 1)) 
                      else 
                        (fromMaybe "" ((DS.split (DS.Pattern " ") (state.data.activeRide.riderName)) DA.!! 0)),
    sourceAddress  {
      titleText = fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.activeRide.source)) DA.!! 0),
      detailText = state.data.activeRide.source
    },
    destinationAddress {
      titleText = fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.activeRide.destination)) DA.!! 0),
      detailText = state.data.activeRide.destination
    },
    estimatedRideFare = state.data.activeRide.estimatedFare,
    isDriverArrived = state.data.activeRide.isDriverArrived,
    notifiedCustomer = state.data.activeRide.notifiedCustomer
  }
  in rideActionModalConfig'

---------------------------------------- endRidePopUp -----------------------------------------
endRidePopUp :: ST.HomeScreenState -> PopUpModal.Config
endRidePopUp state = let 
  config' = PopUpModal.config
  popUpConfig' = config'{
    primaryText {text = (getString END_RIDE)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE)},
    option1 {text =(getString GO_BACK), testIdText = (getEN GO_BACK)},
    option2 {text = (getString END_RIDE), testIdText = (getEN END_RIDE)}
  }
  in popUpConfig'

------------------------------------------ cancelRideModalConfig ---------------------------------
cancelRideModalConfig :: ST.HomeScreenState -> CancelRide.Config
cancelRideModalConfig state = let
  config = CancelRide.config
  lastIndex = ((DA.length state.data.cancelRideModal.cancelRideReasons) -1)
  cancelRideModalConfig' = config {
    activeIndex = state.data.cancelRideModal.activeIndex,
    headingText = ((getString CANCEL_RIDE) <> "?"),
    subHeadingText = (getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL),
    hint = (getString HELP_US_WITH_YOUR_REASON),
    strings {
      mandatory = (getString MANDATORY),
      limitReached = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
    },
    showAllOptionsText = (getString SHOW_ALL_OPTIONS),
    cancelRideReasons = state.data.cancelRideModal.cancelRideReasons,
    isLimitExceeded = ((DS.length (state.data.cancelRideModal.selectedReasonDescription)) >= 100),
    activeReasonCode = state.data.cancelRideModal.selectedReasonCode,
    primaryButtonTextConfig {
      firstText = (getString GO_BACK)
    , secondText = (getString CANCEL_RIDE)
    },
    isCancelButtonActive = case state.data.cancelRideModal.activeIndex of 
                              Just index -> true
                              Nothing    -> false
  }
  in cancelRideModalConfig' 

---------------------------------- statsModelConfig --------------------------------
statsModelConfig :: ST.HomeScreenState -> StatsModel.Config
statsModelConfig state = 
  let 
    config = StatsModel.config
    config' = config
      { countTextConfig { text = getString TRIP_COUNT }
      , earningsTextConfig { text = getString TODAYS_EARNINGS }
      , textConfig {  text = "" }
      , totalRidesOfDay = state.data.totalRidesOfDay
      , totalEarningsOfDay = state.data.totalEarningsOfDay
      }
  in config'

------------------------------------ cancelConfirmationConfig -----------------------------
cancelConfirmationConfig :: ST.HomeScreenState -> PopUpModal.Config
cancelConfirmationConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 0 16 20 ,
    primaryText {
      text = (getString FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES)
    , margin = Margin 16 24 16 24 },
    secondaryText {visibility = GONE},
    option1 {
      text = (getString CONTINUE)
    , fontSize = FontSize.a_16
    , width = V $ (((EHC.screenWidth unit)-92)/2) 
    , isClickable = state.data.cancelRideConfirmationPopUp.continueEnabled
    , timerValue = state.data.cancelRideConfirmationPopUp.delayInSeconds
    , enableTimer = true
    , background = Color.white900
    , strokeColor = Color.black500
    , color = Color.black700
    , testIdText = (getEN CONTINUE)
    },
    option2 {
      text = (getString GO_BACK)
    , margin = Margin 12 0 0 0
    , fontSize = FontSize.a_16
    , width = V $ (((EHC.screenWidth unit)-92)/2)
    , color = Color.yellow900
    , strokeColor = Color.black900
    , background = Color.black900
    , testIdText = (getEN GO_BACK)
    },
    backgroundClickable = false,
    cornerRadius = (PTD.Corners 15.0 true true true true),
    coverImageConfig {
      imageUrl = "ny_ic_cancel_prevention,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cancel_prevention.png"
    , visibility = VISIBLE
    , margin = Margin 16 20 16 0
    , height = V 178
    }
  }
  in popUpConfig'