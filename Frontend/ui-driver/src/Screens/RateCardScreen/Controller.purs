{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RateCardScreen.Controller where

import Prelude
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Log (printLog, trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import Screens (ScreenName(..), getScreen)
import Components.PrimaryButton as PrimaryButton
import Components.RateCard as RateCard
import Common.Types.App as CTA
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import JBridge as JB
import Engineering.Helpers.Commons as EHC
import Screens.Types as ST

data ScreenOutput = Back ST.RateCardScreenState 
  | UpdatePrice ST.RateCardScreenState Int
  | OpenRateCard ST.RateCardScreenState ST.RidePreference

data Action = BackClick
    | ShowRateCard ST.RidePreference
    | PrimaryButtonAC PrimaryButton.Action
    | SliderCallback Int
    | SliderCallbackManual Int
    | AfterRender
    | RateCardAction RateCard.Action
    | ChangeSlider Boolean
    | OpenLink String
    | DebounceCallBack String Boolean

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen RATE_CARD_SCREEN)
      BackClick -> do
        trackAppBackPress appId (getScreen RATE_CARD_SCREEN)
        trackAppEndScreen appId (getScreen RATE_CARD_SCREEN)
      ShowRateCard pref -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "rate_card"
      OpenLink link -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "open_link"
      SliderCallback val -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "slider"
      RateCardAction action ->
        case action of
          RateCard.Close -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "close"
          RateCard.BackPressed -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "back"
          RateCard.GoToDefaultStart -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "default_start"
          RateCard.GoToDriverAddition -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "driver_addition"
          RateCard.GoToTollOrParkingCharges -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "toll_parking"
          _ -> pure unit
      PrimaryButtonAC action -> 
        case action of
          PrimaryButton.OnClick -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "primary_button_action" "open_google_maps"
          _ -> pure unit
      ChangeSlider increment -> trackAppActionClick appId (getScreen RATE_CARD_SCREEN) "in_screen" "slider"
      _ -> pure unit
      


eval :: Action -> ST.RateCardScreenState -> Eval Action ScreenOutput ST.RateCardScreenState

eval BackClick state = 
  if state.props.showRateCard then continue state { props { showRateCard = false}}
  else exit $ Back state

eval (PrimaryButtonAC PrimaryButton.OnClick) state = continueWithCmd state [ pure $ BackClick ]

eval (SliderCallback val) state = continue state { props { sliderVal = val, sliderLoading = true}}

eval (SliderCallbackManual val) state = continue state { props { sliderVal = val}}

eval (DebounceCallBack _ _) state = updateAndExit state $ UpdatePrice state state.props.sliderVal

eval (ChangeSlider increment) state = continueWithCmd state [do 
    let val = if increment then state.props.sliderVal + state.props.incrementUnit else state.props.sliderVal - state.props.incrementUnit
        isSliderValueValid = val <= state.props.sliderMaxValue && val >= state.props.sliderMinValue
    if isSliderValueValid then do 
      void $ JB.updateSliderValue {sliderValue : val, id : EHC.getNewIDWithTag "RateSlider"}
      pure $ SliderCallbackManual val
      else pure $ AfterRender
    ]

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = CTA.DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = CTA.TollOrParkingCharges,onFirstPage = true}}}

eval (ShowRateCard pref) state = do
  let mbPref = DA.find (\item -> item.serviceTierType == pref.serviceTierType) state.data.ridePreferences
  case mbPref of
    Just ridePreference -> do
      case ridePreference.rateCardData of
        Just rateCardData -> do
          let  newState = state { data { rateCard  {
              onFirstPage = false,
              serviceTierName = Just ridePreference.name,
              currentRateCardType = CTA.DefaultRateCard,
              driverAdditions = rateCardData.driverAdditions,
              fareInfoDescription = rateCardData.fareInfo,
              isNightShift = rateCardData.isNightShift,
              nightChargeTill = rateCardData.nightChargeEnd,
              nightChargeFrom = rateCardData.nightChargeStart,
              extraFare = rateCardData.fareList
            }}}
          if getMerchant CTA.FunctionCall /= BRIDGE then continue newState {props {showRateCard = true }}
            else exit $ OpenRateCard newState ridePreference
        Nothing -> continue state
    Nothing -> continue state


eval (OpenLink link) state = continueWithCmd state [ do 
  void $ JB.openUrlInApp link
  pure AfterRender
  ]

eval _ state = update state