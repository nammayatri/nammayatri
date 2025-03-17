module Screens.RideBookingFlow.MeterRideScreen.Controller where

import Prelude
import Screens.Types as ST
import PrestoDOM (Eval, FontWeight(..), update, continue, continueWithCmd, exit, updateAndExit)
import Data.Maybe (Maybe(..))
import JBridge as JB
import PrestoDOM.Types.Core (class Loggable)
import JBridge (requestKeyboardShow, hideKeyboardOnNavigation)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Debug
import Effect.Uncurried (runEffectFn1)

data Action = NoAction
            | BackPressed
            | ShowInfoCard
            | OTPFocussed1 Boolean
            | OTPTextChanged1 String
            | EnterOTP
            | ClearFocus 
            | ShowRateCard
            | CloseRateCard
            | EnterDestination
            | RefreshTime
            | OnNavigate
            | ChangeSlider Boolean
            | DebounceCallBack String Boolean
            | SliderCallback Int
            | StopRotation String

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = pure unit

data ScreenOutput = GoBack ST.MeterRideScreenState 
                    | CustomerOTP String

eval :: Action -> ST.MeterRideScreenState -> Eval Action ScreenOutput ST.MeterRideScreenState

eval ShowInfoCard state = do
    let _ = runEffectFn1 JB.clearFocus (EHC.getNewIDWithTag "OTP1")
    _ <- pure $ hideKeyboardOnNavigation true
    continue state {props{showInfoCard = true, isFocussed = false}}

eval BackPressed state = do
    if state.data.isMeterRideSynced then do 
        _ <- pure $ JB.minimizeApp ""
        continue state
    else do
        _ <- pure $ hideKeyboardOnNavigation true
        if state.props.showInfoCard then do
            continue state {props{showInfoCard = false}}
        else do
            exit $ GoBack state

eval (OTPFocussed1 focussed) state = do
    continue state {props {isFocussed = EHC.isTrue focussed}}

eval (OTPTextChanged1 val) state = do
    if (DS.length val == 4) then do
        void $ pure $ hideKeyboardOnNavigation true
        let _ = runEffectFn1 JB.clearFocus (EHC.getNewIDWithTag "OTP1")
        continue state{props{otp = val, invalidOTP = false, isFocussed = true}}
    else 
        continue state{props{otp = val, invalidOTP = false, isFocussed = true}}

eval EnterOTP state = do
    updateAndExit state{props{isOTPLoading = true}} $ CustomerOTP state.props.otp

eval ClearFocus state = do
    continueWithCmd state {props{isFocussed = false}}[do
        void $ pure $ hideKeyboardOnNavigation true
        void $ runEffectFn1 JB.clearFocus (EHC.getNewIDWithTag "OTP1")
        pure NoAction
    ]

eval _ state = do
    continue state