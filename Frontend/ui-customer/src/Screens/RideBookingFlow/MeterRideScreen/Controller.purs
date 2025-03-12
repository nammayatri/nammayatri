module Screens.RideBookingFlow.MeterRideScreen.Controller where

import Prelude
import Screens.Types as ST
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
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
            | ShowRateCard
            | OTPFocussed1 Boolean
            | OTPTextChanged1 String
            | EnterOTP
            | ClearFocus 

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = pure unit

data ScreenOutput = GoBack ST.MeterRideScreenState | CustomerOTP String

eval :: Action -> ST.MeterRideScreenState -> Eval Action ScreenOutput ST.MeterRideScreenState

eval ShowRateCard state = do
    let _ = runEffectFn1 JB.clearFocus (EHC.getNewIDWithTag "OTP1")
    _ <- pure $ hideKeyboardOnNavigation true
    continue state {props{showInfoCard = true, isFocussed = false}}

eval BackPressed state = do
    _ <- pure $ hideKeyboardOnNavigation true
    if state.props.showInfoCard then do
        continue state {props{showInfoCard = false}}
    else do
        exit $ GoBack state

eval (OTPFocussed1 focussed) state = do
    continue state {props {isFocussed = EHC.isTrue focussed}}
        

-- eval (OTPFocussed2 focussed) state = do
--     if EHC.isTrue focussed then do
--         if DS.length state.props.otp.four == 1 then do 
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP4")
--             continue state
--         else if DS.length state.props.otp.three == 1 then do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP3")
--             continue state
--         else if DS.length state.props.otp.two == 1 then do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP2")
--             continue state
--         else do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP1")
--             continue state
--     else do
--         continue state

-- eval (OTPFocussed3 focussed) state = do
--     if EHC.isTrue focussed then do
--         if DS.length state.props.otp.four == 1 then do 
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP4")
--             continue state
--         else if DS.length state.props.otp.three == 1 then do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP3")
--             continue state
--         else if DS.length state.props.otp.two == 1 then do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP2")
--             continue state
--         else do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP1")
--             continue state
--     else do
--         continue state

-- eval (OTPFocussed4 focussed) state = do
--     if EHC.isTrue focussed then do
--         if DS.length state.props.otp.four == 1 then do 
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP4")
--             continue state
--         else if DS.length state.props.otp.three == 1 then do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP3")
--             continue state
--         else if DS.length state.props.otp.two == 1 then do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP2")
--             continue state
--         else do
--             _ <- pure $ JB.requestKeyboardShow (EHC.getNewIDWithTag "OTP1")
--             continue state
--     else do
--         continue state

eval (OTPTextChanged1 val) state = do
    if (DS.length val == 4) then do
        void $ pure $ hideKeyboardOnNavigation true
        let _ = runEffectFn1 JB.clearFocus (EHC.getNewIDWithTag "OTP1")
        continue state{props{otp{one = val}, invalidOTP = false, isFocussed = true}}
    else 
        continue state{props{otp{one = val}, invalidOTP = false, isFocussed = true}}
   

-- eval (OTPTextChanged2 val) state = do

-- eval (OTPTextChanged3 val) state = do
    

-- eval (OTPTextChanged4 val) state = do

eval EnterOTP state = do
    updateAndExit state{props{isOTPLoading = true}} $ CustomerOTP state.props.otp.one

eval ClearFocus state = do
    continueWithCmd state {props{isFocussed = false}}[do
        void $ pure $ hideKeyboardOnNavigation true
        void $ runEffectFn1 JB.clearFocus (EHC.getNewIDWithTag "OTP1")
        pure NoAction
    ]

eval _ state = do
    continue state