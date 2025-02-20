module Screens.MeterMapScreen.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge as JB
import Prelude 
import PrestoDOM 
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types as ST
import Common.Types.App as CTA
import Data.String as DS
import Data.Maybe (Maybe(..))
import Components.RateCard as RateCard
import Components.InAppKeyboardModal as InAppKeyboardModal
import JBridge(hideKeyboardOnNavigation)
import Engineering.Helpers.Utils (mobileNumberValidator)
import Common.Types.App (MobileNumberValidatorResp(..)) as MVR
import Debug (spy)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data Action
  = BackPressed
  | PrimaryButtonAC PrimaryButtonController.Action
  | NoAction
  | ShowMap String String String
  | PhoneNoChanged String
  | RateCardAction RateCard.Action
  | ShowRateCard
  | InAppKeyboardModalOtp InAppKeyboardModal.Action
  | ConfirmRidePressed

data ScreenOutput
  = GoToMeterScreen ST.MeterMapScreenState
  | SearchPlace String ST.MeterMapScreenState 
  | OTPEntered ST.MeterMapScreenState

eval :: Action -> ST.MeterMapScreenState -> Eval Action ScreenOutput ST.MeterMapScreenState

eval BackPressed state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  exit $ GoToMeterScreen state

eval (ShowMap _ _ _) state = continueWithCmd state [ do
  -- id <- checkPermissionAndUpdateDriverMarker true
  pure NoAction
  ]

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = CTA.DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = CTA.DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = CTA.TollOrParkingCharges,onFirstPage = true}}}

eval ShowRateCard state = continue state {data { rateCard  {
              onFirstPage = false,
              serviceTierName = Just "Auto",
              currentRateCardType = CTA.DefaultRateCard,
              driverAdditions = [{key : "0km-2km", val : "Upto ₹10"}, {key : "2km-10km", val : "Upto ₹20"}, {key: "10km-17km", val : "Upto ₹30"}, {key: "17km+", val : "Upto ₹40"}],
              fareInfoDescription = [],
              isNightShift = false,
              nightChargeTill = "5 am",
              nightChargeFrom = "10 am",
              extraFare = [{key : "Min. Fare upto 2km", val : "₹30"}, {key: "Waiting Charges", val : "₹1.50 per minute"}]
            }}, props {showRateCard = true}}

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  continue state{props{enableOtpModal=true}}
  
eval (InAppKeyboardModalOtp InAppKeyboardModal.BackPressed) state = 
  continue state{props{alternateMobileOtp = "", enableOtpModal=false, enterOtpFocusIndex = 0}}

-- eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickResendOtp)) state = do
--   exit (ResendAlternateNumberOTP state)

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickDone _)) state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  exit $ OTPEntered state

-- eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickBack text)) state = do
--   let newVal = (if DS.length(text) > 0 then (DS.take (DS.length(text) - 1 ) text) else "" )
--       focusIndex = DS.length newVal
--   continue state {props { alternateMobileOtp = newVal, enterOtpFocusIndex = focusIndex,otpIncorrect = false, otpAttemptsExceeded = false}}

-- eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnSelection key index)) state = do
--   let
--     alternateMobileOtp' = if (index + 1) > (DS.length state.props.alternateMobileOtp) then ( DS.take 4 (state.props.alternateMobileOtp <> key)) else (DS.take index (state.props.alternateMobileOtp)) <> key <> (DS.take 4 (DS.drop (index+1) state.props.alternateMobileOtp))
--     focusIndex = DS.length alternateMobileOtp'
--     _ = spy "STATE CHANGED " state
--   continue state { props { alternateMobileOtp = alternateMobileOtp', enterOtpFocusIndex = focusIndex, otpIncorrect = false } }

eval (PhoneNoChanged phoneNumber) state = do
  let numberValidator = mobileNumberValidator "" "IN" phoneNumber
  if DS.length phoneNumber == 10 then do
    _ <- pure $ hideKeyboardOnNavigation true
    if numberValidator == MVR.Valid then continue state {props {customerMobileNumber = phoneNumber, isCustomerNumberValid = true}}
    else continue state {props {customerMobileNumber = phoneNumber, isCustomerNumberValid = false}}
  else continue state {props {customerMobileNumber = phoneNumber, isCustomerNumberValid = false}}

eval _ state = update state
