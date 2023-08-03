{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MultipleRCUploadScreen.Controller where

import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.ReferralMobileNumber.Controller as ReferralMobileNumberController
import Engineering.Helpers.Commons (getNewIDWithTag, getExpiryTime, setText)
import JBridge (hideKeyboardOnNavigation, minimizeApp, openWhatsAppSupport, showDialer, toast)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent, trackAppTextInput)
import Prelude (class Show, pure, unit, bind, not, ($), discard, (==), (&&), (||), (<=), (>=), (/), (>))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Data.String as DS
import Screens.Types (MultipleRCUploadScreenState)
import Services.API (DriverRegistrationStatusResp(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    BackPressed -> trackAppBackPress appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    GoToProfile a -> trackAppBackPress appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    OnRCNumberChange a -> trackAppBackPress appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    OnReEnterRCNumberChange a -> trackAppBackPress appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    GoToImageUpload -> trackAppBackPress appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    NoAction -> trackAppBackPress appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN)
    DriverRegistrationStatusAction resp -> trackAppScreenEvent appId (getScreen MULTIPLE_RC_UPLOAD_SCREEN) "in_screen" "driver_registration_status"

data ScreenOutput = GoToDriverDetailsScreen MultipleRCUploadScreenState 
                  | GoToSuccessScreen MultipleRCUploadScreenState
data Action = BackPressed
              | AfterRender
              | DriverRegistrationStatusAction DriverRegistrationStatusResp
              | GoToProfile PrimaryButton.Action
              | OnRCNumberChange String
              | OnReEnterRCNumberChange String
              | GoToImageUpload
              | NoAction

eval :: Action -> MultipleRCUploadScreenState -> Eval Action ScreenOutput MultipleRCUploadScreenState
eval AfterRender state =  continue state
eval NoAction state =  continue state
eval BackPressed state = do
  _ <- pure $ minimizeApp ""
  continue state
eval (DriverRegistrationStatusAction (DriverRegistrationStatusResp resp)) state = do
  if (resp.dlVerificationStatus == "VALID" && resp.rcVerificationStatus == "VALID") then
    exit $ GoToDriverDetailsScreen state
    else do
      continue state
eval (GoToProfile (PrimaryButton.OnClick)) state = exit $ GoToDriverDetailsScreen state
eval (OnRCNumberChange val) state = do
  let newState = state {data { vehicleRegistrationNumber = val }, props { isButtonEnabled = (checkRegNum val) && val == state.data.reEnterVehicleRegistrationNumber }}
  continue newState
eval (OnReEnterRCNumberChange val) state = do
  let newState = state {data { reEnterVehicleRegistrationNumber = val }, props { isButtonEnabled = (checkRegNum val) && val == state.data.vehicleRegistrationNumber }}
  continue newState

eval _ state = continue state

checkRegNum :: String -> Boolean
checkRegNum temp = if (DS.length temp > 1) then true else false
    --   let  popup_visibility = any (_ == resp.dlVerificationStatus) ["FAILED","INVALID"]  || any (_ == resp.rcVerificationStatus) ["FAILED","INVALID"]
    --   let  onBoardingStatus =  any (_ == resp.dlVerificationStatus) ["LIMIT_EXCEED","NO_DOC_AVAILABLE"] || any (_ == resp.rcVerificationStatus) ["LIMIT_EXCEED","NO_DOC_AVAILABLE"]
    --   let timeDifference = (getExpiryTime (getValueToLocalStore DOCUMENT_UPLOAD_TIME) true)/3600
    --   if (timeDifference>=48 && (resp.dlVerificationStatus == "PENDING" || resp.rcVerificationStatus == "PENDING")) then continue state{data { dlVerificationStatus = resp.dlVerificationStatus, rcVerificationStatus = resp.rcVerificationStatus},props{onBoardingFailure = true,isVerificationFailed = false}}
    --   else continue state { data { dlVerificationStatus = resp.dlVerificationStatus, rcVerificationStatus = resp.rcVerificationStatus}, props{onBoardingFailure = onBoardingStatus, isVerificationFailed = popup_visibility}}