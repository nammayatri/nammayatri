{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Data.Maybe (Maybe(..), fromMaybe)
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard, (==), Unit)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.DriverProfileScreen.Controller (ScreenOutput(..))
import Screens.DriverProfileScreen.ScreenData as DriverProfileScreenData
import Screens.DriverProfileScreen.View as DriverProfileScreen
import Screens.CancellationRateScreen.ScreenData as CancellationRateScreenData
import Types.App (FlowBT, GlobalState(..), DRIVER_PROFILE_SCREEN_OUTPUT(..), ScreenType(..))
import Screens.DriverProfileScreen.View (getVehicleCategory)
import Types.ModifyScreenState (modifyScreenState)
import Data.Maybe (isJust)
import Debug (spy)

driverProfileScreen :: FlowBT String DRIVER_PROFILE_SCREEN_OUTPUT
driverProfileScreen = do
  (GlobalState gbstate) <- getState
  logField_ <- lift $ lift $ getLogFields
  action <- lift $ lift $ runLoggableScreen $ DriverProfileScreen.screen gbstate.driverProfileScreen{data{logField = logField_}}
  case action of
    GoToDriverDetailsScreen updatedState -> do
      modifyScreenState $ DriverDetailsScreenStateType (\driverDetails ->
        driverDetails { data { driverName = updatedState.data.driverName,
        driverVehicleType = updatedState.data.driverVehicleType,
        driverRating = updatedState.data.driverRating,
        base64Image = updatedState.data.base64Image,
        driverMobile = updatedState.data.driverMobile,
        driverAlternateMobile = updatedState.data.driverAlternateNumber
        },
        props {
          checkAlternateNumber = (if (updatedState.data.driverAlternateNumber == Nothing) then true else false)
        }})
      App.BackT $ App.BackPoint <$> pure DRIVER_DETAILS_SCREEN

    GoToVehicleDetailsScreen updatedState -> do
      modifyScreenState $ VehicleDetailsScreenStateType (\vehicleDetails ->
        vehicleDetails { data { vehicleRegNumber = updatedState.data.vehicleRegNumber,
        vehicleType = updatedState.data.driverVehicleType,
        vehicleModel = updatedState.data.vehicleModelName,
        vehicleColor = updatedState.data.vehicleColor
        }})
      App.BackT $ App.BackPoint <$> pure VEHICLE_DETAILS_SCREEN

    GoToAboutUsScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure ABOUT_US_SCREEN
    GoToLogout state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure GO_TO_LOGOUT
    GoToHelpAndSupportScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure HELP_AND_SUPPORT_SCREEN
    GoToHomeScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure (GO_TO_HOME_FROM_PROFILE state)
    GoToReferralScreen state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN
    GoToDriverHistoryScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure GO_TO_DRIVER_HISTORY_SCREEN
    GoToSelectLanguageScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure SELECT_LANGUAGE_SCREEN
    OnBoardingFlow state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure ON_BOARDING_FLOW
    DocumentsFlow state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure DOCUMENTS_FLOW
    GoToNotifications state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure NOTIFICATIONS_SCREEN
    GoToBookingOptions state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure (GO_TO_BOOKING_OPTIONS_SCREEN state)
    VerifyAlternateNumberOTP state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure (VERIFY_OTP1 state)
    ResendAlternateNumberOTP state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.BackPoint <$> pure (RESEND_ALTERNATE_OTP1 state)
    ValidateAlternateNumber  updatedState ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> updatedState)
      App.BackT $ App.NoBack <$> pure (DRIVER_ALTERNATE_CALL_API1 updatedState)
    RemoveAlternateNumber state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure (ALTERNATE_NUMBER_REMOVE1 state)
    UpdateGender state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure (DRIVER_GENDER1 state)
    ActivatingOrDeactivatingRC state -> App.BackT $ App.NoBack <$> pure (GO_TO_ACTIVATE_OR_DEACTIVATE_RC state)
    DeletingRc state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure (GO_TO_DELETE_RC state)
    CallingDriver state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure (GO_TO_CALL_DRIVER state)
    AddingRC state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure (ADD_RC state)
    SubscriptionScreen state ->  do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> state)
      App.BackT $ App.NoBack <$> pure (SUBCRIPTION )
    UpdateLanguages updatedState language -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> updatedState)
      App.BackT $ App.NoBack  <$> (pure $ UPDATE_LANGUAGES language)
    GoToDriverSavedLocationScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> pure SAVED_LOCATIONS_SCREEN
    GoToPendingVehicle updatedState rcNumber vehicleCategory -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> updatedState)
      App.BackT $ App.BackPoint <$> pure (VIEW_PENDING_VEHICLE rcNumber vehicleCategory)
    GoBack updatedState -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverDetailsScreen ->
        DriverProfileScreenData.initData { data { driverVehicleType = driverDetailsScreen.data.driverVehicleType
                                                , capacity = driverDetailsScreen.data.capacity
                                                , downgradeOptions = driverDetailsScreen.data.downgradeOptions
                                                , vehicleSelected = driverDetailsScreen.data.vehicleSelected
                                                , profileImg = driverDetailsScreen.data.profileImg}})
      App.BackT $ App.NoBack <$> pure (GO_HOME updatedState)
    GoToCompletingProfile updatedState -> do
      modifyScreenState $ DriverProfileScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> pure (DRIVER_COMPLETING_PROFILE_SCREEN (getVehicleCategory updatedState))
    GoToCancellationRateScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\_ -> state)
      let cancellationScreenState = CancellationRateScreenData.initData { data { cancellationRate = if isJust state.data.cancellationWindow then state.data.cancellationRate else state.data.analyticsData.cancellationRate
                                                   , assignedRides = state.data.analyticsData.totalRidesAssigned
                                                   , cancelledRides = state.data.analyticsData.ridesCancelled
                                                   , cancellationWindow = state.data.cancellationWindow
                                                   , missedEarnings = state.data.analyticsData.missedEarnings}}
      App.BackT $ App.BackPoint <$> pure (CANCELLATION_RATE_SCREEN cancellationScreenState)
    GotoMeterRideScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> pure GO_TO_METER_RIDE_SCREEN_FROM_PROFILE

    GoToExtraChargeInfoScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\_ -> state)
      modifyScreenState $ ExtraChargeInfoScreenStateType (\exState -> exState {driverInfoResp = state.data.driverInfoResponse})
      App.BackT $ App.BackPoint <$> pure GO_TO_EXTRA_CHARGE_INFO_SCREEN
    GoToClubDetailsScreen state -> do
      modifyScreenState $ DriverProfileScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> pure GO_TO_CLUB_DETAILS_SCREEN