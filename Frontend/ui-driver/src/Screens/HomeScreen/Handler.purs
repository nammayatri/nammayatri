{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Handler where

import Prelude

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Error, makeAff, nonCanceler)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (markPerformance)
import Helpers.Utils (getCurrentLocation, LatLon(..))
import Helpers.Utils (getDistanceBwCordinates, LatLon(..), getCurrentLocation, getRideInfoEntityBasedOnBookingType)
import JBridge (getCurrentPosition, getCurrentPositionWithTimeout)
import Log (printLog)
import Presto.Core.Types.Language.Flow (doAff)
import Presto.Core.Types.Language.Flow (getLogFields)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Engineering.Helpers.Events as Events
import Screens.HomeScreen.Controller (ScreenOutput(..))
import Screens.HomeScreen.View as HomeScreen
import Screens.Types (KeyboardModalType(..),TripType(..))
import Types.App (FlowBT, GlobalState(..), HOME_SCREENOUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)
import Debug

homeScreen :: FlowBT String HOME_SCREENOUTPUT
homeScreen = do
  liftFlowBT $ Events.endMeasuringDuration "Flow.initialFlow"
  logField_ <- lift $ lift $ getLogFields
  (GlobalState state) <- getState
  liftFlowBT $ markPerformance "HOME_SCREEN"
  action <- lift $ lift $ runScreen $ HomeScreen.screen state.homeScreen{data{logField = logField_}} (GlobalState state)
  (GlobalState updatedGlobalState) <- getState
  modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps { bgLocPopupShown = updatedGlobalState.globalProps.bgLocPopupShown || updatedGlobalState.homeScreen.props.bgLocationPopup }
  case action of
    GoToCompleteProfile updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_COMPLETE_PROFILE_SCREEN
    GoToVehicleDetailScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_VEHICLE_DETAILS_SCREEN
    GoToProfileScreen updatedState-> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_PROFILE_SCREEN updatedState)
    CustomerReferralTrackerScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_CUSTOMER_REFERRAL_TRACKER updatedState)
    GoToHelpAndSupportScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_HELP_AND_SUPPORT_SCREEN
    GotoEditGenderScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_EDIT_GENDER_SCREEN
    GoToRidesScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_RIDES_SCREEN
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
    DriverAvailabilityStatus updatedState status -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (DRIVER_AVAILABILITY_STATUS updatedState status)
    GoToNewStop updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_NEW_STOP updatedState)
    StartRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      LatLon lat lon ts <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  updatedState.data.activeRide.src_lat updatedState.data.activeRide.src_lon 700 false false
      let odometerValue = if updatedState.props.isOdometerReadingsRequired then Just updatedState.props.odometerValue else Nothing 
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_RIDE {id: updatedState.data.activeRide.id, otp : updatedState.props.rideOtp , startOdometerReading : odometerValue, startOdometerImage : updatedState.props.startRideOdometerImage, lat : lat , lon : lon , ts :ts} updatedState) 
    StartZoneRide  updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      LatLon lat lon ts <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  updatedState.data.activeRide.src_lat updatedState.data.activeRide.src_lon 1000 true false
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_ZONE_RIDE {otp : updatedState.props.rideOtp , lat : lat , lon : lon ,ts :ts}) 
    EndRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      let destLat = if updatedState.data.activeRide.tripType == Rental then fromMaybe updatedState.data.activeRide.src_lat updatedState.data.activeRide.lastStopLat else updatedState.data.activeRide.dest_lat
          destLon = if updatedState.data.activeRide.tripType == Rental then fromMaybe updatedState.data.activeRide.src_lon updatedState.data.activeRide.lastStopLon else updatedState.data.activeRide.dest_lon
          odometerValue = if updatedState.props.isOdometerReadingsRequired then Just updatedState.props.odometerValue else Nothing 
      LatLon lat lon ts <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon destLat destLon 700 false false
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_END_RIDE {id : updatedState.data.activeRide.id, endOtp : updatedState.props.rideOtp, endOdometerReading : odometerValue, endOdometerImage: updatedState.props.endRideOdometerImage , lat : lat, lon : lon, ts :ts} updatedState)
    ArrivedAtStop updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      let destLat = fromMaybe 0.0 updatedState.data.activeRide.nextStopLat
          destLon = fromMaybe 0.0 updatedState.data.activeRide.nextStopLon
      LatLon lat lon ts <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  destLat destLon 700 false false
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_ARRIVED_AT_STOP {id : updatedState.data.activeRide.id, lat : lat, lon : lon, ts :ts} updatedState)
    SelectListModal updatedState -> do
      let rideData = getRideInfoEntityBasedOnBookingType updatedState
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_CANCEL_RIDE {id : rideData.id , info : updatedState.data.cancelRideModal.selectedReasonDescription, reason : updatedState.data.cancelRideModal.selectedReasonCode} updatedState)
    Refresh updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure REFRESH_HOME_SCREEN_FLOW
    UpdatedState screenState -> do
      modifyScreenState $ HomeScreenStateType (\_ → screenState)
      App.BackT $ App.NoBack <$> (pure $ RELOAD screenState)
    UpdateRoute updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE updatedState)
    FcmNotification notificationType notificationBody screenState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → screenState)
      App.BackT $ App.BackPoint <$> (pure $ FCM_NOTIFICATION notificationType screenState notificationBody)
    NotifyDriverArrived updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NOTIFY_CUSTOMER updatedState)
    UpdateStage stage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_STAGE stage)
    GoToNotifications updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_NOTIFICATIONS
    AddAlternateNumber updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ADD_ALTERNATE_HOME)
    CallCustomer updatedState exophoneNumber -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ON_CALL updatedState exophoneNumber)
    OpenPaymentPage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ OPEN_PAYMENT_PAGE updatedState)
    AadhaarVerificationFlow updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_AADHAAR_VERIFICATION)
    SubscriptionScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ HOMESCREEN_NAV GoToSubscription)
    GoToRideDetailsScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDE_DETAILS_SCREEN)
    PostRideFeedback updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ POST_RIDE_FEEDBACK updatedState)
    ClearPendingDues updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CLEAR_PENDING_DUES)
    EnableGoto updatedState locationId -> do
      let destLat = updatedState.data.activeRide.dest_lat
          destLon = updatedState.data.activeRide.dest_lon
      LatLon lat lon _ <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon destLat destLon 700 false true
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ ENABLE_GOTO_API updatedState locationId (lat <> "," <> lon))
    LoadGotoLocations updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ LOAD_GOTO_LOCATIONS updatedState)
    DisableGoto updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ DISABLE_GOTO updatedState)
    ExitGotoLocation updatedState addLocation -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GOTO_LOCATION_FLOW updatedState addLocation)
    RefreshGoTo updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_GOTO updatedState)
    EarningsScreen updatedState showCoinsView -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ HOMESCREEN_NAV $ GoToEarningsScreen showCoinsView)
    DriverStatsUpdate driverStats updatedState -> do       
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GOT_DRIVER_STATS driverStats)
    UpdateSpecialLocationList updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_SPECIAL_LOCATION_LIST)
    UpdateRouteOnStageSwitch updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_ROUTE_ON_STAGE_SWITCH updatedState)
    FetchOdometerReading updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CLEAR_PENDING_DUES)
    UpdateAirConditioned updatedState isAcWorking -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_AIR_CONDITIONED isAcWorking)
    GoToBookingPreferences updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_BOOKING_PREFERENCES)
    BenefitsScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_BENEFITS_SCREEN_FROM_HOME)
    GotoAddUPIScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_ADD_UPI_SCREEN)
    VerifyManualUPI updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ VERIFY_MANUAL_UPI updatedState)
    SwitchPlan plan updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SWITCH_PLAN_FROM_HS plan updatedState)
    GotoHotspotScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GOTO_HOTSPOT_SCREEN updatedState)
    GoToRideReqScreen updatedState -> do
      LatLon lat lon _ <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon updatedState.data.currentDriverLat updatedState.data.currentDriverLon 700 false true
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDE_REQ_SCREEN updatedState lat lon )
    GoToRideSummary updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure GO_TO_RIDE_SUMMARY
    GoToRideSummaryScreen  updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDE_SUMMARY_SCREEN  updatedState)
    UploadParcelImage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_UPLOAD_PARCEL_IMAGE updatedState)
    NotifyDriverReachedDestination updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NOTIFY_DRIVER_REACHED_DESTINATION updatedState)
    UpdateToggleMetroWarriors updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_METRO_WARRIOR updatedState)
    GoToMetroWarriors updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_WARRIOR updatedState)
    MeterFareScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_METER_SCREEN)
