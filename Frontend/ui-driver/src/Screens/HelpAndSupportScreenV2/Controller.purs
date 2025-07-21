{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreenV2.Controller where

import Prelude (class Show, pure, unit, ($), discard, bind,map,(||),(==),(&&),(/=),(>),(<>),(/), void, not, when)
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import Screens.Types (HelpAndSupportScreenState,IssueModalType(..),IssueInfo, UpdateDummyTestPopUpType(..))
import PrestoDOM.Types.Core (class Loggable)
import Components.SourceToDestination as SourceToDestinationController
import Screens.HelpAndSupportScreen.ScreenData (IssueOptions(..))
import Language.Strings (getString)
import Services.API as API
import Language.Types(STR(..))
import JBridge (showDialer, differenceBetweenTwoUTC, openUrlInApp, openUrlInMailApp, openNavigation)
import Helpers.Utils (getTime,getCurrentUTC,toStringJSON, contactSupportNumber)
import Data.Array (foldr,cons,filter,reverse)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Components.IssueList as IssueList
import Screens (ScreenName(..), getScreen)
import Common.Types.App (LazyCheck(..), CategoryListType)
import Prelude ((<>))
import Effect.Unsafe (unsafePerformEffect)
import Components.IssueView.Controller as IssueViewController
import Data.Function.Uncurried (runFn2)
import Screens.HelpAndSupportScreen.Transformer (getApiIssueList)
import Timers as TF
import Data.String as DS
import Components.PopUpModal as PopUpModal
import Constants
import Resource.Constants (mailToLink)
import RemoteConfig.Utils (getHelpAndSupportConfig)
import Storage
import Debug (spy)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HELP_AND_SUPPORT_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen HELP_AND_SUPPORT_SCREEN)
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    NoRidesAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action_view_rides"
    NoAction -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action"
    CheckDummyRide -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_dummy_check_now"
    _ -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_done"


data ScreenOutput = GoBack HelpAndSupportScreenState
                  | DriverDummyRideRequest HelpAndSupportScreenState
                  | GoToProfileScreen HelpAndSupportScreenState
                  | GoToHomeScreen HelpAndSupportScreenState
                  | GoToTripDetailsScreen HelpAndSupportScreenState
                  | ShowOperationHubs HelpAndSupportScreenState
                  | GotoOnboardingFAQScreen HelpAndSupportScreenState
data Action = NoAction
             | BackPressed
             | SourceToDestinationAction SourceToDestinationController.Action
             | AfterRender
             | NoRidesAction
             | CheckDummyRide
             | PopUpModalAction PopUpModal.Action
             | HelpAndSupportCategoryAC CategoryListType
             | CallHub String
             | OpenMaps API.OperationHub

eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState
eval AfterRender state = continue state
eval BackPressed state = do 
  if state.props.showOperationsHub then continue state {props{showOperationsHub = false}}
  else if not (state.data.issueListType == HELP_AND_SUPPORT_SCREEN_MODAL) then do
     exit (GoBack state {props{startTimerforDummyRides = false}, data{issueListType =  HELP_AND_SUPPORT_SCREEN_MODAL}})
  else if state.props.enableDummyPopup then continue state {props{enableDummyPopup = false}}
  else do
    case state.data.goBackTo of
        HELP_AND_SUPPORT_SCREEN -> exit (GoBack state {props{startTimerforDummyRides = false}, data{timerId = "",issueListType =  HELP_AND_SUPPORT_SCREEN_MODAL}})
        DRIVER_PROFILE_SCREEN -> exit (GoToProfileScreen state {props{startTimerforDummyRides = false}, data{timerId = ""}})
        HOME_SCREEN -> exit (GoToHomeScreen state {props{startTimerforDummyRides = false}, data{timerId = ""}})
        TRIP_DETAILS_SCREEN -> exit (GoToTripDetailsScreen state {props{startTimerforDummyRides = false}, data{timerId = ""}})
        _ -> continue state

eval (HelpAndSupportCategoryAC categoryGroup) state = do
    let helpAndSupportConfig = getHelpAndSupportConfig (getValueToLocalStore DRIVER_LOCATION)
    case categoryGroup.categoryType of
        "FAQS" -> updateAndExit state $ GotoOnboardingFAQScreen state
        "WHATSAPP_CHAT" -> continueWithCmd state [do
            void $ openUrlInApp $ whatsAppSupportLink <> helpAndSupportConfig.supportWAN
            pure NoAction]
        "MAIL_US" -> continueWithCmd state [do
            void $ openUrlInMailApp $ mailToLink <> helpAndSupportConfig.supportMail
            pure NoAction]
        "CALL_US" -> do
            void $ pure $ showDialer helpAndSupportConfig.supportNumber false
            continue state 
        "OPERATIONS_HUB" -> updateAndExit state {props{showOperationsHub = true}} $ ShowOperationHubs state {props{showOperationsHub = true}}
        _ -> continue state 

eval CheckDummyRide state = exit (DriverDummyRideRequest state{props{startTimerforDummyRides = true}})

eval (CallHub mobileNumber) state = do
  void $ pure $ unsafePerformEffect $ contactSupportNumber mobileNumber
  continue state

eval (OpenMaps (API.OperationHub hub)) state = do
  void $ pure $ openNavigation hub.lat hub.lon "DRIVE"
  continue state

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  case state.props.popupType of
    TEST_RIDE_RECIEVED -> continue state {props{popupType = EVERYTHING_OK}}
    _ -> continue state {props{enableDummyPopup = false}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{popupType = PROBLEM_WITH_TEST}}

eval (PopUpModalAction (PopUpModal.OnSecondaryTextClick)) state = do
   if (state.props.popupType == PROBLEM_WITH_TEST) then do
    void $ pure $ unsafePerformEffect $ contactSupportNumber ""
    else pure unit
   continue state {props{startTimerforDummyRides = false}, data {timerId = ""}}
  
eval _ state = update state