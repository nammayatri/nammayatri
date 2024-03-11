{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Controller where

import Prelude (class Show, pure, unit, ($), discard, bind,map,(||),(==),(&&),(/=),(>),(<>),(/), void, not, when)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Screens.Types (HelpAndSupportScreenState,IssueModalType(..),IssueInfo, UpdateDummyTestPopUpType(..))
import PrestoDOM.Types.Core (class Loggable)
import Components.SourceToDestination as SourceToDestinationController
import Screens.HelpAndSupportScreen.ScreenData (IssueOptions(..))
import Language.Strings (getString)
import Services.API (GetRidesHistoryResp,IssueReportDriverListItem(..),Status(..))
import Language.Types(STR(..))
import Services.Config (getSupportNumber)
import JBridge (showDialer, differenceBetweenTwoUTC, openUrlInApp)
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

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HELP_AND_SUPPORT_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen HELP_AND_SUPPORT_SCREEN)
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    SourceToDestinationAction (SourceToDestinationController.Dummy) -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "source_to_destination" "dummy"
    OptionClick optionIndex -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_options"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    SelectRide _ -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "select_ride"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    OpenChat _ -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "open_chat"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    RideHistoryAPIResponse resp -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "ride_history_api_resp"
    NoRidesAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action_view_rides"
    NoAction -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action"
    CheckDummyRide -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_dummy_check_now"
    _ -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_done"


data ScreenOutput = GoBack HelpAndSupportScreenState
                  | GoToWriteToUsScreen
                  | GoToMyRidesScreen CategoryListType HelpAndSupportScreenState
                  | GoToReportIssueChatScreen CategoryListType HelpAndSupportScreenState
                  | IssueListBackPressed HelpAndSupportScreenState
                  | RemoveIssue String HelpAndSupportScreenState
                  | OngoingIssuesScreen HelpAndSupportScreenState
                  | ResolvedIssuesScreen HelpAndSupportScreenState
                  | DriverDummyRideRequest HelpAndSupportScreenState
                  | GoToProfileScreen HelpAndSupportScreenState
data Action = NoAction
             | BackPressed
             | SourceToDestinationAction SourceToDestinationController.Action
             | OptionClick IssueOptions
             | SelectRide CategoryListType
             | OpenChat CategoryListType
             | RideHistoryAPIResponse GetRidesHistoryResp
             | AfterRender
             | NoRidesAction
             | IssueScreenModal IssueList.Action
             | OnClickOngoingIssues
             | OnClickResolvedIssues
             | FetchIssueListApiCall (Array IssueReportDriverListItem)
             | CheckDummyRide
             | ClearTimer
             | UpdateTimer Int String String
             | PopUpModalAction PopUpModal.Action

eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState
eval AfterRender state = continue state
eval BackPressed state = do
  when (not (DS.null state.data.timerId)) $ do
    void $ pure $ TF.clearTimerWithId state.data.timerId
  if state.props.enableDummyPopup then continue state {props{enableDummyPopup = false}}
  else do
    case state.data.goBackTo of
        HELP_AND_SUPPORT_SCREEN -> exit (GoBack state {props{startTimerforDummyRides = false}, data{timerId = "",issueListType =  HELP_AND_SUPPORT_SCREEN_MODAL, goBackTo = HELP_AND_SUPPORT_SCREEN}})
        DRIVER_PROFILE_SCREEN -> exit (GoToProfileScreen state {props{startTimerforDummyRides = false}, data{timerId = ""}})
        _ -> continue state
eval (SourceToDestinationAction (SourceToDestinationController.Dummy)) state = continue state
eval (SelectRide selectedCategory) state = do
  when (not (DS.null state.data.timerId)) $ do
    void $ pure $ TF.clearTimerWithId state.data.timerId
  exit $ GoToMyRidesScreen selectedCategory state {props{startTimerforDummyRides = false}, data{timerId = ""}}

eval (OpenChat selectedCategory)state = do
  when (not (DS.null state.data.timerId)) $ do
    void $ pure $ TF.clearTimerWithId state.data.timerId
  exit $ GoToReportIssueChatScreen selectedCategory state {props{startTimerforDummyRides = false}, data{timerId = ""}}

eval (OptionClick optionIndex) state = do
  when (not (DS.null state.data.timerId)) $ do
    void $ pure $ TF.clearTimerWithId state.data.timerId
  case optionIndex of
    OngoingIssues -> do
      exit $ OngoingIssuesScreen state {data {issueListType = ONGOING_ISSUES_MODAL, timerId = "", goBackTo = HELP_AND_SUPPORT_SCREEN}, props{startTimerforDummyRides = false}}
    ResolvedIssues -> exit $ ResolvedIssuesScreen state {data {issueListType = RESOLVED_ISSUES_MODAL, timerId = "", goBackTo = HELP_AND_SUPPORT_SCREEN}, props{startTimerforDummyRides = false}}
    CallSupportCenter -> do
      void $ pure $ unsafePerformEffect $ contactSupportNumber "" -- TODO: FIX_DIALER -- unsafePerformEffect is temporary fix
      continue state {props{startTimerforDummyRides = false}, data {timerId = "", goBackTo = DRIVER_PROFILE_SCREEN}}
    WhatsAppSupport -> continueWithCmd state [do
        let supportPhone = state.data.cityConfig.registration.supportWAN
        void $ openUrlInApp $ whatsAppSupportLink <> supportPhone
        pure NoAction]

eval (IssueScreenModal (IssueList.AfterRender )) state = do
       when (not (DS.null state.data.timerId)) $ do
        void $ pure $ TF.clearTimerWithId state.data.timerId
       continue state{props{startTimerforDummyRides= false}}

eval (IssueScreenModal (IssueList.BackPressed )) state = do
  when (not (DS.null state.data.timerId)) $ do
    void $ pure $ TF.clearTimerWithId state.data.timerId
  exit $ GoBack state {data {issueListType =  HELP_AND_SUPPORT_SCREEN_MODAL, timerId = ""}, props{startTimerforDummyRides = false}}

eval (IssueScreenModal (IssueList.IssueViewAction (IssueViewController.Remove issueId)  )) state = do
  when (not (DS.null state.data.timerId)) $ do
    void $ pure $ TF.clearTimerWithId state.data.timerId
  exit $ RemoveIssue issueId state{props{startTimerforDummyRides = false}, data{timerId = ""}}

eval (IssueScreenModal (IssueList.IssueViewAction (IssueViewController.CallSupportCenter ))) state = do
       void $ pure $ unsafePerformEffect $ contactSupportNumber ""-- TODO: FIX_DIALER -- unsafePerformEffect is temporary fix
       when (not (DS.null state.data.timerId)) $ do
        void $ pure $ TF.clearTimerWithId state.data.timerId
       continue state{props{startTimerforDummyRides = false}}

eval (FetchIssueListApiCall issueList) state = do
     let apiIssueList = getApiIssueList issueList
         updatedResolvedIssueList = reverse (getUpdatedIssueList "RESOLVED" apiIssueList)
         updatedOngoingIssueList = reverse (getUpdatedIssueList "NEW" apiIssueList)
     continue state {data {issueList =apiIssueList, resolvedIssueList =  updatedResolvedIssueList , ongoingIssueList =  updatedOngoingIssueList}}

eval CheckDummyRide state = exit (DriverDummyRideRequest state{props{startTimerforDummyRides = true}})

eval ClearTimer state = do
    when (not (DS.null state.data.timerId)) $ do
      void $ pure $ TF.clearTimerWithId state.data.timerId
    continue state {data{timerId = ""}}
  
eval (UpdateTimer seconds status timerID) state = do
  if status == "EXPIRED" then do 
    void $ pure $ TF.clearTimerWithId timerID
    continue state{ data {timerId = ""}, props{enableDummyPopup = true, startTimerforDummyRides = false, popupType = TEST_RIDE_RECIEVED} }
    else do
      continue state { data {timerId = timerID} }

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  case state.props.popupType of
    TEST_RIDE_RECIEVED -> continue state {props{popupType = EVERYTHING_OK}}
    _ -> continue state {props{enableDummyPopup = false}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{popupType = PROBLEM_WITH_TEST}}

eval (PopUpModalAction (PopUpModal.OnSecondaryTextClick)) state = do
   when (state.props.popupType == PROBLEM_WITH_TEST) $ do
    void $ pure $ unsafePerformEffect $ contactSupportNumber "" -- TODO: FIX_DIALER -- unsafePerformEffect is temporary fix
   continue state {props{startTimerforDummyRides = false}, data {timerId = "", goBackTo = DRIVER_PROFILE_SCREEN}}
  
eval _ state = continue state

getIssueTitle :: IssueOptions -> String
getIssueTitle menuOption =
  case menuOption of
    OngoingIssues -> (getString ONGOING_ISSUES)
    ResolvedIssues -> (getString RESOLVED_ISSUES)
    CallSupportCenter -> (getString CALL_SUPPORT_CENTER)
    WhatsAppSupport -> (getString GET_SUPPORT_ON_WHATSAPP)

getExactTime :: Int -> String
getExactTime sec = if (sec > 31536000) then (toStringJSON (sec / 31536000)) <> (" ") <> (getString YEARS_AGO)
                    else if (sec > 2592000) then (toStringJSON (sec / 2592000)) <> (" ") <> (getString MONTHS_AGO)
                    else if  (sec > 86400) then (toStringJSON (sec / 86400)) <> (" ") <> (getString DAYS_AGO)
                    else if (sec > 3600) then (toStringJSON (sec / 3600)) <> (" ") <> (getString HOURS_AGO)
                    else if  (sec > 60) then (toStringJSON (sec / 60)) <> (" ") <> (getString MIN_AGO)
                    else (toStringJSON (sec) <> (" ") <> (getString SEC_AGO))

getUpdatedIssueList :: String -> Array IssueInfo -> Array IssueInfo
getUpdatedIssueList status list = (filter (\(issue) -> ((issue.status == status)||(status /= "RESOLVED" && issue.status /= "RESOLVED")) ) list )
