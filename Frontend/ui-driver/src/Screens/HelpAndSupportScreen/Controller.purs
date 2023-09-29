{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Controller where

import Components.IssueView as IssueViewController
import Prelude (class Show, pure, unit, ($), discard, bind,map,(||),(==),(&&),(/=),(>),(<>),(/))
import PrestoDOM (Eval, continue, exit)
import Screens.Types (CategoryListType,HelpAndSupportScreenState,IssueModalType(..),IssueInfo)
import PrestoDOM.Types.Core (class Loggable)
import Components.SourceToDestination as SourceToDestinationController
import Screens.HelpAndSupportScreen.ScreenData (IssueOptions(..))
import Language.Strings (getString)
import Services.API (GetRidesHistoryResp,IssueReportDriverListItem(..),Status(..))
import Language.Types(STR(..))
import Services.Config (getSupportNumber)
import JBridge (showDialer, differenceBetweenTwoUTC)
import Helpers.Utils (getTime,getCurrentUTC,toStringJSON)
import Data.Array (foldr,cons,filter,reverse)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Components.IssueList as IssueList
import Screens (ScreenName(..), getScreen)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

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
    _ -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_done"


data ScreenOutput = GoBack HelpAndSupportScreenState
                  | GoToWriteToUsScreen
                  | GoToMyRidesScreen CategoryListType
                  | GoToReportIssueChatScreen CategoryListType
                  | IssueListBackPressed HelpAndSupportScreenState
                  | RemoveIssue String HelpAndSupportScreenState
                  | OngoingIssuesScreen HelpAndSupportScreenState
                  | ResolvedIssuesScreen HelpAndSupportScreenState

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


eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState
eval AfterRender state = continue state
eval BackPressed state = exit (GoBack state)
eval (SourceToDestinationAction (SourceToDestinationController.Dummy)) state = continue state
eval (SelectRide selectedCategory) state = exit $ GoToMyRidesScreen selectedCategory
eval (OpenChat selectedCategory)state = exit $ GoToReportIssueChatScreen selectedCategory

eval (OptionClick optionIndex) state = do
  case optionIndex of
    OngoingIssues -> exit $ OngoingIssuesScreen state {data {issueListType = ONGOING_ISSUES_MODAL}}
    ResolvedIssues -> exit $ ResolvedIssuesScreen state {data {issueListType = RESOLVED_ISSUES_MODAL}}
    CallSupportCenter -> do
      _ <- pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
      continue state
eval (IssueScreenModal (IssueList.AfterRender )) state = continue state
eval (IssueScreenModal (IssueList.BackPressed )) state = exit (GoBack state {data {issueListType =  HELP_AND_SUPPORT_SCREEN_MODAL  }})
eval (IssueScreenModal (IssueList.IssueViewAction (IssueViewController.Remove issueId)  )) state = exit $ RemoveIssue issueId state
eval (IssueScreenModal (IssueList.IssueViewAction (IssueViewController.CallSupportCenter ))) state = do
       _ <- pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
       continue state
eval (FetchIssueListApiCall issueList) state = do
     let apiIssueList = getApiIssueList issueList
         updatedResolvedIssueList = reverse (getUpdatedIssueList "RESOLVED" apiIssueList)
         updatedOngoingIssueList = reverse (getUpdatedIssueList "NEW" apiIssueList)
     continue state {data {issueList =apiIssueList, resolvedIssueList =  updatedResolvedIssueList , ongoingIssueList =  updatedOngoingIssueList}}
eval _ state = continue state

getIssueTitle :: IssueOptions -> String
getIssueTitle menuOption =
  case menuOption of
    OngoingIssues -> (getString ONGOING_ISSUES)
    ResolvedIssues -> (getString RESOLVED_ISSUES)
    CallSupportCenter -> (getString CALL_SUPPORT_CENTER)

getApiIssueList :: Array IssueReportDriverListItem -> Array IssueInfo
getApiIssueList issueList = (map (\(IssueReportDriverListItem issue) -> {
   issueReportId : issue.issueReportId,
   status : issue.status,
   category : (case issue.category of
                  "lost and found" -> "Lost Item"
                  "app related" -> "App Related Issue"
                  "ride related" -> "Ride Related Issue"
                  "fare" -> "Fare Related Issue"
                  _ -> ""
              ),
   createdAt : (getExactTime (differenceBetweenTwoUTC (getCurrentUTC "") (issue.createdAt)))
}) issueList)

getExactTime :: Int -> String
getExactTime sec = if (sec > 31536000) then (toStringJSON (sec / 31536000)) <> (" ") <> (getString YEARS_AGO)
                    else if (sec > 2592000) then (toStringJSON (sec / 2592000)) <> (" ") <> (getString MONTHS_AGO)
                    else if  (sec > 86400) then (toStringJSON (sec / 86400)) <> (" ") <> (getString DAYS_AGO)
                    else if (sec > 3600) then (toStringJSON (sec / 3600)) <> (" ") <> (getString HOURS_AGO)
                    else if  (sec > 60) then (toStringJSON (sec / 60)) <> (" ") <> (getString MIN_AGO)
                    else (toStringJSON (sec) <> (" ") <> (getString SEC_AGO))

getUpdatedIssueList :: String -> Array IssueInfo -> Array IssueInfo
getUpdatedIssueList status list = (filter (\(issue) -> ((issue.status == status)||(status /= "RESOLVED" && issue.status /= "RESOLVED")) ) list )
