{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Controller where

import Components.IssueList as IssueListController
import Components.IssueView as IssueViewController
import Data.Array (length) as DA
import Accessor (_driverRatings, _contents, _toLocation, _amount, _driverName, _list, _vehicleNumber, _id, _computedPrice, _shortRideId, _rideRating, _vehicleVariant)
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard as IndividualRideCard
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array ((!!), null, filter, reverse, elem)
import Language.Types (STR(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.Utils (validateEmail,strLenWithSpecificCharacters, isParentView, emitTerminateApp, getTime,toStringJSON, fetchImage, FetchImageFrom(..))
import JBridge (showDialer, hideKeyboardOnNavigation,toast, differenceBetweenTwoUTC)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, bind, discard, show, unit, map, ($), (<>), (==), void, (&&), (>), (||),(/), not, (>=))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getKmMeter, fetchVehicleVariant)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types (HelpAndSupportScreenState, DeleteStatus(..), IssueInfo, IssueModalType(..))
import Services.API (IssueReportCustomerListItem(..), RideBookingRes(..), FareBreakupAPIEntity(..), RideAPIEntity(..), BookingLocationAPIEntity(..), RideBookingAPIDetails(..), RideBookingListRes(..))
import Services.Config (getSupportNumber)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Data.String (length, trim, joinWith, splitAt, toUpper, split, Pattern(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.HelpAndSupportScreen.ScreenData (initData)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Foreign.Object (empty)
import ConfigProvider
import Language.Strings( getString)
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen HELP_AND_SUPPORT_SCREEN)
      BackPressed flag -> do
        trackAppBackPress appId (getScreen HELP_AND_SUPPORT_SCREEN)
        if flag then trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "backpress_in_call_confirmation"
        else trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      GenericHeaderActionController act -> case act of
        GenericHeader.PrefixImgOnClick -> do
          trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "generic_header_action" "back_icon"
          trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
        GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "generic_header_action" "forward_icon"
      DeleteGenericHeaderAC act -> case act of
        GenericHeader.PrefixImgOnClick -> do
          trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "generic_header_action" "back_icon"
          trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
        GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "generic_header_action" "forward_icon"
      ContactUs -> do
        trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "contact_us"
        trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      ViewRides -> do
        trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_rides"
        trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      ReportIssue -> do
        trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "report_issue"
        trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
      NoRidesActionController act -> case act of
        ErrorModal.PrimaryButtonActionController act -> case act of
          PrimaryButton.OnClick -> do
            trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "no_rides_error_modal_action" "primary_button_go_home"
            trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
          PrimaryButton.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "error_modal_action" "primary_button_no_action"
      APIFailureActionController act -> case act of
        ErrorModal.PrimaryButtonActionController act -> case act of
          PrimaryButton.OnClick -> do
            trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "api_failure_error_modal_action" "primary_button_go_back"
            trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
          PrimaryButton.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "api_failure_error_modal_action" "primary_button_no_action"
      PopupModelActionController act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "call_driver_cancel"
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "call_driver_accept"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "countdown_updated"
        PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "secondary_text_clicked"
        PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "tip_clicked"
        PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "popup_dismissed"
        PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "option_with_html_clicked"
      SourceToDestinationActionController (SourceToDestination.Dummy) -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "source_to_destination_updated"
      FAQs -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "faq_action"
      RideBookingListAPIResponseAction rideList status -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "ride_booking_list_api_response"
      CallSupport -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "call_support"
      DeleteAccount -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "delete_account"
      EmailEditTextAC act -> case act of
        PrimaryEditText.TextChanged _ _-> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "email_edit_text_changed" "primary_edit_text"
        PrimaryEditText.FocusChanged _-> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "email_edit_text_focus_changed" "primary_edit_text"
      DescriptionEditTextAC act -> case act of
        PrimaryEditText.TextChanged _ _-> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "description_edit_text_changed" "primary_edit_text"
        PrimaryEditText.FocusChanged _-> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "description_edit_text_focus_changed" "primary_edit_text"
      PrimaryButtonAC act -> case act of
        PrimaryButton.OnClick -> do
            trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "primary_button_action" "go_to_home/submit"
            trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "primary_button" "no_action"
      PopUpModalAction act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "delete_account_cancel"
        PopUpModal.OnButton2Click -> do
          trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "delete_account_accept"
          trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "primary_edit_text"
        PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "secondary_text_clicked"
        PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "countdown_updated"
        PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "tip_clicked"
        PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "option_with_html_clicked"
        PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "show_delete_popup_modal_action" "popup_dismissed"
      AccountDeletedModalAction act -> case act of
        PopUpModal.OnButton1Click -> do
          trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "account_deleted"
          trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "button_2"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "primary_edit_text"
        PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "secondary_text_clicked"
        PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "countdown_updated"
        PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "tip_clicked"
        PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "popup_modal_action" "option_with_html_clicked"
        PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "delete_account_popup_modal_action" "popup_dismissed"
      FetchIssueListApiCall issueList -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "fetch_issue_list_api_call"
      ReportedIssue -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "reported_issue"
      ResolvedIssue -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "resolved_issue"
      SelectRide _ -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "select_ride"
      OpenChat _ -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "open_chat"
      IssueScreenModal act -> case act of
        IssueListController.IssueViewAction act -> case act of
          IssueViewController.IssueClick selectedIssue -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "issue_click"
          IssueViewController.Remove _ -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "issue_remove"
          IssueViewController.CallSupportCenter -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "CallSupportCenter"
        IssueListController.BackPressed -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "back_press"
        IssueListController.AfterRender -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "screen" "after_render"

      -- _ -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_done"

data Action = BackPressed Boolean
            | SourceToDestinationActionController SourceToDestination.Action
            | GenericHeaderActionController GenericHeader.Action
            | ContactUs
            | FAQs
            | ViewRides
            | ReportIssue
            | RideBookingListAPIResponseAction RideBookingListRes String
            | NoRidesActionController ErrorModal.Action
            | APIFailureActionController ErrorModal.Action
            | PopupModelActionController PopUpModal.Action
            | AfterRender
            | CallSupport
            | DeleteAccount
            | DeleteGenericHeaderAC GenericHeader.Action
            | EmailEditTextAC PrimaryEditText.Action
            | DescriptionEditTextAC PrimaryEditText.Action
            | PrimaryButtonAC PrimaryButton.Action
            | PopUpModalAction PopUpModal.Action
            | AccountDeletedModalAction PopUpModal.Action
            | FetchIssueListApiCall (Array IssueReportCustomerListItem)
            | ReportedIssue
            | ResolvedIssue
            | SelectRide CategoryListType
            | OpenChat CategoryListType
            | IssueScreenModal IssueList.Action

data ScreenOutput = GoBack HelpAndSupportScreenState
                  | GoToSupportScreen String HelpAndSupportScreenState
                  | GoToTripDetails HelpAndSupportScreenState
                  | GoToMyRides HelpAndSupportScreenState
                  | GoHome HelpAndSupportScreenState
                  | UpdateState HelpAndSupportScreenState
                  | GoToRideSelectionScreen CategoryListType HelpAndSupportScreenState
                  | GoToChatScreen CategoryListType HelpAndSupportScreenState
                  | GoToHelpAndSupportScreen HelpAndSupportScreenState 
                  | ConfirmDeleteAccount HelpAndSupportScreenState 
                  | GoToOldChatScreen IssueInfo HelpAndSupportScreenState

eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState

eval (BackPressed _ ) state = 
  if state.data.issueListType == REPORTED_ISSUES_MODAL || state.data.issueListType == RESOLVED_ISSUES_MODAL then continue state {data {issueListType = HELP_AND_SUPPORT_SCREEN_MODAL}}
  else if state.props.isCallConfirmation then continue state{props{isCallConfirmation = false}}
  else if state.data.accountStatus == CONFIRM_REQ then continue state{data{accountStatus = ACTIVE}}
  else if state.data.accountStatus == DEL_REQUESTED then updateAndExit (state {data{accountStatus = ACTIVE}} ) $ GoHome state
  else if state.props.showDeleteAccountView then continue state{props {showDeleteAccountView = false}}
  else 
    if isParentView FunctionCall 
      then do 
        void $ pure $ emitTerminateApp Nothing true
        continue state
      else exit $ GoBack state

eval ContactUs state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_help_and_support_email"
  exit $ GoToSupportScreen state.data.bookingId state

eval (IssueScreenModal (IssueListController.IssueViewAction (IssueViewController.IssueClick selectedIssue))) state = exit $ GoToOldChatScreen selectedIssue state
eval (IssueScreenModal (IssueListController.BackPressed)) state = exit $ GoToHelpAndSupportScreen state {data {issueListType = HELP_AND_SUPPORT_SCREEN_MODAL}}

eval (SelectRide selectedCategory) state = exit $ GoToRideSelectionScreen selectedCategory state

eval (OpenChat selectedCategory) state = exit $ GoToChatScreen selectedCategory state

eval ReportedIssue state = continue state {data {issueListType = REPORTED_ISSUES_MODAL}}

eval ResolvedIssue state = continue state {data {issueListType = RESOLVED_ISSUES_MODAL}}

eval ReportIssue state = exit $ GoToTripDetails state

eval CallSupport state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_help_and_support_call_click"
  continue state{props{isCallConfirmation = true}}

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure $ BackPressed state.props.isCallConfirmation]

eval ViewRides state = exit $ GoToMyRides state

eval (RideBookingListAPIResponseAction rideList status) state = do
  let email = if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else ""
      updatedState = state{data{email = email}}
  case status of
      "success" -> do
                    if (null (rideList^._list)) then continue updatedState {data{isNull = true}, props{apiFailure = false}}
                      else do
                        let list = myRideListTransform state (rideList ^._list)
                        case (null (list)) of
                          true -> continue updatedState {data{isNull = true }, props{apiFailure=false}}
                          _    -> do
                                    let newState = (myRideListTransform state (rideList ^._list))!!0
                                    updateAndExit (fromMaybe initData newState) (UpdateState (fromMaybe initData newState))
      "failure"   -> continue updatedState{props{apiFailure = true}}
      _           -> continue updatedState

eval (PopupModelActionController (PopUpModal.OnButton1Click)) state = continue state{props{isCallConfirmation = false}}

eval (PopupModelActionController (PopUpModal.OnButton2Click)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_help_and_support_call_performed"
  void $ pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
  continue state{props{isCallConfirmation = false}}

eval (APIFailureActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ GoBack state

eval (NoRidesActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ GoHome state

eval (EmailEditTextAC (PrimaryEditText.TextChanged _ a)) state = continue state{data {email = trim(a)},props{btnActive = length (trim a) > 0  && (strLenWithSpecificCharacters (trim state.data.description) "[a-zA-Z]") > 9 && validateEmail a}}

eval (DescriptionEditTextAC (PrimaryEditText.TextChanged id a)) state = do 
  let email= if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else state.data.email
  continue state{data {description = a},props{btnActive = length email > 0 && (strLenWithSpecificCharacters (trim a) "[a-zA-Z]")  > 9 && validateEmail email}}

eval DeleteAccount state = do
 continue state {props {showDeleteAccountView = true}, data{description = "", email = ""}}

eval (DeleteGenericHeaderAC(GenericHeader.PrefixImgOnClick )) state = continue state {props {showDeleteAccountView = false}}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state{ data { accountStatus = CONFIRM_REQ} }

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {data{ accountStatus= ACTIVE}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do 
  let email = if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else state.data.email
  exit $ ConfirmDeleteAccount state{data{email=email}}

eval (AccountDeletedModalAction (PopUpModal.OnButton2Click)) state =  exit $ GoHome state {data{accountStatus = ACTIVE}, props {showDeleteAccountView = false}} 

eval (FetchIssueListApiCall issueList) state = do
     let apiIssueList = getApiIssueList issueList
         updatedResolvedIssueList = getUpdatedIssueList ["CLOSED"] apiIssueList
         updatedOngoingIssueList =  getUpdatedIssueList ["OPEN", "PENDING", "RESOLVED", "REOPENED"] apiIssueList
     continue state {data {issueList =apiIssueList, resolvedIssueList =  updatedResolvedIssueList , ongoingIssueList =  updatedOngoingIssueList}}
eval _ state = continue state

myRideListTransform :: HelpAndSupportScreenState -> Array RideBookingRes -> Array HelpAndSupportScreenState
myRideListTransform state listRes = filter (\item -> (item.data.status == "COMPLETED")) (map(\(RideBookingRes ride) ->
    let
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
    config = getAppConfig appConfig
      in  {
        data:{
          date : (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
          time : (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
          source: decodeAddress (Booking ride.fromLocation),
          destination: (decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation))),
          rating: (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _rideRating)),
          driverName :((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _driverName) ,
          totalAmount : (config.currency <> " " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice))),
          status : (ride.status),
          isNull : false,
          rideStartTime : (convertUTCtoISC (fromMaybe "" ride.rideStartTime )"h:mm A"),
          rideEndTime : (convertUTCtoISC (fromMaybe "" ride.rideEndTime )"h:mm A"),
          vehicleNumber : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleNumber),
          rideId : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._id),
          tripId : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._shortRideId),
          bookingId : ride.id,
          faresList : updatedFareList,
          config : config,
          email : "",
          description : "",
          accountStatus : ACTIVE,
          vehicleVariant : fetchVehicleVariant ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleVariant),
          logField : empty,
          issueList : [],
          resolvedIssueList : [],
          ongoingIssueList : [],
          issueListType : HELP_AND_SUPPORT_SCREEN_MODAL,
          categories : state.data.categories,
          merchantExoPhone : ride.merchantExoPhone
          },
      props : {
        apiFailure : false
      , isCallConfirmation : false
      , showDeleteAccountView : false
      , btnActive : false
      }
      }) listRes)


dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0,description: ""}

isEmailPresent :: LazyCheck -> Boolean
isEmailPresent _ = not ( getValueToLocalStore USER_EMAIL == "__failed" || getValueToLocalStore USER_EMAIL == "(null)" )

getApiIssueList :: Array IssueReportCustomerListItem -> Array IssueInfo
getApiIssueList issueList = (map (\(IssueReportCustomerListItem issue) -> {
   issueReportId : issue.issueReportId,
   status : issue.status,
   category : if (getLanguageLocale languageKey == "EN_US")
                then
                  joinWith " " (map (\catName ->
                    let { before, after } = splitAt 1 catName
                    in (toUpper before <> after)
                  ) (split (Pattern " ") issue.category))
                else issue.category,
   createdAt : getExactTime (runFn2 differenceBetweenTwoUTC (getCurrentUTC "") (issue.createdAt)) issue.createdAt
}) issueList)

getExactTime :: Int -> String -> String 
getExactTime sec createdAt = 
   if sec >= 86400 then convertUTCtoISC createdAt "DD/MM/YYYY"
   else 
    let {base , suffix}=    if sec >= 3600 then {base : sec / 3600, suffix : getString HOURS_AGO}
                            else if sec >= 60 then {base : sec / 60,  suffix : getString MIN_AGO}
                            else {base: sec , suffix: getString SEC_AGO}
    in toStringJSON (base) <> " " <> suffix

getUpdatedIssueList :: Array String -> Array IssueInfo -> Array IssueInfo
getUpdatedIssueList statusList list = filter (\(issue) -> elem issue.status statusList ) list 

topicsList :: HelpAndSupportScreenState -> Array CategoryListType
topicsList state =  (if state.data.config.feature.enableSelfServe 
  then 
    state.data.categories 
  else 
    [{ categoryAction : "CONTACT_US"
    , categoryName : getString FOR_OTHER_ISSUES_WRITE_TO_US
    , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_clip_board"
    , categoryId : "5"
    },
    { categoryAction : "CALL_SUPPORT"
    , categoryName : getString CONTACT_SUPPORT
    , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_help"
    , categoryId : "6"
    }]
  ) <> if state.data.config.showDeleteAccount 
          then 
            [{ categoryAction : "DELETE_ACCOUNT"
            , categoryName : getString REQUEST_TO_DELETE_ACCOUNT
            , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_delete_account"
            , categoryId : "7"
            }] 
          else 
            []

reportsList :: HelpAndSupportScreenState -> Array CategoryListType
reportsList state = []
  <> if null state.data.ongoingIssueList then [] else [
        { categoryAction : "REPORTED"
        , categoryName : getString REPORTED <> " : " <> (toStringJSON (DA.length (state.data.ongoingIssueList)))
        , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_reported"
        , categoryId : "1"
        }]
  <> if null state.data.resolvedIssueList then [] else [
        { categoryAction : "CLOSED"
        , categoryName : getString RESOLVED <> " : " <> (toStringJSON (DA.length (state.data.resolvedIssueList)))
        , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_resolved"
        , categoryId : "2"
        }]