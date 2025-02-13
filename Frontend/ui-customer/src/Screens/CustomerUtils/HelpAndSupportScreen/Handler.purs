{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Screens.HelpAndSupportScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HelpAndSupportScreen.View as HelpAndSupportScreen
import Components.SettingSideBar.Controller as SettingSideBar
import ModifyScreenState (modifyScreenState, FlowState(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType(..))
import Screens.HelpAndSupportScreen.Transformer (isEmailPresent)
import Data.Array as DA
import Data.String as DS 
import Services.Backend as Remote
import JBridge
import Engineering.Helpers.Utils
import Locale.Utils
import Services.API
import Mobility.Prelude
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.Types as ST
import Data.Maybe
import Components.ChatView (makeChatComponent')
import Screens.HelpAndSupportScreen.Transformer(reportIssueMessageTransformer)
import Engineering.Helpers.Commons (getCurrentUTC)
import Screens.HelpAndSupportScreen.ScreenData
import Components.IssueView (IssueInfo)
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Debug (spy)
import Language.Strings (getString)
import Language.Types

helpAndSupportScreen :: FlowBT String FlowState
helpAndSupportScreen = do
  (GlobalState globalState) <- getState
  let helpAndSupportScreenState = globalState.helpAndSupportScreen
  if DA.null helpAndSupportScreenState.data.categories then do 
    let language = fetchLanguage $ getLanguageLocale languageKey 
    (GetCategoriesRes response) <- Remote.getCategoriesBT language
    filteredCategories <- pure $ DA.filter (\(Category catObj) -> catObj.categoryType == "Category") response.categories
    let isFaqListEmpty' = DA.null $ DA.filter (\(Category catObj) -> catObj.categoryType == "FAQ") response.categories
    let categories' = map (\(Category catObj) ->{ categoryName : if (language == "en") then capitalize catObj.category else catObj.category , categoryId : catObj.issueCategoryId, categoryAction : Just catObj.label, categoryImageUrl : Just catObj.logoUrl, isRideRequired : catObj.isRideRequired , maxAllowedRideAge : catObj.maxAllowedRideAge, categoryType : catObj.categoryType, allowedRideStatuses : catObj.allowedRideStatuses}) filteredCategories
    modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data {categories = categories', isFaqListEmpty = isFaqListEmpty'} } )
  else pure unit
  modifyScreenState $ ReportIssueChatScreenStateType (\reportIssueChatScreen -> reportIssueChatScreen { data { entryPoint = ReportIssueChatScreenData.HelpAndSupportScreenEntry }})
  (GlobalState updatedGlobalState) <- getState
  act <- lift $ lift $ runScreen $ HelpAndSupportScreen.screen updatedGlobalState.helpAndSupportScreen
  
  case act of
    GoBack updatedState -> goBackHandler updatedState
    GoHome updatedState -> goHomeHandler updatedState
    GoToSupportScreen bookingId updatedState-> goToSupportScreenHandler bookingId updatedState
    GoToTripDetails state-> goToTripDetailsHandler state
    GoToMyRides updatedState -> goToMyRidesHandler updatedState
    UpdateState updatedState -> updateStateHandler updatedState
    ConfirmDeleteAccount updatedState -> confirmDeleteAccountHandler updatedState
    GoToHelpAndSupportScreen updatedState -> goToHelpAndSupportScreenHanlder updatedState
    GoToRideSelectionScreen selectedCategory updatedState -> goToRideSelectionScreenHandler selectedCategory updatedState
    GoToChatScreen selectedCategory updatedState -> goToChatScreenHandler selectedCategory updatedState
    GoToSelectFaqScreen selectedIssue updatedState -> goToSelectFaqScreenHandler selectedIssue updatedState
    GoToOldChatScreen selectedIssue updatedState-> goToOldChatScreenHandler selectedIssue updatedState


goBackHandler :: HelpAndSupportScreenState -> FlowBT String FlowState
goBackHandler updatedState =  do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure $ if updatedState.data.fromScreen == "RideCompleted" then RiderRideCompleted else HomeScreenFlow)


goHomeHandler :: HelpAndSupportScreenState -> FlowBT String FlowState
goHomeHandler updatedState = do 
  modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBar.CLOSED}}}) 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure $ HomeScreenFlow)


goToSupportScreenHandler :: String -> HelpAndSupportScreenState -> FlowBT String FlowState
goToSupportScreenHandler bookingId updatedState = do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  modifyScreenState $ ContactUsScreenStateType (
    \contactUsScreen -> contactUsScreen {
      data{
        bookingId = bookingId
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure ContactUsScreenFlow)

goToTripDetailsHandler :: HelpAndSupportScreenState -> FlowBT String FlowState
goToTripDetailsHandler state = do
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> state)
  modifyScreenState $ TripDetailsScreenStateType (
    \tripDetailsScreen -> tripDetailsScreen {
      data {
        tripId = state.data.tripId
      , vehicleVariant = state.data.vehicleVariant
      , selectedItem {
          status = state.data.status
        , rideCreatedAt = state.data.rideCreatedAt
        , rideStatus = state.data.rideStatus
        , faresList = state.data.faresList 
        , date = state.data.date
        , bookingId = state.data.bookingId
        , rideStartTime = state.data.rideStartTime
        , rideStartTimeUTC = state.data.rideStartTimeUTC
        , rideEndTime = state.data.rideEndTime
        , rideEndTimeUTC = state.data.rideEndTimeUTC
        , rideId = state.data.rideId
        , vehicleNumber = state.data.vehicleNumber
        , time = state.data.time
        , source = state.data.source
        , destination = state.data.destination 
        , driverName = state.data.driverName 
        , totalAmount = state.data.totalAmount
        , rating = state.data.rating
        , shortRideId = state.data.tripId
        , merchantExoPhone = state.data.merchantExoPhone
        }
      , date = state.data.date
      , time = state.data.time
      , source = state.data.source
      , destination = state.data.destination
      , driverName = state.data.driverName
      , totalAmount = state.data.totalAmount
      , rating = state.data.rating
      , categories = DA.filter (\(category) -> category.categoryType == "Category") state.data.categories
      }
    , props {
        fromMyRides = ST.HelpAndSupport
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure TripDetailsScreenFlow )


goToMyRidesHandler :: HelpAndSupportScreenState -> FlowBT String FlowState
goToMyRidesHandler updatedState = do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  modifyScreenState $ MyRideScreenStateType (
    \myRidesScreen -> myRidesScreen {
      data{
        offsetValue = 0
      }, 
      props{
        fromNavBar = false
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure MyRidesScreenFlow )


updateStateHandler :: HelpAndSupportScreenState -> FlowBT String FlowState
updateStateHandler updatedState = do
  let email = if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else "" 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ ->  updatedState{data{email=email}})
  App.BackT $ App.BackPoint <$> (pure HelpAndSupportScreenFlow )


confirmDeleteAccountHandler :: HelpAndSupportScreenState -> FlowBT String FlowState 
confirmDeleteAccountHandler updatedState = do
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  void $ Remote.sendIssueBT (Remote.makeSendIssueReq (Just updatedState.data.email) Nothing "Request To Delete Account" updatedState.data.description $ Just false)
  modifyScreenState $ HelpAndSupportScreenStateType (
    \helpAndSupportScreen -> helpAndSupportScreen {
      props{
        showDeleteAccountView = true
      }, 
      data {
        accountStatus = ST.DEL_REQUESTED
      }
    }
  )
  App.BackT $ App.NoBack <$> (pure HelpAndSupportScreenFlow )


goToHelpAndSupportScreenHanlder :: HelpAndSupportScreenState -> FlowBT String FlowState
goToHelpAndSupportScreenHanlder updatedState =  do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure HelpAndSupportScreenFlow )


goToRideSelectionScreenHandler :: CategoryListType -> HelpAndSupportScreenState -> FlowBT String FlowState
goToRideSelectionScreenHandler selectedCategory updatedState = do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  modifyScreenState $ RideSelectionScreenStateType (
    \rideHistoryScreen -> rideHistoryScreen {
      data {
        offsetValue = 0,
        selectedOptionId = Nothing,
        entryPoint = RideSelectionScreenData.HelpAndSupportScreenEntry
      },
      selectedCategory = selectedCategory
    } 
  )
  App.BackT $ App.BackPoint <$> (pure RideSelectionScreenFlow )


goToChatScreenHandler :: CategoryListType -> HelpAndSupportScreenState -> FlowBT String FlowState 
goToChatScreenHandler selectedCategory updatedState =  do
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState) 
  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language selectedCategory.categoryId "" "" ""
  let 
    options' = DA.mapWithIndex (\index (Option optionObj) -> optionObj{ option = optionObj.option}) getOptionsRes.options
    messages' = DA.mapWithIndex (\index (Message currMessage) -> makeChatComponent' currMessage.message currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1))) getOptionsRes.messages
    chats' = map (
      \(Message currMessage) -> Chat {
        chatId : currMessage.id,
        chatType : "IssueMessage",
        timestamp : getCurrentUTC ""
      }
    ) getOptionsRes.messages

  modifyScreenState $ ReportIssueChatScreenStateType (
    \updatedState ->  updatedState {
      data {
        chats = chats'
      , selectedCategory = selectedCategory {categoryName = getString REPORT_AN_ISSUE}
      , options = options'
      , chatConfig = ReportIssueChatScreenData.initData.data.chatConfig{
          messages = messages'
        }
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure $ IssueReportChatScreenFlow)

goToSelectFaqScreenHandler :: CategoryListType -> HelpAndSupportScreenState -> FlowBT String FlowState
goToSelectFaqScreenHandler selectedCategory updatedState = do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  modifyScreenState $ SelectFaqScreenStateType (
    \updatedState ->  updatedState {
      data {
        categoryName = getString FAQ
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure SelectFaqScreenFlow )



goToOldChatScreenHandler :: IssueInfo -> HelpAndSupportScreenState -> FlowBT String FlowState
goToOldChatScreenHandler selectedIssue updatedState = do 
  modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
  let language =  fetchLanguage $ getLanguageLocale languageKey
  (IssueInfoRes issueInfoRes) <- Remote.issueInfoBT language selectedIssue.issueReportId
  let 
    options' = DA.mapWithIndex (\index (Option optionObj) -> optionObj{ option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option)}) issueInfoRes.options
    messages' = DA.mapWithIndex (\_ (ChatDetail currMessage) -> makeChatComponent' (reportIssueMessageTransformer (fromMaybe "" currMessage.content)) currMessage.title currMessage.actionText currMessage.label (if currMessage.sender == "USER" then "Customer" else "Bot") currMessage.timestamp currMessage.chatType 0)issueInfoRes.chats
    showStillHaveIssue' = case (DA.last issueInfoRes.chats) of
      Just (ChatDetail msg) -> (fromMaybe "" msg.label) == "AUTO_MARKED_RESOLVED"
      Nothing -> false 
    isResolved' =  DA.any (\x -> x == selectedIssue.status) ["CLOSED", "NOT_APPLICABLE"]
  modifyScreenState $ ReportIssueChatScreenStateType (
    \ _ -> ReportIssueChatScreenData.initData { 
      data {
        entryPoint = ReportIssueChatScreenData.OldChatEntry
        , showStillHaveIssue = showStillHaveIssue'
        , selectedCategory = {
              categoryName : getString REPORT_AN_ISSUE
            , categoryImageUrl : Nothing
            , categoryAction : Nothing
            , categoryId : issueInfoRes.categoryId
            , isRideRequired : false
            , maxAllowedRideAge : Nothing
            , allowedRideStatuses : Nothing
            , categoryType: "Category"
        }
        , options = options'
        , issueId = Just selectedIssue.issueReportId
        , issueReportShortId = issueInfoRes.issueReportShortId
        , chatConfig = ReportIssueChatScreenData.initData.data.chatConfig {
            messages = messages'
          } 
        }
    , props {
        showSubmitComp = false
      , isResolved = isResolved'
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure IssueReportChatScreenFlow)