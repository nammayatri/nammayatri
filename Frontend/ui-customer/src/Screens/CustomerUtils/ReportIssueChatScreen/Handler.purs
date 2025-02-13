{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.Handler where

import Types.App

import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import ModifyScreenState (modifyScreenState, FlowState(..))
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReportIssueChatScreen.Controller (ScreenOutput(..))
import Screens.ReportIssueChatScreen.View (screen)
import Screens.ReportIssueChatScreen.ScreenData (ReportIssueChatScreenEntryPoint(..), initData, ReportIssueChatScreenState)
import Data.Maybe 
import Data.String as DS
import Data.Array as DA
import Engineering.Helpers.Utils
import Locale.Utils
import Services.Backend as Remote
import Services.API 
import Components.ChatView.Controller 
import Screens.HelpAndSupportScreen.Transformer (reportIssueMessageTransformer, rideInfoTransformer)
import Engineering.Helpers.Commons 
import JBridge
import Language.Strings
import Language.Types
import Services.Config
import Debug (spy)
import Screens.HomeScreen.ScreenData (dummyRideBooking) as HSD
import Helpers.API (callApiBT) 
import Data.Either(Either(..))
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Screens.Types (TripDetailsGoBackType(..))
import Control.Applicative (unless)
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData

reportIssueChatScreen :: FlowBT String FlowState
reportIssueChatScreen = do
  modifyScreenState $ ReportIssueChatScreenStateType (\state -> state { props {isKeyboardOpen = false}})
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ screen state.reportIssueChatScreen
  case act of
    SelectIssueOption updatedState -> selectIssueOptionHandler updatedState

    UploadIssue updatedState -> uploadIssueHandler updatedState

    CallDriver updatedState -> callDriverHandler updatedState
      
    CallSupport updatedState -> callSupportHandler updatedState
      
    ReopenIssue updatedState -> reOpenIssueHandler updatedState
      
    GoToRideSelectionScreen updatedState -> goToRideSelectionScreenHandler updatedState

    GotoTripDetailsScreen updatedState -> gotoTripDetailsScreenHandler updatedState

    GoToHelpAndSupportScreen updatedState -> goToHelpAndSupportScreenHandler updatedState

    GoToSafetyScreen updatedState -> goToSafetyScreenHandler updatedState

    GoToHomeScreen updatedState -> goToHomeScreenHandler updatedState

    GoToFaqScreen updatedState -> goToFaqScreenHandler updatedState

    RideEndScreen updatedState -> callRideEndScreen updatedState

callRideEndScreen :: ReportIssueChatScreenState -> FlowBT String FlowState
callRideEndScreen updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
  App.BackT $ App.NoBack <$> (pure $ RiderRideEndScreen)

selectIssueOptionHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
selectIssueOptionHandler updatedState = do 

  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
  (GlobalState globalState) <- getState 
  resp <- case globalState.rideSelectionScreen.selectedItem of
                    Just item -> 
                      callApiBT (RideBookingReq item.bookingId)
                    Nothing -> 
                      pure $ HSD.dummyRideBooking 

  let 
    selectedOptionId = fromMaybe "" $ map (\option -> option.issueOptionId) updatedState.data.selectedOption
    selectedOptionLabel = fromMaybe "" $ map (\option -> option.label) updatedState.data.selectedOption
    language = fetchLanguage $ getLanguageLocale languageKey
    isResolved = selectedOptionLabel == "MARK_RESOLVED"
    rideId = fromMaybe "" updatedState.data.tripId
    issueReportId = fromMaybe "" updatedState.data.issueId
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language updatedState.data.selectedCategory.categoryId selectedOptionId rideId issueReportId

  when isResolved do
    let updateIssueReqBody = UpdateIssueReqBody {status : "CLOSED"}
    (UpdateIssueRes _) <- Remote.updateIssue (fromMaybe "" updatedState.data.issueId) language updateIssueReqBody
    pure unit

  let 
    getOptionsRes' = DA.mapWithIndex (
      \index (Option optionObj) -> optionObj { option = optionObj.option}
    ) getOptionsRes.options
   
    messages' = DA.mapWithIndex (
      \index (Message currMessage) -> 
          makeChatComponent' currMessage.message currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1))
    ) getOptionsRes.messages

    chats' = [
      Chat {
        chatId : selectedOptionId
      , chatType : "IssueOption"
      , timestamp : getCurrentUTC ""
      }
    ] <> (
      map (
        \(Message currMessage) -> Chat {
          chatId : currMessage.id, 
          chatType : "IssueMessage", 
          timestamp : getCurrentUTC ""
        }
      ) getOptionsRes.messages
    )

    showSubmitComp = DA.any (\ (Message  message) -> (fromMaybe "" message.label) == "CREATE_TICKET") getOptionsRes.messages 
    isEndFlow' = DA.any (\ (Message  message) -> (fromMaybe "" message.label) == "END_FLOW") getOptionsRes.messages

  when isEndFlow' $ do 
    let 
      postIssueReqBody = PostIssueReqBody {
        mediaFiles : []
      , categoryId : updatedState.data.selectedCategory.categoryId 
      , optionId : Just selectedOptionId
      , description : ""
      , rideId : updatedState.data.tripId
      , chats : updatedState.data.chats <> chats'
      , createTicket : false
      }
    postIssueResp <- Remote.postIssueBT language postIssueReqBody 
    case postIssueResp of
      Right _ -> pure unit
      Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
    pure unit 

  let mandatoryUploads = maybe Nothing (\option -> option.mandatoryUploads) updatedState.data.selectedOption

  modifyScreenState $ ReportIssueChatScreenStateType (
    \_ -> updatedState { 
      data {
        chats = (updatedState.data.chats <> chats')
      , options = getOptionsRes'
      , chatConfig = updatedState.data.chatConfig{
          messages = (updatedState.data.chatConfig.messages <> messages'),
          enableSuggestionClick = false,
          chatSuggestionsList = map (_.option) getOptionsRes'
        }
      , mandatoryUploads = if isJust mandatoryUploads then mandatoryUploads else updatedState.data.mandatoryUploads
      }
    , props {
        isResolved = isResolved
      , showSubmitComp = (not isResolved && showSubmitComp)
      , isEndFlow = isEndFlow'
      }
    }
  )
  modifyScreenState $ HelpAndSupportScreenStateType (
    \helpAndSupportScreen -> helpAndSupportScreen {
      props {
        needIssueListApiCall = true
      }
    }
  )
  App.BackT $ App.NoBack <$> (pure $ IssueReportChatScreenFlow)


uploadIssueHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
uploadIssueHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
  let 
    selectedOptionId = map (\option -> option.issueOptionId) updatedState.data.selectedOption
    language = fetchLanguage $ getLanguageLocale languageKey
    mediaFiles' = case updatedState.data.uploadedAudioId of
      Just audioId -> DA.cons audioId updatedState.data.uploadedImagesIds
      _            -> updatedState.data.uploadedImagesIds
    postIssueReqBody = PostIssueReqBody { 
      mediaFiles  : mediaFiles', 
      categoryId : updatedState.data.selectedCategory.categoryId, 
      optionId : selectedOptionId, 
      description : DS.trim updatedState.data.messageToBeSent, 
      rideId : updatedState.data.tripId,
      chats : updatedState.data.chats,
      createTicket : true
    }

  postIssueResp <- Remote.postIssueBT language postIssueReqBody
  case postIssueResp of
    Left errorPayload -> void $ pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
    Right (PostIssueRes postIssueRes) -> do
      void $ pure $ toast $ getString YOUR_ISSUE_HAS_BEEN_REPORTED
      (IssueInfoRes issueInfoRes) <- Remote.issueInfoBT language postIssueRes.issueReportId
      void $ pure $ hideKeyboardOnNavigation true

      let 
        showDescription = DS.length (DS.trim issueInfoRes.description) > 0
        descMessages = 
          if showDescription then 
            DA.snoc updatedState.data.chatConfig.messages (makeChatComponent' (reportIssueMessageTransformer issueInfoRes.description) Nothing Nothing Nothing "Customer" (getCurrentUTC "") "Text" 500) 
          else
            updatedState.data.chatConfig.messages

        mediaMessages' = DA.mapWithIndex (\index media -> makeChatComponent' media.url Nothing Nothing Nothing "Customer" (getCurrentUTC "") media._type ((index +  1) * 500)) issueInfoRes.mediaFiles
        messages' = DA.concat [
          descMessages
        , mediaMessages'
        , DA.mapWithIndex (
            \index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (DA.length mediaMessages' + 1 + index))
          ) postIssueRes.messages
        ]
      modifyScreenState $ ReportIssueChatScreenStateType (
        \_ -> updatedState { 
          data {
            issueId = Just postIssueRes.issueReportId
          , issueReportShortId = postIssueRes.issueReportShortId
          , chatConfig { 
              messages = messages'
            }
          , messageToBeSent = "" 
          , uploadedAudioId = Nothing
          , uploadedImagesIds = [] 
          }
        , props { 
            showSubmitComp = false 
          } 
        }
      )
      modifyScreenState $ HelpAndSupportScreenStateType (
        \helpAndSupportScreen -> helpAndSupportScreen {
          props {
            needIssueListApiCall = true
          }
        }
      )
  App.BackT $ App.NoBack <$> (pure $ IssueReportChatScreenFlow)


callDriverHandler :: ReportIssueChatScreenState -> FlowBT String FlowState  
callDriverHandler updatedState = do
  let selectedOptionId = fromMaybe "" $ map (\option -> option.issueOptionId) updatedState.data.selectedOption
  case updatedState.data.selectedRide of
    Just ride -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      case ride.driverPhoneNumber of
        Just driverPhoneNumber -> void $ pure $ showDialer driverPhoneNumber false
        Nothing -> do
          pure $ toast $ getString REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON
          void $ Remote.callDriverBT ride.rideId

      let language = fetchLanguage $ getLanguageLocale languageKey
          rideId = fromMaybe "" updatedState.data.tripId
          issueReportId = fromMaybe "" updatedState.data.issueId
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language updatedState.data.selectedCategory.categoryId selectedOptionId rideId issueReportId
      let getOptionsRes' = DA.mapWithIndex (\index (Option optObj) -> optObj{ option = optObj.option}) getOptionsRes.options
          messages' = DA.mapWithIndex (\index (Message currMessage) -> (makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1)))) getOptionsRes.messages
          chats' = [Chat {chatId : selectedOptionId,chatType : "IssueOption",timestamp : (getCurrentUTC "")}] <> 
                    (map (\(Message currMessage) -> Chat {
                      chatId : currMessage.id, 
                      chatType : "IssueMessage",
                      timestamp : (getCurrentUTC "")}) getOptionsRes.messages)
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> updatedState { data {chats = (updatedState.data.chats <> chats'), options = getOptionsRes', chatConfig = updatedState.data.chatConfig{messages = (updatedState.data.chatConfig.messages <> messages')} }, props {showSubmitComp = ((DA.null getOptionsRes'))}})
      App.BackT $ App.NoBack <$> (pure $ IssueReportChatScreenFlow)
    _ -> do
      pure $ toast $ getString PLEASE_SELECT_THE_RIDE_TO_CALL_DRIVER
      (GlobalState globalState) <- getState
      if updatedState.data.entryPoint == TripDetailsScreenEntry then do 
        modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = globalState.tripDetailsScreen.props.fromMyRides}})
        App.BackT $ App.NoBack <$> (pure $ TripDetailsScreenFlow)
      else do 
        App.BackT $ App.NoBack <$> (pure $ HelpAndSupportScreenFlow)


callSupportHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
callSupportHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
  let selectedOptionId = fromMaybe "" (map (\option -> option.issueOptionId) updatedState.data.selectedOption)
  void $ pure $ showDialer (getSupportNumber "") false
  let language = fetchLanguage $ getLanguageLocale languageKey
      rideId = fromMaybe "" updatedState.data.tripId
      issueReportId = fromMaybe "" updatedState.data.issueId
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language updatedState.data.selectedCategory.categoryId selectedOptionId rideId issueReportId
  let getOptionsRes' = DA.mapWithIndex (\index (Option optionObj) -> optionObj {option =  optionObj.option}) getOptionsRes.options
      messages' = DA.mapWithIndex (\index (Message currMessage) -> (makeChatComponent' (reportIssueMessageTransformer currMessage.message) currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1)))) getOptionsRes.messages -- Transformer Not Needed for Issue Flow
      chats' = [Chat {chatId : selectedOptionId, 
                      chatType : "IssueMessage",
                      timestamp : (getCurrentUTC "")}] <> 
              (map (\(Message currMessage) -> Chat {chatId : currMessage.id, 
                                                    chatType : "IssueMessage", 
                                                    timestamp : (getCurrentUTC "")}) getOptionsRes.messages)
  modifyScreenState $ ReportIssueChatScreenStateType (\_ -> updatedState { data {chats = (updatedState.data.chats <> chats'), options = getOptionsRes', chatConfig = updatedState.data.chatConfig{messages = (updatedState.data.chatConfig.messages <> messages')} }, props {showSubmitComp = ((DA.null getOptionsRes'))}})
  App.BackT $ App.NoBack <$> (pure $ IssueReportChatScreenFlow)


reOpenIssueHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
reOpenIssueHandler updatedState = do
  let language = fetchLanguage $ getLanguageLocale languageKey
      selectedOptionId = fromMaybe "" $ map (\option -> option.issueOptionId) updatedState.data.selectedOption
      updateIssueReqBody = UpdateIssueReqBody { status : "REOPENED" }
  (GetOptionsRes _) <- Remote.getOptionsBT language updatedState.data.selectedCategory.categoryId selectedOptionId "" (fromMaybe "" updatedState.data.issueId)
  (UpdateIssueRes updateIssueRes) <- Remote.updateIssue (fromMaybe "" updatedState.data.issueId) language updateIssueReqBody
  let messages' = DA.mapWithIndex (\index (Message currMessage) -> 
                                      makeChatComponent' currMessage.message currMessage.messageTitle currMessage.messageAction currMessage.label "Bot" (getCurrentUTC "") "Text" (500 * (index + 1))
                                  ) updateIssueRes.messages
  modifyScreenState $ ReportIssueChatScreenStateType (\_ -> 
    updatedState { 
      "data" = updatedState.data {
        chatConfig = updatedState.data.chatConfig {
          messages = updatedState.data.chatConfig.messages <> messages'
        }
      },
      props = updatedState.props {
        isResolved = false
      }
    }
  )
  App.BackT $ App.NoBack <$> pure IssueReportChatScreenFlow


goToRideSelectionScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
goToRideSelectionScreenHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState)
  modifyScreenState $ RideSelectionScreenStateType (
    \rideHistoryScreen -> rideHistoryScreen {
      data {
        offsetValue = 0,
        selectedOptionId = (_.issueOptionId) <$> updatedState.data.selectedOption,
        entryPoint = RideSelectionScreenData.HelpAndSupportScreenEntry
      },
      selectedCategory = updatedState.data.selectedCategory
    } 
  )
  App.BackT $ App.NoBack <$> (pure $ RideSelectionScreenFlow)

-- gotoTripDetailsScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
-- gotoTripDetailsScreenHandler updatedState = do
--   modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> initData )
--   App.BackT $ App.NoBack <$> (pure $ TripDetailsScreenFlow)

goToHelpAndSupportScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
goToHelpAndSupportScreenHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> initData )
  App.BackT $ App.NoBack <$> (pure $ HelpAndSupportScreenFlow)

goToSafetyScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
goToSafetyScreenHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> initData )
  App.BackT $ App.NoBack <$> (pure $ ActivateSafetyScreenFlow)

goToHomeScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
goToHomeScreenHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> initData )
  App.BackT $ App.NoBack <$> (pure $ HomeScreenFlow)

gotoTripDetailsScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
gotoTripDetailsScreenHandler updatedState = do 
  (GlobalState globalState) <- getState 
  let selectedRide = fromMaybe dummyIndividualCard updatedState.data.selectedRide
      backPointForTripDetails = if updatedState.data.entryPoint == TripDetailsScreenEntry 
        then globalState.tripDetailsScreen.props.fromMyRides
        else ReportIssueChat
  unless (updatedState.data.entryPoint /= TripDetailsScreenEntry) $
    modifyScreenState $ ReportIssueChatScreenStateType (\_ -> initData)
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen → tripDetailsScreen { 
    data 
    { tripId = selectedRide.shortRideId 
    , source = selectedRide.source
    , destination = selectedRide.destination
    , date = selectedRide.date
    , time = selectedRide.time
    , rating = selectedRide.rating
    , driverName = selectedRide.driverName
    , totalAmount = selectedRide.totalAmount 
    , selectedItem = selectedRide
    , vehicleVariant = selectedRide.vehicleVariant
    },
    props
    { fromMyRides = backPointForTripDetails
    }
  })
  if updatedState.data.entryPoint == TripDetailsScreenEntry then
    App.BackT $ App.NoBack <$> (pure $ TripDetailsScreenFlow)
  else 
    App.BackT $ App.BackPoint <$> (pure TripDetailsScreenFlow)

goToFaqScreenHandler :: ReportIssueChatScreenState -> FlowBT String FlowState
goToFaqScreenHandler updatedState = do
  modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> initData )
  App.BackT $ App.NoBack <$> (pure $ FaqScreenFlow)