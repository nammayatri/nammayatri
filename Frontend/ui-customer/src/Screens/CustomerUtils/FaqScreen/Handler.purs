{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FaqScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Screens.FaqScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.FaqScreen.View as FaqScreen
import Components.SettingSideBar.Controller as SettingSideBar
import ModifyScreenState (modifyScreenState, FlowState(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Common.Types.App (LazyCheck(..), CategoryListType(..))
import Screens.FaqScreen.Transformer
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
import Components.ChatView (makeChatComponent', makeChatComponent)
import Screens.FaqScreen.Transformer
import Engineering.Helpers.Commons (getCurrentUTC)
import Screens.FaqScreen.ScreenData
import Components.IssueView (IssueInfo)
import Debug
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData

faqScreen :: FlowBT String FlowState
faqScreen = do
  (GlobalState globalState) <- getState
  let faqScreenState = globalState.faqScreen
  if DA.null faqScreenState.data.categories then do 
    let language = fetchLanguage $ getLanguageLocale languageKey 
    (GetCategoriesRes response) <- Remote.getCategoriesBT language
    let categories' = map (\(Category catObj) ->{ categoryName : if (language == "en") then capitalize catObj.category else catObj.category , categoryId : catObj.issueCategoryId, categoryAction : Just catObj.label, categoryImageUrl : Just catObj.logoUrl, isRideRequired : catObj.isRideRequired , maxAllowedRideAge : catObj.maxAllowedRideAge, categoryType: catObj.categoryType, allowedRideStatuses: catObj.allowedRideStatuses}) response.categories
    modifyScreenState $ FaqScreenStateType (\faqScreen -> faqScreen { data {categories = categories' } } )
  else pure unit
  (GlobalState updatedGlobalState) <- getState
  act <- lift $ lift $ runScreen $ FaqScreen.screen updatedGlobalState.faqScreen
  
  case act of
    GoBack updatedState -> goBackHandler updatedState
    GoHome updatedState -> goHomeHandler updatedState
    GoToFavourites updatedState -> goToFavouritesHandler updatedState
    ChangeLanguage updatedState -> changeLanguageHandler updatedState
    GoToChatScreen categoryId optionId updatedState -> gotToChatScreenHandler categoryId optionId updatedState
    GoToSelectRideScreen categoryId optionId updatedState -> goToSelectRideScreen categoryId optionId updatedState


goBackHandler :: ST.FaqScreenState -> FlowBT String FlowState
goBackHandler updatedState =  do 
  modifyScreenState $ FaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure SelectFaqScreenFlow)


goHomeHandler :: ST.FaqScreenState -> FlowBT String FlowState
goHomeHandler updatedState = do 
  modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBar.CLOSED}}}) 
  modifyScreenState $ FaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure HomeScreenFlow)

goToFavouritesHandler :: ST.FaqScreenState -> FlowBT String FlowState
goToFavouritesHandler updatedState = do 
  modifyScreenState $ FaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure GoToFavouritesScreenFlow)

changeLanguageHandler :: ST.FaqScreenState -> FlowBT String FlowState
changeLanguageHandler updatedState = do 
  modifyScreenState $ FaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure ChangeLanguageScreenFlow)

gotToChatScreenHandler :: String -> Maybe String -> ST.FaqScreenState -> FlowBT String FlowState
gotToChatScreenHandler categoryId optionId updatedState = do 
  modifyScreenState $ FaqScreenStateType (\_ -> updatedState)
  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetOptionsRes getOptionsRes) <- case optionId of 
                                      Just optionId' -> Remote.getOptionsBT language categoryId optionId' "" ""
                                      Nothing -> Remote.getOptionsBT language categoryId "" "" ""
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
    showSubmitComp' = DA.any (\ (Message  message) -> (fromMaybe "" message.label) == "CREATE_TICKET") getOptionsRes.messages 
  modifyScreenState $ ReportIssueChatScreenStateType (
    \state ->  state {
      data {
          chats = chats'
        , entryPoint = ReportIssueChatScreenData.FaqEntry
        , selectedCategory = {
            categoryAction : Nothing
          , categoryName : updatedState.data.categoryName
          , categoryImageUrl : Nothing
          , categoryId : categoryId
          , isRideRequired : false
          , maxAllowedRideAge : updatedState.data.maxAllowedRideAge
          , categoryType : "Category"
          , allowedRideStatuses : Nothing
        }
        , options = options'
        , chatConfig = ReportIssueChatScreenData.initData.data.chatConfig{
            messages = messages'
          }
      }
    , props {
        showSubmitComp = showSubmitComp'
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure IssueReportChatScreenFlow)

goToSelectRideScreen :: String -> Maybe String -> ST.FaqScreenState -> FlowBT String FlowState
goToSelectRideScreen categoryId optionId updatedState = do 
  modifyScreenState $ FaqScreenStateType (\_ -> updatedState)
  modifyScreenState $ RideSelectionScreenStateType (
    \rideHistoryScreen -> rideHistoryScreen {
      data {
        offsetValue = 0,
        selectedOptionId = optionId,
        entryPoint = RideSelectionScreenData.FaqScreenEntry
      },
      selectedCategory = {
          categoryAction : Nothing
        , categoryName : updatedState.data.categoryName
        , categoryImageUrl : Nothing
        , categoryId : categoryId
        , isRideRequired : false
        , maxAllowedRideAge : updatedState.data.maxAllowedRideAge
        , categoryType : "Category"
        , allowedRideStatuses : Nothing
      }
    } 
  )
  App.BackT $ App.BackPoint <$> (pure RideSelectionScreenFlow) 





