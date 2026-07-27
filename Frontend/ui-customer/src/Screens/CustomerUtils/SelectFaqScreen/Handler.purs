{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectFaqScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Screens.SelectFaqScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SelectFaqScreen.View as SelectFaqScreen
import Components.SettingSideBar.Controller as SettingSideBar
import ModifyScreenState (modifyScreenState, FlowState(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.SelectFaqScreen.Transformer
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
import Screens.SelectFaqScreen.Transformer
import Engineering.Helpers.Commons (getCurrentUTC)
import Screens.SelectFaqScreen.ScreenData
import Components.IssueView (IssueInfo)
import Debug (spy)
import Common.Types.App (FaqCardDropDownInfo)

selectFaqScreen :: FlowBT String FlowState
selectFaqScreen = do
  (GlobalState globalState) <- getState
  let selectFaqScreenState = globalState.selectFaqScreen
  if DA.null selectFaqScreenState.data.categories then do 
    let language = fetchLanguage $ getLanguageLocale languageKey 
    (GetCategoriesRes response) <- Remote.getCategoriesBT language
    filteredCategories <- pure $ DA.filter (\(Category catObj) -> catObj.categoryType == "FAQ") response.categories
    let categories' = map (\(Category catObj) ->{ categoryName : if (language == "en") then capitalize catObj.category else catObj.category , categoryId : catObj.issueCategoryId, categoryAction : Just catObj.label, categoryImageUrl : Just catObj.logoUrl, isRideRequired : catObj.isRideRequired , maxAllowedRideAge : catObj.maxAllowedRideAge, categoryType : catObj.categoryType, allowedRideStatuses: catObj.allowedRideStatuses}) filteredCategories
    modifyScreenState $ SelectFaqScreenStateType (\selectFaqScreen -> selectFaqScreen { data {categories = categories' } } )
  else pure unit
  (GlobalState updatedGlobalState) <- getState
  act <- lift $ lift $ runScreen $ SelectFaqScreen.screen updatedGlobalState.selectFaqScreen
  
  case act of
    GoBack updatedState -> goBackHandler updatedState
    GoHome updatedState -> goHomeHandler updatedState
    GoToChatScreen selectedCategory updatedState -> goToChatScreenHandler selectedCategory updatedState
    GoToFaqScreen selectedIssue updatedState -> goToFaqScreenHandler selectedIssue updatedState 

goBackHandler :: ST.SelectFaqScreenState -> FlowBT String FlowState
goBackHandler updatedState =  do 
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure HelpAndSupportScreenFlow)


goHomeHandler :: ST.SelectFaqScreenState -> FlowBT String FlowState
goHomeHandler updatedState = do 
  modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBar.CLOSED}}}) 
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure HomeScreenFlow)

goToFaqScreenHandler :: CategoryListType -> ST.SelectFaqScreenState -> FlowBT String FlowState 
goToFaqScreenHandler selectedCategory updatedState = do
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState)
  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language selectedCategory.categoryId "" "" ""
  let messages' = DA.mapWithIndex (\index (Message currMessage) -> {title : (fromMaybe "" currMessage.messageTitle), description : currMessage.message, isExpanded : false, id : currMessage.id, action : currMessage.messageAction, referenceCategoryId : currMessage.referenceCategoryId, referenceOptionId : currMessage.referenceOptionId, label : currMessage.label}) getOptionsRes.messages
  modifyScreenState $ FaqScreenStateType (
    \updatedState ->  updatedState {
      data {
        categoryName = fromMaybe "FAQs" selectedCategory.categoryAction,
        dropDownList = messages',
        maxAllowedRideAge = selectedCategory.maxAllowedRideAge
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure FaqScreenFlow)

goToChatScreenHandler :: CategoryListType ->  ST.SelectFaqScreenState -> FlowBT String FlowState 
goToChatScreenHandler selectedCategory updatedState =  do
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState) 
  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language selectedCategory.categoryId "" "" ""
  let 
    options' = DA.mapWithIndex (\index (Option optionObj) -> optionObj{ option = (show (index + 1)) <> ". " <> (optionObj.option)}) getOptionsRes.options
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
      , selectedCategory = selectedCategory
      , options = options'
      , chatConfig = ReportIssueChatScreenData.initData.data.chatConfig{
          messages = messages'
        }
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure $ IssueReportChatScreenFlow)
