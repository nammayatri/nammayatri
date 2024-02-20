{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Handler where

import Components.IndividualRideCard.View as IndividualRideCard
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons 
import ModifyScreenState (modifyScreenState, FlowState(..))
import Prelude
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.List as PrestoList
import Screens.RideSelectionScreen.ScreenData
import Screens.RideSelectionScreen.View as RideSelectionScreen
import Screens.Types (IndividualRideCardState, AnimationState(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Engineering.Helpers.Utils
import Locale.Utils
import Services.Backend as Remote
import Services.API 
import Data.Array
import Components.ChatView.Controller
import Screens.HelpAndSupportScreen.Transformer 
import Screens.RideSelectionScreen.Controller 
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.Types as ST

rideSelection :: FlowBT String FlowState
rideSelection = do
  (GlobalState globalState) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "RideSelectionScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.view (push <<< IndividualRideCardActionController) dummyIndividualCard
  logField_ <- lift $ lift $ getLogFields
  act <- lift $ lift $ runScreen $ RideSelectionScreen.screen globalState.rideSelectionScreen{shimmerLoader = AnimatedIn} listItemm

  case act of 
    GoBack state -> goBackHandler state
    LoaderOutput state -> loaderOutputHandler state
    SelectRide state -> selectRideHandler state
    RefreshScreen state -> refreshScreenHandler state
      



-- ##########################################################  Handlers ##########################################################


goBackHandler :: RideSelectionScreenState -> FlowBT String FlowState
goBackHandler state = do 
  modifyScreenState $ RideSelectionScreenStateType (\_ -> state )
  App.BackT $ App.NoBack <$> (pure HelpAndSupportScreenFlow) 


loaderOutputHandler :: RideSelectionScreenState -> FlowBT String FlowState
loaderOutputHandler state = do
  modifyScreenState $ RideSelectionScreenStateType (
    \_ -> state {
      data {
        offsetValue = state.data.offsetValue + 8
      }
    }
  )
  App.BackT $ App.NoBack <$> (pure RideSelectionScreenFlow)


selectRideHandler :: RideSelectionScreenState -> FlowBT String FlowState
selectRideHandler state = do
  modifyScreenState $ RideSelectionScreenStateType (\_ -> state )

  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language state.selectedCategory.categoryId "" ""

  let 
    getOptionsRes' = mapWithIndex (
      \index (Option optionObj) -> optionObj { 
        option = (show (index + 1)) <> ". " <> optionObj.option 
      }
    ) getOptionsRes.options

    messages' = mapWithIndex (
      \index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) "Bot" (getCurrentUTC "") "Text" (500*(index + 1))
    ) getOptionsRes.messages

    chats' = map (
      \(Message currMessage) -> Chat {
        chatId : currMessage.id, 
        chatType : "IssueMessage", 
        timestamp : getCurrentUTC ""
      } 
    ) getOptionsRes.messages

    tripId' = case state.selectedItem of
      Just item -> Just item.rideId
      _         -> Nothing

    merchantExoPhone' = case state.selectedItem of
      Just item -> Just item.merchantExoPhone
      _         -> Nothing
    categoryName = getTitle state.selectedCategory.categoryAction

  modifyScreenState $ ReportIssueChatScreenStateType (\_ -> 
    ReportIssueChatScreenData.initData { 
      data {
        entryPoint = ReportIssueChatScreenData.RideSelectionScreenEntry
      , chats = chats'
      , tripId = tripId'
      , merchantExoPhone = merchantExoPhone'
      , categoryName = categoryName
      , categoryId = state.selectedCategory.categoryId
      , options = getOptionsRes'
      , chatConfig { 
        messages = messages' 
      }
      ,  selectedRide = state.selectedItem 
      } 
    } 
  )
  App.BackT $ App.BackPoint <$> (pure IssueReportChatScreenFlow)


refreshScreenHandler :: RideSelectionScreenState -> FlowBT String FlowState
refreshScreenHandler state = do
  modifyScreenState $ RideSelectionScreenStateType (
    \_ -> state{
      data{
        offsetValue = 0
      }
    }
  )
  App.BackT $ App.NoBack <$> (pure  RideSelectionScreenFlow)