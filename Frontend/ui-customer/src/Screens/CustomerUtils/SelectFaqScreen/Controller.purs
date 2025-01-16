{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectFaqScreen.Controller where

import Data.Array (length) as DA
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Language.Types (STR(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppActionClick)
import Prelude (class Show, pure, bind, discard, show, unit, map, ($), (<>), (==), void, (&&), (>), (||),(/), not, (>=))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (SelectFaqScreenState)
import Services.API (RideBookingListRes(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.SelectFaqScreen.ScreenData (initData)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Foreign.Object (empty)
import ConfigProvider
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils
import Screens.SelectFaqScreen.Transformer

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
      _ -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_done"

data Action = BackPressed
            | GenericHeaderActionController GenericHeader.Action
            | APIFailureActionController ErrorModal.Action
            | AfterRender
            | OpenChat CategoryListType
            | OpenFaqScreen CategoryListType
            
data ScreenOutput = GoBack SelectFaqScreenState
                  | GoHome SelectFaqScreenState
                  | GoToChatScreen CategoryListType SelectFaqScreenState
                  | GoToFaqScreen CategoryListType SelectFaqScreenState

eval :: Action -> SelectFaqScreenState -> Eval Action ScreenOutput SelectFaqScreenState

eval BackPressed state = exit $ GoBack state

eval (OpenChat selectedCategory) state = exit $ GoToChatScreen selectedCategory state

eval (OpenFaqScreen selectedCategory) state = exit $ GoToFaqScreen selectedCategory state

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure $ BackPressed]

eval (APIFailureActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ GoBack state

eval _ state = update state

