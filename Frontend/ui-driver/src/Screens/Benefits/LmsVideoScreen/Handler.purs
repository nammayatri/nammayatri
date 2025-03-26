{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsVideoScreen.Handler where

import Prelude
import Types.App (FlowBT, LMS_VIDEO_SCREEN_OUTPUT(..), ScreenType(..), GlobalState(..))
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.Benefits.LmsVideoScreen.View as LmsVideoScreen
import Types.ModifyScreenState (modifyScreenState)
import Control.Transformers.Back.Trans as App
import Screens.Benefits.LmsVideoScreen.Controller (ScreenOutput(..))
import Screens.Benefits.LmsVideoScreen.ScreenData as LmsVideoScreenData

lmsVideoScreen :: FlowBT String LMS_VIDEO_SCREEN_OUTPUT
lmsVideoScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ LmsVideoScreen.screen state.lmsVideoScreen
  case act of
    GoBack updatedState -> do
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> updatedState)
      App.BackT $ pure App.GoBack
    GoToBenefitsScreen updatedState -> do
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> LmsVideoScreenData.initData)
      App.BackT $ App.NoBack <$> (pure GO_TO_BENEFITS_SCREEN)
    GoToSelectLanguage updatedState -> do
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SELECT_LANGUAGE_FOR_VIDEOS updatedState)
    GoToTakeQuiz updatedState -> do
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_QUIZ_SCREEN updatedState)
    RefreshLmsVideoScreen updatedState -> do
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFRESH_LMS_VIDEO_SCREEN updatedState)