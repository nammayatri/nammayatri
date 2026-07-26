{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsQuizScreen.Handler where

import Prelude
import Types.App (FlowBT, LMS_QUIZ_SCREEN_OUTPUT(..), ScreenType(..), GlobalState(..))
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.Benefits.LmsQuizScreen.View as LmsQuizScreen
import Types.ModifyScreenState (modifyScreenState)
import Control.Transformers.Back.Trans as App
import Screens.Benefits.LmsQuizScreen.Controller (ScreenOutput(..))
import Screens.Benefits.LmsQuizScreen.ScreenData as LmsQuizScreenData

lmsQuizScreen :: FlowBT String LMS_QUIZ_SCREEN_OUTPUT
lmsQuizScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runLoggableScreen $ LmsQuizScreen.screen state.lmsQuizScreen
  case act of
    GoBack updatedState -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> updatedState)
      App.BackT $ pure App.GoBack
    GoToNextQuestion updatedState -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_NEXT_QUESTION updatedState)
    ConfirmQuestion updatedState -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CONFIRM_QUESTION updatedState)
    RetryQuestion updatedState -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RETRY_QUESTION updatedState)
    RetakeQuiz updatedState -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RETAKE_QUIZ_SO updatedState)
    GoToChangeLanguage updatedState ->do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SELECT_LANGUAGE_FOR_QUESTION updatedState)
    GoToLmsVideosScreenSO state -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> LmsQuizScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_LMS_VIDEOS_SCREEN_FROM_QUIZ state)
    GoToBenefitsScreenSO state -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> LmsQuizScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_BENEFITS_SCREEN_FROM_QUIZ state)