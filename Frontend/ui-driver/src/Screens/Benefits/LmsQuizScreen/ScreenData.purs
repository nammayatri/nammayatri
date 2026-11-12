{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsQuizScreen.ScreenData where

import Prelude
import Screens.Types (LmsQuizScreenState(..), QuestionStatus(..), LmsQuestion(..))
import Foreign.Object (empty)
import Services.API (LmsTranslatedModuleInfoRes(..), ModuleCompletionCriteria(..), LmsCategory(..), LmsEntityCompletionStatus(..), ModuleCompletionStatus(..), LmsModuleRes(..), LmsQuestionRes(..), QuizQuestion(..), Options(..), OptionEntity(..), SingleOption(..), QuizOptions(..))
import Data.Maybe (Maybe(..))
import Locale.Utils
import ConfigProvider

initData :: LmsQuizScreenState
initData = {
  data : {
    config : getAppConfig appConfig,
    logField : empty,
    questions : []
  },
  props : {
    selectedTranslatedModule : Nothing,
    currentQuestionSelectedOptionsData : { 
      selectedSingleOption : Nothing,
      selectedMultipleOptions : []
    },
    currentQuestionIndex : 0,
    isRetryEnabled : false,
    showShimmer : true,
    animationVisibilty : true,
    isConfirming : false,
    isConfirmed : false,
    selectedLanguage : getLanguageLocale languageKey,
    languageUpdated : false,
    exitPopupVisible : false,
    bottomButtonVisibility : true
  }
}