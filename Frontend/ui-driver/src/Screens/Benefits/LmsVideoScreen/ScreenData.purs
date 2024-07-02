{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsVideoScreen.ScreenData where

import Prelude
import Screens.Types (LmsVideoScreenState(..))
import Foreign.Object (empty)
import Services.API (LmsTranslatedModuleInfoRes(..), ModuleCompletionCriteria(..), LmsVideoRes(..), LmsModuleRes(..), LmsEntityCompletionStatus(..), LmsCategory(..), ModuleCompletionStatus(..))
import Data.Maybe (Maybe(..))
import Locale.Utils
import ConfigProvider

initData :: LmsVideoScreenState
initData = {
  data : {
    config : getAppConfig appConfig,
    logField : empty,
      ytVideo : {
        videoId : ""
      , enable : false
    },
    videosScreenData : {
      quizEnabled : true,
      completed : [],
      pending : [],
      quizStatus : ENTITY_INCOMPLETE,
      selectedTranslatedModule : Nothing
    }
  },
  props : {
    selectedModule : Nothing,
    showShimmer : true,
    showError : false,
    selectedLanguage : getLanguageLocale languageKey,
    isFetchingQuiz : false
  }
}
