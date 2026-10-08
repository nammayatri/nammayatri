{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.Benefits.LmsQuizScreen.Transformer where

import Prelude
import Services.API as API
import Screens.Types as ST
import Data.Maybe (Maybe(..))
import Data.Array as DA

transformQuizRespToQuestions :: API.LmsGetQuizRes -> Array ST.LmsQuestion
transformQuizRespToQuestions (API.LmsGetQuizRes resp) = DA.mapWithIndex (\index (API.LmsQuestionRes item) -> {
    questionId : item.questionId
  , moduleId : item.moduleId
  , language : item.language
  , question : item.question
  , options : item.options
  , previousHistory : item.previousHistory
  , questionStatusDuringQuiz : if index == 0 then ST.QUESTION_ATTEMPTING else ST.QUESTION_NOT_ATTEMPTED
  , validationRes : Nothing
  }) resp.questions