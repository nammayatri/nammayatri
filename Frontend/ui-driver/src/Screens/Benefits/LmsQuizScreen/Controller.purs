{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsQuizScreen.Controller where

import Prelude
import Screens.Types as ST
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Components.GenericHeader as GenericHeader
import Services.API as API
import Data.Array (find, elem, filter, mapWithIndex, length)
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Screens.Benefits.LmsQuizScreen.Transformer
import JBridge (toast, toggleBtnLoader)
import Resource.Localizable.StringsV2 (getString) as StringsV2
import Language.Types (STR(..))
import Components.PopUpModal as PopUpModal

instance showAction :: Show Action where
  show (GenericHeaderActionController var1) = "GenericHeaderActionController_" <> show var1
  show (GoToPreviousScreen) = "GoToPreviousScreen"
  show (GoToNextQuestionAction) = "GoToNextQuestionAction"
  show (NoAction) = "NoAction"
  show (SelectOption var1 var2) = "SelectOption_" <> show var1 <> "_" <> show var2
  show (QuizPrimaryButtonActionController var1 var2) = "QuizPrimaryButtonActionController_" <> show var1 <> "_" <> show var2
  show (AfterRender) = "AfterRender"
  show (GoToLmsVideosScreen) = "GoToLmsVideosScreen"
  show (ChangeLanguage) = "ChangeLanguage"
  show (ErrorOccuredAction) = "ErrorOccuredAction"
  show (UpdateQuestionAccordingToTranslation var1) = "UpdateQuestionAccordingToTranslation_" <> show var1
  show (PopUpModalAction var1) = "PopUpModalAction_" <> show var1

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "no_action"

data Action = GenericHeaderActionController GenericHeader.Action
              | GoToPreviousScreen
              | GoToNextQuestionAction
              | NoAction
              | SelectOption ST.LmsQuestion String
              | QuizPrimaryButtonActionController String PrimaryButton.Action
              | AfterRender
              | GoToLmsVideosScreen
              | ChangeLanguage
              | ErrorOccuredAction
              | UpdateQuestionAccordingToTranslation  (API.LmsGetQuizRes)
              | PopUpModalAction PopUpModal.Action

data ScreenOutput = GoBack ST.LmsQuizScreenState
                  | GoToNextQuestion ST.LmsQuizScreenState
                  | ConfirmQuestion ST.LmsQuizScreenState
                  | RetryQuestion ST.LmsQuizScreenState
                  | RetakeQuiz ST.LmsQuizScreenState
                  | GoToChangeLanguage ST.LmsQuizScreenState
                  | GoToLmsVideosScreenSO ST.LmsQuizScreenState
                  | GoToBenefitsScreenSO ST.LmsQuizScreenState

eval :: Action -> ST.LmsQuizScreenState -> Eval Action ScreenOutput ST.LmsQuizScreenState

eval AfterRender state = continue state {props {showShimmer = false, animationVisibilty = true, bottomButtonVisibility = true}}

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state =  continue state { props { exitPopupVisible = true }} 

eval GoToPreviousScreen state = if state.props.currentQuestionIndex < (length state.data.questions)  then continue state { props { exitPopupVisible = true}}
                                else updateAndExit state {props {exitPopupVisible = false}} $ GoToLmsVideosScreenSO  state {props {exitPopupVisible = false}}

eval GoToNextQuestionAction state = updateAndExit state{ props {currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing, selectedMultipleOptions : []}}} $ GoToNextQuestion state{ props {currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing, selectedMultipleOptions : []}}}

eval (SelectOption question selOptionId) state =
  let selectedOptionData = {  optionId : selOptionId,
                              isCorrect : false,
                              validated : false
                            }
      newState = case question.options of
                    API.SingleSelect _ -> state {props{ currentQuestionSelectedOptionsData = {selectedSingleOption : Just selectedOptionData, selectedMultipleOptions : []}}}
                    API.MultiSelect _ -> 
                      let selectedMultipleOptions = map (\option -> option.optionId) state.props.currentQuestionSelectedOptionsData.selectedMultipleOptions
                      in
                      if (selOptionId `elem` selectedMultipleOptions)
                      then state {props { currentQuestionSelectedOptionsData = { selectedMultipleOptions : filter (\option -> option.optionId /= selOptionId) state.props.currentQuestionSelectedOptionsData.selectedMultipleOptions, selectedSingleOption : Nothing }}}
                      else state { props { currentQuestionSelectedOptionsData = { selectedMultipleOptions : state.props.currentQuestionSelectedOptionsData.selectedMultipleOptions <> [selectedOptionData], selectedSingleOption : Nothing }}}
  in continue newState

eval (QuizPrimaryButtonActionController label (PrimaryButton.OnClick)) state =
  case label of
    "CONFIRM_QUESTION" -> updateAndExit state{props{isConfirmed = false, isConfirming = true, animationVisibilty = false, bottomButtonVisibility = false}} $ ConfirmQuestion state{props{isConfirmed = false, isConfirming = true, isRetryEnabled = false, showShimmer = true}}
    "NEXT_QUESTION" -> updateAndExit state{props{isConfirmed = false, animationVisibilty = false, bottomButtonVisibility = false}} $ GoToNextQuestion state{props{isConfirmed = false, isRetryEnabled = false, showShimmer = false}}
    "RETRY_QUESTION" ->
       let updateQuestions = mapWithIndex (\index eQuestion -> if index == state.props.currentQuestionIndex 
                                                               then case eQuestion.validationRes of
                                                                      Nothing -> eQuestion
                                                                      Just (API.QuestionConfirmRes res) -> eQuestion {previousHistory = Just $ API.LmsQuizHistory { attemptNumber : -1,
                                                                                                                                                                    status : if res.validation == API.CORRECT_ANSWER then API.CORRECT else API.INCORRECT ,
                                                                                                                                                                    selectedOptions : buildSelectedOptions res.validationRes 
                                                                                                                                                                  },
                                                                                                                      validationRes = Nothing
                                                                                                                      }
                                                               else eQuestion) state.data.questions
       in
       updateAndExit state{data {questions = updateQuestions}, props {isConfirmed = false, isRetryEnabled = true, animationVisibilty = false, bottomButtonVisibility = false, currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing, selectedMultipleOptions : []}}} $ RetryQuestion  state{data {questions = updateQuestions}, props {isConfirmed = false, isRetryEnabled = true, showShimmer = true, currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing, selectedMultipleOptions : []}}}
    "QUIZ_DONE" -> exit $ GoToBenefitsScreenSO state
    "CLOSE_QUIZ" -> exit $ GoToBenefitsScreenSO state
    "RETAKE_QUIZ" -> updateAndExit state{props {isConfirmed = false, animationVisibilty = false, bottomButtonVisibility = false}} $ RetakeQuiz state{props {isConfirmed = false, showShimmer = true}}
    _ -> continue state

  where
    buildSelectedOptions validationRes = case validationRes of
      API.SingleSelectedOptionValidation (API.ValidationResult singleOption) -> [singleOption.id]
      API.MultiSelectedOptionValidation multiOptions -> map (\(API.ValidationResult res) -> res.id) multiOptions

eval ChangeLanguage state = updateAndExit state $ GoToChangeLanguage state

eval ErrorOccuredAction state = do
    _ <- pure $ toast $ StringsV2.getString state.props.selectedLanguage UNABLE_TO_CHANGE_LANGUAGE_PLEASE_TRY_AGAIN
    continue state{ props{languageUpdated = false, showShimmer = false}}

eval (UpdateQuestionAccordingToTranslation quizResp) state =
  let transformedQuestions = transformQuizRespToQuestions quizResp
      updatedQuestions = map (updateQuestion transformedQuestions) state.data.questions
      (API.LmsGetQuizRes extractedData) = quizResp
  in
  continue state {data {questions = updatedQuestions}, props{selectedTranslatedModule = Just extractedData.selectedModuleInfo, languageUpdated = false, showShimmer = false}}
  where 
  updateQuestion translatedQuestions question  =
      case (find (\translatedQuestion -> question.questionId == translatedQuestion.questionId) translatedQuestions) of
        Nothing -> question
        Just translation -> question {question = translation.question, language = translation.language, options = translation.options}

eval GoToLmsVideosScreen state = exit $ GoToLmsVideosScreenSO state

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{ exitPopupVisible = false}}

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = updateAndExit state {props {exitPopupVisible = false}} $ GoToLmsVideosScreenSO  state {props {exitPopupVisible = false}} 
eval _ state = update state