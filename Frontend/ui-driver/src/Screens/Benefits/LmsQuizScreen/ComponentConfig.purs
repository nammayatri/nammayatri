{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsQuizScreen.ComponentConfig where

import Prelude

import Components.GenericHeader as GenericHeader
import Screens.Types as ST
import PrestoDOM (Length(..), Margin(..), Visibility(..), Gravity(..), Padding(..))
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Styles.Colors as Color
import Services.API (LmsModuleRes(..))
import Components.PrimaryButton as PrimaryButton
import JBridge as JB
import Engineering.Helpers.Commons (screenWidth)
import Resource.Localizable.StringsV2 (getString) as StringsV2
import Language.Types (STR(..))
import Components.PopUpModal as PopUpModal
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Debug

quizDoneButtonConfig :: String -> ST.LmsQuizScreenState -> Boolean -> PrimaryButton.Config
quizDoneButtonConfig label state isRetryButtonVisible =
  let sWidth = (screenWidth unit) - 48
  in
  PrimaryButton.config
  { textConfig {
      text = case label of
             "QUIZ_DONE" -> StringsV2.getString state.props.selectedLanguage DONE
             "RETAKE_QUIZ" -> StringsV2.getString state.props.selectedLanguage RETAKE_QUIZ
             "CLOSE_QUIZ" -> StringsV2.getString state.props.selectedLanguage CLOSE
             "NEXT_QUESTION" -> StringsV2.getString state.props.selectedLanguage NEXT
             "CONFIRM_QUESTION" -> StringsV2.getString state.props.selectedLanguage CONFIRM
             "RETRY_QUESTION" -> (StringsV2.getString state.props.selectedLanguage RETRY_STR) <> " "
             _ -> ""
    , color = case label of
               "RETRY_QUESTION" -> state.data.config.themeColors.quizButtonStrokeAndText
               "CLOSE_QUIZ" -> state.data.config.themeColors.quizButtonStrokeAndText
               _ -> state.data.config.primaryTextColor
    }
  , background = case label of
                  "RETRY_QUESTION" -> Color.white900
                  "CLOSE_QUIZ" -> Color.white900
                  _ -> state.data.config.primaryButtonBackground
  , enableRipple = false
  , rippleColor = Color.white40Alpha
  , margin = case label of
              "RETRY_QUESTION" -> Margin 0 0 8 0 
              "NEXT_QUESTION" -> if isRetryButtonVisible then Margin 8 0 0 0 else Margin 0 0 0 0
              _ -> Margin 0 0 0 0
  , stroke = case label of
              "RETRY_QUESTION" -> "1," <> state.data.config.themeColors.quizButtonStrokeAndText
              _ -> "0," <> state.data.config.primaryButtonBackground
  , id = "QuizPrimaryButton_" <> label
  , enableLoader = (JB.getBtnLoader $ "QuizPrimaryButton_" <> label)
  , width = if isRetryButtonVisible then (V (sWidth/2))  else MATCH_PARENT
  }

exitPopupConfig :: ST.LmsQuizScreenState -> PopUpModal.Config
exitPopupConfig state =
  PopUpModal.config {   
    gravity = CENTER,
    margin = (MarginHorizontal 16 16),
    padding = PaddingHorizontal 16 16,
    cornerRadius = (Corners 20.0 true true true true),
    optionButtonOrientation = "VERTICAL",
    primaryText {
      text = (StringsV2.getString state.props.selectedLanguage EXIT_THE_QUIZ)
    , margin = (Margin 0 20 0 20)
      },
    secondaryText {
      visibility = GONE
      },
    option1 {
      text = (StringsV2.getString state.props.selectedLanguage CANCEL)
    , enableRipple = true
    , color = state.data.config.primaryTextColor
    , background = state.data.config.primaryButtonBackground
    , strokeColor = state.data.config.primaryButtonBackground
    , width = MATCH_PARENT
    },
    option2 {
      text = (StringsV2.getString state.props.selectedLanguage EXIT_AND_START_AGAIN_LATER)
    , enableRipple = true
    , color = Color.black900
    , background = Color.white900
    , strokeColor = Color.white900
    , height = WRAP_CONTENT
    , width = MATCH_PARENT
    , margin = MarginTop 16
    }
}