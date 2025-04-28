{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsQuizScreen.View where

import Prelude
import Effect (Effect)
import PrestoDOM --(imageUrlWithFallback,  alignParentBottom, afterRender, shimmerFrameLayout, Orientation(..), PrestoDOM, Screen, Length(..), Padding(..), Margin(..), Gravity(..), Visibility(..), layoutGravity, weight, maxLines, ellipsize, frameLayout, stroke, clickable, onClick, imageWithFallback, cornerRadius, margin, imageView, visibility, color, gravity, relativeLayout, height, width, background, textView, text, padding, linearLayout, orientation, onBackPressed, scrollView, scrollBarY)
import Screens.Types (LmsQuizScreenState, QuestionStatus(..), LmsQuestion(..))
import Screens.Benefits.LmsQuizScreen.Controller (Action(..), eval, ScreenOutput)
import Debug (spy)
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Utils (getLanguageTwoLetters, fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (screenWidth, flowRunner, liftFlow, screenHeight)
import Components.GenericHeader as GenericHeader
import Screens.Benefits.LmsQuizScreen.ComponentConfig
import Animation as Anim
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Font.Style as FontStyle
import Data.Array (mapWithIndex, (!!), find, elem)
import Services.API (LmsTranslatedModuleInfoRes(..), LmsCategory(..), LmsEntityCompletionStatus(..), ModuleCompletionStatus(..), LmsModuleRes(..), LmsQuestionRes(..), QuizQuestion(..), Options(..), OptionEntity(..), SingleOption(..), QuizOptions(..))
import Font.Style (Style(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Services.Backend as Remote
import Data.Lens ((^.))
import Services.Accessor (_moduleId, _name)
import Data.Either (Either(..))
import Data.Array (length, null, (..))
import Services.API as API
import Components.PrimaryButton as PrimaryButton
import PrestoDOM.Animation as PrestoAnim
import Animation.Config (translateYAnimConfig)
import Data.Foldable (foldl)
import Resource.Localizable.StringsV2 (getString) as StringsV2
import Language.Types (STR(..))
import Mobility.Prelude (groupAdjacent, boolToVisibility)
import JBridge (toggleBtnLoader)
import Data.Int(fromNumber, toNumber, ceil)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Components.PopUpModal as PopUpModal
import Animation.Config as AnimConfig

screen :: LmsQuizScreenState -> Screen Action LmsQuizScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "LmsQuizScreen"
  , globalEvents: [(\push -> do
        _ <- pure $ toggleBtnLoader "" false
        if initialState.props.languageUpdated then do
            void $ launchAff $ flowRunner defaultGlobalState do
              case initialState.props.selectedTranslatedModule of
                Nothing -> pure unit
                Just moduleInfo -> do
                  questionResp <- Remote.getAllLmsQuestions (moduleInfo ^. _moduleId) (getLanguageTwoLetters $ Just initialState.props.selectedLanguage)
                  case questionResp of
                    Right resp -> liftFlow $ push $ UpdateQuestionAccordingToTranslation resp
                    Left err -> liftFlow $ push $ ErrorOccuredAction 
            pure $ pure unit
        else pure $ pure unit
   )]
  , eval: (\action state -> do
        let _ = spy "LmsQuizScreen ---- state" state
            _ = spy "LmsQuizScreen ---- action" action
        eval action state
    )
  }

view :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let areTotalQuestionsDone = length state.data.questions == state.props.currentQuestionIndex
  in
  Anim.screenAnimation $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , onBackPressed push $ const GoToPreviousScreen
  ]([ linearLayout
     [ width $ MATCH_PARENT
     , height $ MATCH_PARENT
     , orientation VERTICAL
     , background Color.white900
     ]([] <> if not areTotalQuestionsDone then 
                [
                   customHeaderView push state
                ,  separatorView push state
                ,  (if (state.props.isConfirming || state.props.isConfirmed ) then PrestoAnim.animationSet [] else  PrestoAnim.animationSet [ Anim.fadeIn state.props.animationVisibilty ]) $ 
                   linearLayout
                   [ width MATCH_PARENT
                   , orientation VERTICAL
                   , height MATCH_PARENT
                   , afterRender push $ const AfterRender
                   , background Color.white900
                   , visibility $ if (state.props.isConfirming || state.props.isConfirmed) then VISIBLE else boolToVisibility state.props.animationVisibilty
                   ][  quizBodyShimmerView push state
                    ,  quizQuestionBodyView push state
                    ,  primaryButtonBodyView push state
                   ]
                ]
          else  [quizCompletedView push state]
     )
  ] <> if state.props.exitPopupVisible then [PopUpModal.view (push <<< PopUpModalAction) (exitPopupConfig state)] else []
  
   )

customHeaderView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
customHeaderView push state =
  let moduleName = maybe "" (\moduleInfo -> moduleInfo ^. _name) state.props.selectedTranslatedModule
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 16
  , background Color.white900
  ][ linearLayout
     [ width $ MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     , gravity CENTER
     ][ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , width $ V 25
        , height $ V 25
        , onClick push $ const GoToPreviousScreen
        , margin $ Margin 0 8 8 8
        ]
      , textView $
        [ text $ moduleName <> " " <> (StringsV2.getString state.props.selectedLanguage QUIZ) 
        , weight 1.0
        , ellipsize true
        , width $ V 240
        , maxLines 1
        , color Color.black900
        ] <> (FontStyle.h3 LanguageStyle)
      , linearLayout
        [ width WRAP_CONTENT
        , height $ WRAP_CONTENT
        , orientation HORIZONTAL
        , onClick push $ const ChangeLanguage
        ][ imageView
           [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_language_logo"
            , width $ V 16
            , height $ V 16
            , margin $ Margin 0 0 3 0
           ]
          ,textView $
           [ text $ StringsV2.getString state.props.selectedLanguage (getKeyAccordingtoSelectedLanguage state.props.selectedLanguage)
           , color Color.blue800
           , ellipsize true
           , maxLines 1
           , width WRAP_CONTENT
           ] <> FontStyle.body1 LanguageStyle
        ]
     ]
  ,  quizStatusPillView push state (Margin 0 16 0 0) (Padding 0 0 0 0)
  ]
  where
   getKeyAccordingtoSelectedLanguage key = case key of
      "HI_IN" -> HINDI
      "KN_IN" -> KANNADA
      "TA_IN" -> TAMIL
      "TE_IN" -> TELUGU
      "FR_FR" -> FRENCH
      "ML_IN" -> MALAYALAM
      "BN_IN" -> BENGALI
      _       -> ENGLISH

quizStatusPillView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> Margin -> Padding -> PrestoDOM (Effect Unit) w
quizStatusPillView push state marginConfig paddingConfig=
  let areTotalQuestionsDone = state.props.currentQuestionIndex == length state.data.questions
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , padding paddingConfig
  , margin marginConfig
  , orientation HORIZONTAL
  ](mapWithIndex (\index item ->  linearLayout
                                  [ weight 1.0
                                  , height $ V 4
                                  , background $ getQuizPillColor item.questionStatusDuringQuiz areTotalQuestionsDone
                                  , margin $ MarginLeft if index == 0 then 0 else 5
                                  , cornerRadius 2.0
                                  ][]
  
  ) state.data.questions)
     
  where
    getQuizPillColor status areTotalQuestionsDone = case status of
       QUESTION_CORRECT-> if areTotalQuestionsDone then Color.green900 else Color.black800
       QUESTION_INCORRECT -> if areTotalQuestionsDone then Color.red900 else Color.black800
       QUESTION_NOT_ATTEMPTED -> Color.grey900
       QUESTION_ATTEMPTING -> Color.black800
       _ -> Color.white900

separatorView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 1
  , background $ Color.greySmoke
  ][]

quizQuestionBodyView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
quizQuestionBodyView push state =
  let mbQuestion = state.data.questions !! state.props.currentQuestionIndex
  in
  case mbQuestion of
    Nothing -> linearLayout [][]
    Just singleQuestion ->
      scrollView
      [ weight 1.0
      , width MATCH_PARENT
      , scrollBarY false
      , background Color.white900
      , visibility $ boolToVisibility ( not state.props.showShimmer )
      ][ linearLayout
        [ width $ MATCH_PARENT
        , height $ WRAP_CONTENT
        , padding $ Padding 16 24 16 16
        , orientation VERTICAL
        ][ quizQuestionView push state singleQuestion
        ,  quizQuestionOptionBodyView push state singleQuestion
        ]
      ]

quizQuestionView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> LmsQuestion -> PrestoDOM (Effect Unit) w
quizQuestionView push state eQuestion =
  let questionInfo = getQuestionInfo
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginBottom 16
  ][ textView $
     [ text questionInfo.questionText
     , color Color.black900
     ] <> FontStyle.h1 LanguageStyle
  ,  frameLayout
     [ width $ V questionInfo.width
     , height $ V questionInfo.height
     , visibility $ boolToVisibility questionInfo.questionImageVisible
     , margin $ MarginTop 16
     ][   linearLayout
          [ width $ MATCH_PARENT
          , height $ MATCH_PARENT
          , gravity CENTER
          , cornerRadius 12.0
          , stroke $ "1," <> Color.grey900
          , background Color.white900
          ][  PrestoAnim.animationSet [Anim.rotateAnim (AnimConfig.rotateAnimConfig state.props.animationVisibilty)] $ 
              imageView
              [ width $ V 20
              , height $ V 20
              , imageWithFallback $ if (not state.props.animationVisibilty) then fetchImage FF_ASSET "dummy" else fetchImage FF_ASSET "ny_ic_refresh_unfilled"
              ]
          ]
      ,   linearLayout 
          [ width $ MATCH_PARENT
          , height $ MATCH_PARENT
          , gravity CENTER
          ]
          [ imageView
            ([width $ V questionInfo.width
            , height $ V questionInfo.height
            , layoutGravity "center"
            , visibility $ boolToVisibility questionInfo.questionImageVisible
            ] <>  if (state.props.isConfirming || state.props.isConfirmed) then [imageUrlWithFallback $ ListImageUrl questionInfo.questionImage eQuestion.questionId] 
                  else if (not state.props.animationVisibilty) then [imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_background"]
                  else [imageUrlWithFallback $ ListImageUrl questionInfo.questionImage eQuestion.questionId] )
          ]
     ]
  ]
  where
    getQuestionInfo :: {questionText :: String, questionImage :: String, questionImageVisible :: Boolean, width :: Int, height :: Int}
    getQuestionInfo = case eQuestion.question of
                        TextQuestion text -> {questionText : text, questionImage : "", questionImageVisible : false, width : 0, height : 0}
                        ImageQuestion text imageUrl width height -> let imageDim = getScaledImageWidthAndHeight width height
                                                                    in {questionText : text, questionImage : imageUrl, questionImageVisible : true, width : imageDim.width, height : imageDim.height}

    getScaledImageWidthAndHeight :: Int -> Int -> {width :: Int, height :: Int}
    getScaledImageWidthAndHeight width height = 
      let sWidth = ((toNumber((screenWidth unit) - 32)) * 1.0) / (toNumber width)
          sHeight = (toNumber height) * sWidth
      in {width : (screenWidth unit )- 32, height : ceil sHeight}

quizQuestionOptionBodyView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> LmsQuestion -> PrestoDOM (Effect Unit) w
quizQuestionOptionBodyView push state eQuestion =
    if areOptionsInGrid then gridOptionsView push state eQuestion getOptions
    else
    linearLayout
    [ width $ MATCH_PARENT
    , height $ WRAP_CONTENT
    , orientation VERTICAL
    ]  (map (\(OptionEntity eOption) -> case eOption.option of
                                            TextOption optionText -> textOptionPill push state eQuestion optionText eOption.optionId getOptions false false
                                            SingleLineImage optionImage imageWidth imageHeight -> imageOptionPill push state eQuestion optionImage eOption.optionId imageHeight imageWidth getOptions false false
                                            _ -> linearLayout [][]
        ) getOptions)
  where
    getOptions = case eQuestion.options of
                    SingleSelect (Options allOptions) -> allOptions.options
                    MultiSelect (Options allOptions) -> allOptions.options

    areOptionsInGrid = maybe false (\_-> true) $ find (\(OptionEntity eOption) ->  case eOption.option of
                                                                          TwoColumnImage _ _ _ -> true
                                                                          TwoColumnOption _ -> true
                                                                          _ -> false
                                                      ) getOptions


gridOptionsView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> LmsQuestion -> Array OptionEntity -> PrestoDOM (Effect Unit) w
gridOptionsView push state eQuestion options =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ](map (\item -> gridOptionsRowView push state eQuestion item options) (groupAdjacent options))

gridOptionsRowView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> LmsQuestion -> Array OptionEntity ->  Array OptionEntity -> PrestoDOM (Effect Unit) w
gridOptionsRowView push state eQuestion singleRowOptions options =
  let firstOption = singleRowOptions !! 0
      secondOption = singleRowOptions !! 1
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  ]([] <> case firstOption of
             Nothing -> []
             Just (OptionEntity eOption) ->  case eOption.option of
                                                TwoColumnOption optionText -> [textOptionPill push state eQuestion optionText eOption.optionId options true true]
                                                TwoColumnImage optionImage imageHeight imageWidth -> [imageOptionPill push state eQuestion optionImage eOption.optionId imageHeight imageWidth options true true]
                                                TextOption optionText -> [textOptionPill push state eQuestion optionText eOption.optionId getOptions true true]
                                                SingleLineImage optionImage imageWidth imageHeight -> [imageOptionPill push state eQuestion optionImage eOption.optionId imageHeight imageWidth getOptions true true]
                                                _ -> []
        <> case secondOption of
              Nothing -> []
              Just (OptionEntity eOption) -> case eOption.option of
                                                TwoColumnOption optionText -> [linearLayout[weight 1.0][], textOptionPill push state eQuestion optionText eOption.optionId options true false]
                                                TwoColumnImage optionImage imageHeight imageWidth -> [linearLayout[weight 1.0][], imageOptionPill push state eQuestion optionImage eOption.optionId imageHeight imageWidth options true false]
                                                TextOption optionText -> [linearLayout[weight 1.0][], textOptionPill push state eQuestion optionText eOption.optionId getOptions true false]
                                                SingleLineImage optionImage imageWidth imageHeight -> [linearLayout[weight 1.0][], imageOptionPill push state eQuestion optionImage eOption.optionId imageHeight imageWidth getOptions true false]
                                                _ -> []

  )
  where
      getOptions = case eQuestion.options of
                    SingleSelect (Options allOptions) -> allOptions.options
                    MultiSelect (Options allOptions) -> allOptions.options


textOptionPill :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> LmsQuestion -> String -> String -> Array OptionEntity -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
textOptionPill push state eQuestion optionText optionId options isInGrid isFirst =
  let textOptionInfo = getOptionInfo state eQuestion optionId false options
      pillSize = ((screenWidth unit) - 48) / 2
  in
  linearLayout
  ([ height $ WRAP_CONTENT
  , margin $  MarginBottom 16
  , padding $ Padding 12 20 12 20
  , orientation HORIZONTAL
  , cornerRadius 8.0
  , background $ textOptionInfo.backgroundColor
  , stroke $ "1," <> textOptionInfo.strokeColor
  , clickable textOptionInfo.isClickable
  , onClick push $ const $ SelectOption eQuestion optionId
  ] <> [if isInGrid then width $ V pillSize  else width MATCH_PARENT]
    <> [if isInGrid then gravity CENTER else gravity CENTER_VERTICAL])[ textView $
     [ text optionText
     , color state.data.config.themeColors.quizOptionTextColor
     , height $ WRAP_CONTENT
     ] <> FontStyle.body1 LanguageStyle
       <> if isInGrid then [width WRAP_CONTENT, layoutGravity "center", gravity CENTER] else [weight 1.0]
  ,  imageView
     [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_tick"
     , width $ V 20
     , height $ V 20
     , margin $ MarginLeft 15
     , visibility $ boolToVisibility (textOptionInfo.imageVisible && (not isInGrid))
     ]
  ]

imageOptionPill :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> LmsQuestion -> String -> String -> Int -> Int-> Array OptionEntity -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
imageOptionPill push state eQuestion optionImage optionId imageHeight imageWidth options isInGrid isFirst =
  let imageOptionInfo = getOptionInfo state eQuestion optionId true options
      pillSize = ((screenWidth unit) - 48) / 2
      imageDims = getWidthAndHeightAccordingToCondition
      paddingVal = 7 - imageOptionInfo.strokeWidth
  in
  frameLayout
  [ width $ V (imageDims.width + 12)
  , height $ V (imageDims.height + 12)
  , margin $ if isFirst then Margin 0 0 8 16 else if isInGrid then Margin 8 0 0 16 else MarginBottom 16
  ][  linearLayout
      [ width $ MATCH_PARENT
      , height $ MATCH_PARENT
      , gravity CENTER
      , background Color.white900
      ][  PrestoAnim.animationSet [Anim.rotateAnim (AnimConfig.rotateAnimConfig state.props.animationVisibilty)] $ 
          imageView
          [ width $ V 20
          , height $ V 20
          , imageWithFallback $ if (not state.props.animationVisibilty) then fetchImage FF_ASSET "dummy" else fetchImage FF_ASSET "ny_ic_refresh_unfilled"
          ]
      ]
  ,   linearLayout
      [ width $ MATCH_PARENT
      , height $ MATCH_PARENT
      , gravity CENTER
      , cornerRadius 12.0
      , stroke $ (show imageOptionInfo.strokeWidth) <>  "," <> imageOptionInfo.strokeColor
      ][  imageView
          ([ height $ V imageDims.height
          , width $ V imageDims.width
          , clickable imageOptionInfo.isClickable
          , onClick push $ const $ SelectOption eQuestion optionId
          , padding $ Padding paddingVal paddingVal paddingVal paddingVal
          ] <> [if isInGrid then width $ V pillSize else width MATCH_PARENT]
            <> if (state.props.isConfirming || state.props.isConfirmed) then [imageUrlWithFallback $ ListImageUrl optionImage optionId, background $ imageOptionInfo.backgroundColor] 
               else if (not state.props.animationVisibilty) then [imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_background"]
               else [imageUrlWithFallback $ ListImageUrl optionImage optionId, background $ imageOptionInfo.backgroundColor])
      ]
  ]
  where
    getWidthAndHeightAccordingToCondition :: {width :: Int, height :: Int}
    getWidthAndHeightAccordingToCondition = 
      let width = if not isInGrid then ((toNumber((screenWidth unit) - 44)) * 1.0) else ((toNumber(((screenWidth unit) - 72)/2)) * 1.0)
          sWidth = width / (toNumber imageWidth)
          sHeight = (toNumber imageHeight) * sWidth
      in {width : ceil width, height : ceil sHeight}

primaryButtonBodyView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
primaryButtonBodyView push state =
  (if (state.props.isConfirming || state.props.isConfirmed ) then PrestoAnim.animationSet []
   else  PrestoAnim.animationSet [ Anim.fadeIn state.props.animationVisibilty, Anim.translateInYAnim translateYAnimConfig { duration = 500 , fromY = 50, toY = 0} ])
  $
  linearLayout
  [ width  MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , alignParentBottom "true,-1"
  , background Color.white900
  , visibility $ if state.props.isConfirming then VISIBLE
                 else if state.props.bottomButtonVisibility && state.props.animationVisibilty then if (length state.data.questions == state.props.currentQuestionIndex) then VISIBLE else if (optionNotSelected || state.props.showShimmer) then GONE else VISIBLE
                 else GONE
  ][ separatorView push state
  ,  buttonView push state
  ]
  where 
   optionNotSelected = 
     let previousHistoryPresentForCurrentQuestion = maybe false (\question -> getValidatedPreviousHistoryEnabled question) (state.data.questions !! state.props.currentQuestionIndex)
     in  (state.props.isRetryEnabled || not previousHistoryPresentForCurrentQuestion) && (state.props.currentQuestionSelectedOptionsData.selectedSingleOption == Nothing) && (null state.props.currentQuestionSelectedOptionsData.selectedMultipleOptions)
   
   getValidatedPreviousHistoryEnabled question = case question.previousHistory of
     Nothing -> false
     Just (API.LmsQuizHistory history) -> history.status == API.CORRECT

buttonView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
buttonView push state =
  let buttonConfig = getButtonVisibility
  in
  Keyed.linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation buttonConfig.orientation
  , afterRender push $ const AfterRender
  , margin $ Margin 16 16 16 16
  ][
    Tuple "CONFIRM_QUESTION" $ generateButtonLayout "CONFIRM_QUESTION" buttonConfig buttonConfig.confirmButtonVisilbe
  , Tuple "RETRY_QUESTION" $ generateButtonLayout "RETRY_QUESTION" buttonConfig buttonConfig.retryButtonVisible
  , Tuple "NEXT_QUESTION" $ generateButtonLayout "NEXT_QUESTION" buttonConfig buttonConfig.nextButtonVisible
  , Tuple "QUIZ_DONE" $ generateButtonLayout "QUIZ_DONE" buttonConfig buttonConfig.doneButtonVisible
  , Tuple "RETAKE_QUIZ" $ generateButtonLayout "RETAKE_QUIZ" buttonConfig buttonConfig.retakeQuizButtonVisible
  , Tuple "CLOSE_QUIZ" $ generateButtonLayout "CLOSE_QUIZ" buttonConfig buttonConfig.closeQuizButtonVisible
  ]
  where
    getButtonVisibility :: ButtonVisibility
    getButtonVisibility = 
      let totalCorrectQuestions = getTotalCorrectQuestions state
          totalQuestions = length state.data.questions
          isPassed = if totalCorrectQuestions >= (getPassingLimit state) then true else false
      in

      if state.props.isRetryEnabled || state.props.isConfirming then defaultButtonVisibility {confirmButtonVisilbe = state.props.bottomButtonVisibility || state.props.isConfirming}
      else if (state.props.currentQuestionIndex == totalQuestions) then
           if isPassed then defaultButtonVisibility {orientation = VERTICAL, doneButtonVisible = true}
           else defaultButtonVisibility {orientation = VERTICAL, retakeQuizButtonVisible = true, closeQuizButtonVisible = true}
      else case (state.data.questions !! state.props.currentQuestionIndex) of
            Nothing -> defaultButtonVisibility
            Just questionInfo -> case questionInfo.validationRes of
                                  Nothing -> case questionInfo.previousHistory of
                                                Nothing -> defaultButtonVisibility {confirmButtonVisilbe = true}
                                                Just (API.LmsQuizHistory history) -> case history.status of
                                                                                        API.CORRECT -> defaultButtonVisibility {nextButtonVisible = true}
                                                                                        API.INCORRECT -> defaultButtonVisibility {confirmButtonVisilbe = true}
                                  Just (API.QuestionConfirmRes validationResponse) -> case validationResponse.validation of
                                                                                        API.CORRECT_ANSWER -> defaultButtonVisibility {nextButtonVisible = true}
                                                                                        API.INCORRECT_ANSWER -> defaultButtonVisibility {retryButtonVisible = true, nextButtonVisible = true}

    generateButtonLayout :: forall w. String -> ButtonVisibility -> Boolean -> PrestoDOM (Effect Unit) w
    generateButtonLayout key buttonConfig buttonVisibilty = linearLayout[visibility $ boolToVisibility buttonVisibilty][PrimaryButton.view (push <<< QuizPrimaryButtonActionController key) (quizDoneButtonConfig key state buttonConfig.retryButtonVisible)]

    defaultButtonVisibility :: ButtonVisibility
    defaultButtonVisibility = { 
      orientation : HORIZONTAL,
      confirmButtonVisilbe : false,
      retryButtonVisible : false,
      nextButtonVisible : false,
      doneButtonVisible : false,
      retakeQuizButtonVisible : false,
      closeQuizButtonVisible : false
    }

quizBodyShimmerView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w 
quizBodyShimmerView push state = 
  shimmerFrameLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 16 16 16 16
  , background Color.white900
  , visibility $ boolToVisibility state.props.showShimmer
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     ]( map
        ( \_ ->
              linearLayout
              [ height $ V 50
              , width MATCH_PARENT
              , cornerRadius 12.0
              , margin $ MarginTop 16
              , orientation HORIZONTAL
              , padding $ Padding 16 16 16 16
              , background Color.greySmoke
              ][  linearLayout
                  [ width $ MATCH_PARENT
                  , height $ MATCH_PARENT
                  , background Color.black400
                  , cornerRadius 4.0
                  ][]
              ]
        )
        (1 .. 4)
      )
  ]

quizCompletedView :: forall w.(Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
quizCompletedView push state =
  let totalCorrectQuestions = getTotalCorrectQuestions state
      isPassed = if totalCorrectQuestions >= (getPassingLimit state) then true else false
  in
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , visibility $ boolToVisibility (not  state.props.showShimmer )
  , clickable false
  , background Color.white900
  ][  imageView
      [ width $ V (screenHeight unit)
      , height $ V (screenHeight unit)
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_sunburst"
      , layoutGravity "center"
      , visibility $ boolToVisibility isPassed
      ]
  ,   relativeLayout
      [ width $ MATCH_PARENT
      , height $ MATCH_PARENT
      , orientation VERTICAL
      , clickable false
      ][ quizCompletedScreenInformationView push state
      ,  primaryButtonBodyView push state
      ]
  ]

quizCompletedScreenInformationView :: forall w. (Action -> Effect Unit) -> LmsQuizScreenState -> PrestoDOM (Effect Unit) w
quizCompletedScreenInformationView push state =
  let totalCorrectQuestions = getTotalCorrectQuestions state
      totalQuestions = length state.data.questions
      isPassed = if totalCorrectQuestions >= (getPassingLimit state) then true else false
      moduleName = maybe "" (\moduleInfo -> moduleInfo ^. _name) state.props.selectedTranslatedModule
  in
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , clickable false
  ][ textView $
     [ text $ show totalCorrectQuestions <> "/" <> show totalQuestions <> " " <> StringsV2.getString state.props.selectedLanguage CORRECT
     , color Color.black800
     ] <> FontStyle.title2 LanguageStyle
  ,  quizStatusPillView push state (Margin sWidth 0 sWidth 0) (Padding 0 0 0 0)
  ,  textView $
     [ text $ if isPassed then (StringsV2.getString state.props.selectedLanguage (YOU_HAVE_SUCCESSFULLY_COMPLETED moduleName)) else getStringAccoringToPassingLimit totalQuestions moduleName
     , width $ WRAP_CONTENT
     , height $ WRAP_CONTENT
     , margin $ MarginHorizontal (sWidth/2) (sWidth/2)
     , gravity CENTER
     , color Color.black800
     ] <> FontStyle.body25 LanguageStyle
  ]
  where
    sWidth = (screenWidth unit) / 5

    getStringAccoringToPassingLimit totalQuestions moduleName  = let passingLimit = (getPassingLimit state) in
                                                                    if totalQuestions == passingLimit then (StringsV2.getString state.props.selectedLanguage (ALL_ANSWERS_SHOULD_BE_CORRECT_TO_COMPLETE moduleName ))
                                                                    else (show passingLimit) <> "/" <> (show totalQuestions) <> " " <> (StringsV2.getString state.props.selectedLanguage QUESTIONS_SHOULD_BE_CORRECT_TO_COMPLETE) <> " " <> moduleName

getTotalCorrectQuestions :: LmsQuizScreenState -> Int
getTotalCorrectQuestions state = foldl (\acc eQuestion -> acc + (if (isCorrectQuestion eQuestion)then 1 else 0)) 0 state.data.questions
  where
    isCorrectQuestion question = if question.questionStatusDuringQuiz == QUESTION_CORRECT then true else false

getPassingLimit :: LmsQuizScreenState -> Int
getPassingLimit state = 
  case state.props.selectedTranslatedModule of
    Nothing -> 100
    Just (LmsTranslatedModuleInfoRes moduleInfo) -> case moduleInfo.moduleCompletionCriteria of
      API.VIDEOS_AND_QUIZ numberOfQuestionsToPass -> numberOfQuestionsToPass
      _ -> 100

getOptionInfo :: LmsQuizScreenState -> LmsQuestion -> String -> Boolean -> Array OptionEntity -> {backgroundColor :: String, strokeWidth :: Int, strokeColor :: String, imageVisible :: Boolean, isClickable :: Boolean}
getOptionInfo state eQuestion optionId isImageOption options = if state.props.isRetryEnabled then getOptionInfoAccordingToCurrentSelectedOptions state optionId isImageOption eQuestion
    else getOptionsInfoAccordingToValidationRes state eQuestion optionId isImageOption options
      
getOptionsInfoAccordingToOptionsList :: String -> Boolean ->  Array OptionEntity -> LmsQuizScreenState -> {backgroundColor :: String, strokeWidth :: Int, strokeColor :: String, imageVisible :: Boolean, isClickable :: Boolean}
getOptionsInfoAccordingToOptionsList optionId isImageOption options state = case (find (\(OptionEntity option) -> option.optionId == optionId) options) of
                                                          Nothing -> {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : false}
                                                          Just (OptionEntity option) -> if option.isCorrect then {backgroundColor : Color.greenOpacity10, strokeWidth : if isImageOption then 3 else 1, strokeColor : Color.green900, imageVisible : true, isClickable : false}
                                                                         else {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : false}

getOptionsInfoAccordingToValidationRes :: LmsQuizScreenState -> LmsQuestion -> String -> Boolean -> Array OptionEntity -> {backgroundColor :: String, strokeWidth :: Int, strokeColor :: String, imageVisible :: Boolean, isClickable :: Boolean}
getOptionsInfoAccordingToValidationRes state eQuestion optionId isImageOption options =
  case eQuestion.validationRes of
    Nothing -> getOptionInfoAccordingToPreviousHistory state eQuestion optionId isImageOption options
    Just (API.QuestionConfirmRes validationRes) -> case validationRes.validationRes of
          API.SingleSelectedOptionValidation (API.ValidationResult res) -> if optionId == res.id && res.isCorrect then {backgroundColor : Color.greenOpacity10, strokeWidth : if isImageOption then 3 else 1, strokeColor : Color.green900, imageVisible : true, isClickable : false}
                                                                        else if optionId == res.id then {backgroundColor : Color.red900Alpha16, strokeWidth : if isImageOption then 3 else 1, strokeColor : Color.red900, imageVisible : false, isClickable : false}
                                                                        else {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : false}
          API.MultiSelectedOptionValidation resArr -> case (find (\(API.ValidationResult res) -> res.id == optionId) resArr) of 
                                                        Nothing -> {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : false}
                                                        Just (API.ValidationResult validatedOption) -> if validatedOption.isCorrect then {backgroundColor : Color.greenOpacity10, strokeWidth : if isImageOption then 3 else 1, strokeColor : Color.green900, imageVisible : true, isClickable : false}
                                                                                                       else {backgroundColor : Color.red900Alpha16, strokeWidth : if isImageOption then 3 else 1, strokeColor : Color.red900, imageVisible : false, isClickable : false}

getOptionInfoAccordingToPreviousHistory :: LmsQuizScreenState -> LmsQuestion -> String -> Boolean -> Array OptionEntity -> {backgroundColor :: String, strokeWidth :: Int, strokeColor :: String, imageVisible :: Boolean, isClickable :: Boolean}
getOptionInfoAccordingToPreviousHistory state eQuestion optionId isImageOption options = 
  case eQuestion.previousHistory of
    Nothing -> getOptionInfoAccordingToCurrentSelectedOptions  state optionId isImageOption eQuestion
    Just (API.LmsQuizHistory history) ->  case history.status of
                                            API.CORRECT -> 
                                                  if optionId `elem` history.selectedOptions then getOptionsInfoAccordingToOptionsList optionId isImageOption options state
                                                  else {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : false}
                                            API.INCORRECT -> {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : true}

getOptionInfoAccordingToCurrentSelectedOptions :: LmsQuizScreenState -> String -> Boolean -> LmsQuestion ->  {backgroundColor :: String, strokeWidth :: Int, strokeColor :: String, imageVisible :: Boolean, isClickable :: Boolean}
getOptionInfoAccordingToCurrentSelectedOptions state optionId isImageOption eQuestion = 
  case eQuestion.options of
    API.SingleSelect _ -> case state.props.currentQuestionSelectedOptionsData.selectedSingleOption of
                            Nothing -> {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : true}
                            Just selOption -> if optionId == selOption.optionId then {backgroundColor : state.data.config.themeColors.quizOptionSelectedBgColor, strokeWidth : if isImageOption then 3 else 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false, isClickable : true}
                                              else {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : true}
    API.MultiSelect _ -> if optionId `elem` (map (\option -> option.optionId) state.props.currentQuestionSelectedOptionsData.selectedMultipleOptions) 
                         then {backgroundColor : state.data.config.themeColors.quizOptionSelectedBgColor, strokeWidth : if isImageOption then 3 else 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false, isClickable : true}
                         else {backgroundColor : Color.white900, strokeWidth : 1, strokeColor : state.data.config.themeColors.quizOptionStrokeColor, imageVisible : false,  isClickable : true}

type ButtonVisibility =
  { orientation :: Orientation,
    confirmButtonVisilbe :: Boolean,
    retryButtonVisible :: Boolean,
    nextButtonVisible :: Boolean,
    doneButtonVisible :: Boolean,
    retakeQuizButtonVisible :: Boolean,
    closeQuizButtonVisible :: Boolean
  }