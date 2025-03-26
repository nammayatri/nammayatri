{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsVideoScreen.View where

import Prelude
import Effect (Effect)
import PrestoDOM (shimmerFrameLayout, Orientation(..), PrestoDOM, Screen, Length(..), Padding(..), Margin(..), Gravity(..), Visibility(..), layoutGravity, weight, maxLines, ellipsize, frameLayout, stroke, clickable, onClick, imageWithFallback, cornerRadius, margin, imageView, visibility, color, gravity, relativeLayout, height, width, background, textView, text, padding, linearLayout, orientation, onBackPressed, scrollView)
import Screens.Types (LmsVideoScreenState)
import Screens.Benefits.LmsVideoScreen.Controller (Action(..), eval, ScreenOutput)
import Debug (spy)
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Utils (fetchImage, FetchImageFrom(..), getLanguageTwoLetters)
import Engineering.Helpers.Commons (screenWidth, flowRunner, liftFlow)
import Components.GenericHeader as GenericHeader
import Animation as Anim
import Data.Array (length, mapWithIndex, null, find)
import Services.API (LmsEntityCompletionStatus(..), LmsVideoRes(..))
import Services.Accessor(_moduleId)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Services.Backend as Remote
import Data.Lens ((^.))
import Data.Either(Either(..))
import Data.Array ((..))
import PrestoDOM.Properties(cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Locale.Utils
import Resource.Localizable.StringsV2 (getString) as StringsV2
import Language.Types (STR(..))
import Services.API (LmsTranslatedModuleInfoRes(..), LmsModuleRes(..))
import Data.Maybe (Maybe(..), maybe)
import PrestoDOM.Animation as PrestoAnim
import Mobility.Prelude (boolToVisibility)
import Services.Accessor (_name)

screen :: LmsVideoScreenState -> Screen Action LmsVideoScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "LmsVideoScreen"
  , globalEvents: [(\push -> do
        void $ launchAff $ flowRunner defaultGlobalState do
          case initialState.props.selectedModule of
            Nothing -> pure unit
            Just selModule -> do
              videoResp <- Remote.getAllLmsVideos (selModule ^. _moduleId) $ getLanguageTwoLetters $ Just initialState.props.selectedLanguage
              case videoResp of
                Right modules -> liftFlow $ push $ UpdateVideoList modules
                Left err -> liftFlow $ push $ ErrorOccuredAction 
        pure $ pure unit
   )]
  , eval: (\action state -> do
        let _ = spy "LmsVideoScreen ---- state" state
            _ = spy "LmsVideoScreen ---- action" action
        eval action state
    )
  }

view :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , onBackPressed push $ const BackPressed
  , clickable false
  ][ PrestoAnim.animationSet [Anim.fadeIn true] $ 
     linearLayout
     [ width $ MATCH_PARENT
     , height $ MATCH_PARENT
     , orientation VERTICAL
     , background Color.white900
     ]([ customHeaderView push state
     ,  separatorView push state
     ] <> if state.props.showError then [errorView push state]
          else [videosShimmerView push state,  videosView push state])
  ]


customHeaderView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> PrestoDOM (Effect Unit) w
customHeaderView push state =
  let moduleName = maybe "" (\moduleInfo -> moduleInfo ^. _name) state.data.videosScreenData.selectedTranslatedModule
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 16
  ][ linearLayout
     [ width $ MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     , gravity CENTER
     ][ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , width $ V 25
        , height $ V 25
        , onClick push $ const BackPressed
        , margin $ Margin 0 8 8 8
        ]
      , textView $
        [ text $ moduleName
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
        , onClick push $ const SelectLanguage
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

separatorView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 1
  , background Color.greySmoke
  ][]

videosShimmerView :: forall w.(Action -> Effect Unit) -> LmsVideoScreenState -> PrestoDOM (Effect Unit) w
videosShimmerView push state = 
  shimmerFrameLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 16 16 16 16
  , visibility $ boolToVisibility state.props.showShimmer
  ][ linearLayout
     [ width $ MATCH_PARENT
     , height $ V 50
     , background Color.greyDark
     , cornerRadius 12.0
     , padding $ Padding 16 16 16 16
     ][ linearLayout
        [ width $ MATCH_PARENT
        , height $ MATCH_PARENT
        , background Color.black400
        , cornerRadius 4.0
        ][]
     ]
  ,  linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , margin $ MarginTop 50
     ]( map
        ( \_ ->
              linearLayout
              [ height $ V 92
              , width MATCH_PARENT
              , cornerRadius 12.0
              , margin $ MarginTop 16
              , orientation HORIZONTAL
              ][ linearLayout
                 [ width $ V 99
                 , height $ V 92
                 , background Color.black500
                 , cornerRadii $ Corners 12.0 true false false true
                 ][]
               , linearLayout
                 [ weight 1.0
                 , height $ V 92
                 , background Color.greyDark
                 , orientation VERTICAL
                 , gravity CENTER_VERTICAL
                 , cornerRadii $ Corners 12.0 false true true false
                 , padding $ Padding 16 16 16 16
                 ][ linearLayout
                    [ width $ MATCH_PARENT
                    , height $ V 20
                    , background Color.black400
                    , cornerRadius 4.0
                    ][]
                  , linearLayout
                    [ width $ MATCH_PARENT
                    , height $ V 20
                    , background Color.black400
                    , cornerRadius 4.0
                    , margin $ MarginTop 10
                    ][]
                 ]
              ]
        )
        (1 .. 4)
      )
  ]


videosView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> PrestoDOM (Effect Unit) w
videosView push state =
  let quizStatus =  state.data.videosScreenData.quizStatus
      quizEnabled = state.data.videosScreenData.quizEnabled
  in
  scrollView
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ boolToVisibility (not state.props.showShimmer)
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  headerInformationView push state getStatusForHeaderInfo
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ](mapWithIndex (\index eVideo -> videoCardView push state index eVideo "PENDING") state.data.videosScreenData.pending)
        , if quizEnabled && (quizStatus == ENTITY_INCOMPLETE || quizStatus == ENTITY_NOT_STARTED) then quizCardView push state (length state.data.videosScreenData.pending /= 0) else linearLayout [][]
        , if length state.data.videosScreenData.pending > 0 && length  state.data.videosScreenData.completed > 0 then headerInformationView push state "VIDEO_WATCHED_TILE" else linearLayout [][]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ](mapWithIndex (\index eVideo -> videoCardView push state index eVideo "COMPLETED") state.data.videosScreenData.completed)
        , if quizEnabled && quizStatus == ENTITY_COMPLETED then quizCardView push state false else linearLayout [][]
      ]
  ]
  where
    getStatusForHeaderInfo = 
      if length state.data.videosScreenData.pending > 0 then "VIDEOS_PENDING"
      else if  state.data.videosScreenData.quizEnabled && state.data.videosScreenData.quizStatus == ENTITY_INCOMPLETE then "QUIZ_INCOMPLETE"
      else "TRAINING_COMPLETED"

videoCardView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> Int -> LmsVideoRes -> String -> PrestoDOM (Effect Unit) w
videoCardView push state index (LmsVideoRes video) videoStatus =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 92
  , orientation HORIZONTAL
  , stroke $ "1," <> Color.grey900
  , cornerRadius 8.0
  , clickable true
  , onClick push $ const $ OpenReelsView index videoStatus
  , margin $ if index == 0 then Margin 24 24 24 24 else Margin 24 0 24 24
  ][ videoThumbNailView push state (LmsVideoRes video)
  ,  videoInformation push state index (LmsVideoRes video)
  ]

videoThumbNailView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> LmsVideoRes -> PrestoDOM (Effect Unit) w
videoThumbNailView push state (LmsVideoRes video)=
  relativeLayout
  [ width $ V 99
  , height $ V 92
  , gravity CENTER
  ][ imageView
     [ width MATCH_PARENT
     , height MATCH_PARENT
     , imageWithFallback $ "," <> video.thumbnailImage
     ]
  ,  linearLayout
     [ width MATCH_PARENT
     , height MATCH_PARENT
     , gravity CENTER
     ][ imageView
        [ width $ V 28
        , height $ V 28
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_white_play_button_with_grey"
        ]
     ]
  ]

videoInformation :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> Int -> LmsVideoRes -> PrestoDOM (Effect Unit) w
videoInformation push state index (LmsVideoRes video) =
  linearLayout
  [ weight 1.0
  , height $ V 92
  , orientation VERTICAL
  , gravity CENTER_VERTICAL
  , padding $ Padding 12 3 12 3
  ]([ textView $
      [ text video.title
      , maxLines 2
      , ellipsize true
      , color Color.black800
      ] <> FontStyle.body6 LanguageStyle
  ] <> if video.videoCompletionStatus == ENTITY_COMPLETED then [statusPillView push state "COMPLETED" (Margin 0 4 0 0)]
       else if index == 0 then [videoCardPillView push state (StringsV2.getString state.props.selectedLanguage WATCH_NOW)]
       else [])


videoCardPillView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> String -> PrestoDOM (Effect Unit) w
videoCardPillView push state pillText =
  linearLayout
  [ width $ WRAP_CONTENT
  , height $ WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginTop 4
  ][ textView $
     [ text pillText
     , color Color.blue800
     ] <> FontStyle.tags LanguageStyle
  ,  imageView
     [ width $ V 12
     , height $ V 7
     , layoutGravity "center_vertical"
     , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_arrow"
     , margin $ Margin 0 3 0 0
     ]
  ]

quizCardView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> Boolean -> PrestoDOM (Effect Unit) w
quizCardView push state isLocked =
  let videosPending = (length state.data.videosScreenData.pending > 0)
      dataAccToStatus = getDataAccToQuizStatus videosPending
  in
  frameLayout
  [ width MATCH_PARENT
  , height $ V 92
  , margin $ dataAccToStatus.margin
  , padding $ PaddingHorizontal 24 24
  ][  linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation HORIZONTAL
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      , clickable (not isLocked)
      , onClick push $ const TakeQuiz
      ][  imageView
          [ width $ V 99
          , height $ V 92
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_quiz"
          ]
        , linearLayout
          [ weight 1.0
          , height MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER_VERTICAL
          , padding $ Padding 12 12 12 12
          ]([ textView $
              [ text $ StringsV2.getString state.props.selectedLanguage TAKE_A_QUIZ
              , color $ Color.black800
              , maxLines 1
              , ellipsize true
              ] <> FontStyle.body6 LanguageStyle
          ] <> (if isLocked then [statusPillView push state "QUIZ_LOCKED" (Margin 0 4 0 0)]
               else if (state.data.videosScreenData.quizStatus == ENTITY_INCOMPLETE) then [statusPillView push state "INCOMPLETE" (Margin 0 4 0 0)]
               else [statusPillView push state "COMPLETED" (Margin 0 4 0 0)])
            <> if isLocked then [] 
               else [videoCardPillView push state dataAccToStatus.buttonText])
      ]
    , linearLayout -- to disable the card
      [ width $ V ((screenWidth unit) - 32)
      , height $ V 92
      , background $ Color.white40Alpha
      , cornerRadius 8.0
      , clickable false
      , visibility $ boolToVisibility isLocked
      ][]
    , shimmerFrameLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , visibility $ boolToVisibility state.props.isFetchingQuiz
      ][ linearLayout
         [ width $ V ((screenWidth unit) - 32)
         , height $ V 92
         , background $ Color.greySmoke
         ][]
      ]
  ]
  where
    getDataAccToQuizStatus :: Boolean -> {buttonText :: String, margin :: Margin}
    getDataAccToQuizStatus videosPending = case videosPending, state.data.videosScreenData.quizStatus of
      _, ENTITY_COMPLETED -> {buttonText : StringsV2.getString state.props.selectedLanguage PLAY_AGAIN, margin : Margin 0 0 0 24}
      _, ENTITY_NOT_STARTED -> {buttonText : StringsV2.getString state.props.selectedLanguage PLAY_NOW, margin : Margin 0 24 0 0}
      false, ENTITY_INCOMPLETE -> {buttonText : StringsV2.getString state.props.selectedLanguage PLAY_NOW, margin : Margin 0 24 0 0}
      true, ENTITY_INCOMPLETE -> {buttonText : StringsV2.getString state.props.selectedLanguage PLAY_NOW, margin : Margin 0 0 0 24}

statusPillView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> String -> Margin -> PrestoDOM (Effect Unit) w
statusPillView push state status pillMargin =
  let pillProperty = getPropertyAccordingToStatus
  in
  linearLayout
  [ width $ WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , padding $ Padding 4 2 4 2
  , gravity CENTER
  , background pillProperty.pillBackgroundColor
  , cornerRadius pillProperty.cornerRadius
  , margin $ pillMargin
  ][ imageView
     [ width $ V 10
     , height $ V 10
     , visibility $ boolToVisibility pillProperty.shouldImageBeVisible
     , imageWithFallback $ fetchImage FF_ASSET pillProperty.pillImage
     , margin $ MarginRight 3]
  ,  textView $
     [ text $ pillProperty.text
     , color $ pillProperty.textColor
     ] <> (if status == "QUIZ_LOCKED" then (FontStyle.body3 LanguageStyle) else (FontStyle.body19 LanguageStyle))
  ]
  where
    getPropertyAccordingToStatus :: {text :: String, textColor :: String, cornerRadius :: Number, shouldImageBeVisible :: Boolean, pillBackgroundColor :: String, pillImage :: String}
    getPropertyAccordingToStatus = case status of
      "COMPLETED" -> mkProperty COMPLETED_STR Color.white900 16.0 true Color.green900 "ny_ic_white_tick"
      "PENDING" -> mkProperty PENDING_STR_C Color.white900 16.0 false Color.orange900 ""
      "NEW" -> mkProperty NEW_C Color.white900 16.0 false Color.blue900 ""
      "INCOMPLETE" -> mkProperty INCOMPLETE Color.black900 16.0 true Color.yellow800 "ny_ic_warning_unfilled_grey"
      "QUIZ_LOCKED" -> mkProperty WATCH_ALL_VIDEOS_TO_UNLOCK_QUIZ Color.black900 4.0 false Color.yellow800 ""
      _ -> mkProperty INCOMPLETE Color.white900 16.0 false Color.white900 ""

    mkProperty :: STR -> String -> Number -> Boolean -> String -> String -> {text :: String, textColor :: String, cornerRadius :: Number, shouldImageBeVisible :: Boolean, pillBackgroundColor :: String, pillImage :: String}
    mkProperty str textColor cornerRadius shouldImageBeVisible pillBackgroundColor pillImage =
      { text : StringsV2.getString state.props.selectedLanguage str,
        textColor : textColor,
        cornerRadius : cornerRadius,
        shouldImageBeVisible : shouldImageBeVisible,
        pillBackgroundColor : pillBackgroundColor,
        pillImage : pillImage
      }

headerInformationView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> String -> PrestoDOM (Effect Unit) w
headerInformationView push state infoType =
  let hInfoViewProperty = gethInfoViewProperty
  in
  textView $
  [ text $ hInfoViewProperty.text
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , background $ hInfoViewProperty.backgroundColor
  , padding $ Padding 16 12 16 12
  , color hInfoViewProperty.textColor
  ] <> FontStyle.body20 LanguageStyle
  where
    gethInfoViewProperty = case infoType of
      "VIDEOS_PENDING" -> {text: StringsV2.getString state.props.selectedLanguage WATCH_ALL_VIDEOS_TO_LEARN, textColor: Color.black700, backgroundColor: Color.blue600 }
      "QUIZ_INCOMPLETE" -> {text: StringsV2.getString state.props.selectedLanguage PLAY_QUIZ_TO_COMPLETE_YOUR_TRAINING, textColor: Color.black700, backgroundColor: Color.blue600 }
      "TRAINING_COMPLETED" -> {text: StringsV2.getString state.props.selectedLanguage TRAINING_COMPLETED <> " 🎉", textColor: Color.white900, backgroundColor: Color.green900 }
      "VIDEO_WATCHED_TILE" -> {text: StringsV2.getString state.props.selectedLanguage WATCHED, textColor: Color.black700, backgroundColor: Color.blue600 }
      _ -> {text: "", textColor: Color.white900, backgroundColor: Color.transparent}

errorView :: forall w. (Action -> Effect Unit) -> LmsVideoScreenState -> PrestoDOM (Effect Unit) w
errorView push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  ][ imageView
     [ width $ V 159
     , height $ V 117
     , imageWithFallback $ fetchImage FF_ASSET "ny_failed"
     ]
  ,  textView $
     [ text $ StringsV2.getString state.props.selectedLanguage UH_OH_SOMETHING_WENT_WRONG -- "Uh oh! Something went wrong"
     , color Color.black900
     , margin $ MarginTop 33
     ] <> FontStyle.h2 TypoGraphy 
  ,  textView $
     [ text $ StringsV2.getString state.props.selectedLanguage PLEASE_TRY_AGAIN
     , color Color.black700
     , margin $ MarginTop 12
     ] <> FontStyle.paragraphText TypoGraphy
  ]