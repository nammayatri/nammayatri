{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.OnboardingFaqsScreen.View where

import Animation as Anim
import Data.Maybe
import Debug
import Effect (Effect)
import Prelude
import PrestoDOM
import PrestoDOM.Types.Core
import Screens.Types as ST
import Screens.OnboardingFaqsScreen.Controller (Action(..), ScreenOutput, eval)
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons as EHC
import Styles.Colors as Color
import Font.Style as FontStyle
import Screens.OnboardingFaqsScreen.ScreenData
import Resource.Localizable.StringsV2
import Mobility.Prelude
import Resource.Localizable.TypesV2 as LT2
import Common.Types.App
import Helpers.Utils
import Language.Types
import Language.Strings (getString)
import Components.GenericHeader as GenericHeader
import Screens.OnboardingFaqsScreen.ComponentConfig
import Data.Array as DA
import Debug

screen :: ST.OnboardingFaqsScreenState -> Screen Action ST.OnboardingFaqsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "OnboardingFaqsScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let
          _ = spy "OnboardingFaqsScreen ----- state" state
        let
          _ = spy "OnboardingFaqsScreen --------action" action
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.OnboardingFaqsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    Anim.screenAnimation $
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.white900
    , onBackPressed push $ const BackPressed
    ]
    [ scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , horizontalLine true
        , if state.props.showAns then answerView push state else faqView push state
        ]
      ]
    ]


faqView :: forall w. (Action -> Effect Unit) -> ST.OnboardingFaqsScreenState -> PrestoDOM (Effect Unit) w
faqView push state = 
  let selectedCategoryIndex = fromMaybe (-1) state.props.selectedCategoryIndex
      questionAnsList = if (selectedCategoryIndex < 0) then [] else (maybe [] (\item -> item.questionAnsMap) (state.data.categoryToQuestionAnsMap DA.!! selectedCategoryIndex)) 
  in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , visibility $ boolToVisibility $ not state.props.showAns
    ] $ 
    [] <> if DA.null questionAnsList then [ sectionListView push state.data.categoryToQuestionAnsMap state ] else [ questionListView push questionAnsList state ]


sectionListView :: forall w. (Action -> Effect Unit) -> Array ST.CategoryToQuestionAnsMap -> ST.OnboardingFaqsScreenState -> PrestoDOM (Effect Unit) w
sectionListView push categoryToQuestionAnsMap state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background $ Color.white900
    , margin $ MarginTop 8
    ]
    (DA.mapWithIndex (\index item -> individualListView push item.category state index) categoryToQuestionAnsMap)


questionListView :: forall w. (Action -> Effect Unit) -> Array ST.QuestionAnsMap -> ST.OnboardingFaqsScreenState -> PrestoDOM (Effect Unit) w
questionListView push questionAnsMap state =
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background $ Color.white900
    , margin $ MarginTop 8
    ]
    (DA.mapWithIndex (\index item -> individualListView push item.question state index) questionAnsMap)

individualListView :: forall w. (Action -> Effect Unit) -> String -> ST.OnboardingFaqsScreenState -> Int -> PrestoDOM (Effect Unit) w
individualListView push listItem state index =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , onClick push $ const $ OpenListItem index listItem
    ]
    [ horizontalLine (index /= 0)
    , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER_VERTICAL
            ]
            [ textView
                ( [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , text listItem
                  , padding (PaddingVertical 20 20)
                  , color Color.black900
                  , margin $ MarginRight 16
                  ] <> FontStyle.paragraphText TypoGraphy
                )
            ]
        , linearLayout
            [ height MATCH_PARENT
            , width $ V 20
            , gravity CENTER
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_grey,"
                , height $ V 18
                , width $ V 18
                , margin $ MarginHorizontal 10 8
                ]
            ]
        ]
    ]


horizontalLine :: Boolean -> forall w. PrestoDOM (Effect Unit) w
horizontalLine isVisible =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    , gravity CENTER
    , visibility $ boolToVisibility isVisible
    ][]

answerView :: forall w. (Action -> Effect Unit) -> ST.OnboardingFaqsScreenState -> PrestoDOM (Effect Unit) w
answerView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    ]
    [ textView $
      [ text $ state.props.selectedQnA.question
      , color Color.black
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 16
      ] <> FontStyle.h3 TypoGraphy
    , textView $
      [ textFromHtml $ state.props.selectedQnA.answer
      , color Color.black
      , width MATCH_PARENT
      , height WRAP_CONTENT
      ] <> FontStyle.paragraphText TypoGraphy
    ]