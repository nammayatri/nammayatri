{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.IssueListFlow.View where

import Prelude (Unit, bind, const, map, pure, unit, ($), (/), (==), (<>), (<<<), (&&), (||), (<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, afterRender, id, visibility, imageWithFallback, clickable, relativeLayout, stroke)
import Effect (Effect)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Engineering.Helpers.Commons as EHC
import Animation as Anim
import Language.Strings (getString)
import Language.Types (STR(..))
import JBridge as JB
import Effect.Class (liftEffect)
import Common.Types.App
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Debug (spy)
import Data.String (length)
import Components.IssueListFlow.Controller (Action(..), IssueListFlowState, getTitle)
import Data.Show (show)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))

view :: forall w. (Action -> Effect Unit) -> IssueListFlowState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , onBackPressed push (const BackPressed)
              , afterRender push (const AfterRender)
              ]
              [ headerLayout state push
              , driverDetailsView push state
              ]
          ]
        )

headerLayout :: IssueListFlowState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , layoutGravity "center_vertical"
        , padding state.headerConfig.padding
        ]
        [ imageView
            [ width state.headerConfig.imageConfig.width
            , height state.headerConfig.imageConfig.height
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , padding state.headerConfig.imageConfig.padding
            , margin state.headerConfig.imageConfig.margin
            ]
        , textView
            $ [ width state.headerConfig.headTextConfig.width
              , height state.headerConfig.headTextConfig.height
              , text state.headerConfig.headTextConfig.text
              , textSize state.headerConfig.headTextConfig.fontSize
              , margin state.headerConfig.headTextConfig.margin
              , weight state.headerConfig.headTextConfig.weight
              , color state.headerConfig.headTextConfig.color
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        , margin (MarginBottom 20)
        ]
        []
    ]

------------------------------------------ driverDetailsView ---------------
driverDetailsView :: (Action -> Effect Unit) -> IssueListFlowState -> forall w. PrestoDOM (Effect Unit) w
driverDetailsView push state =
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin (MarginBottom 20)
        ]
        ( map
            ( \optionItem ->
                linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , gravity CENTER_VERTICAL
                  , visibility VISIBLE
                  , stroke $ "1," <> Color.grey900
                  , cornerRadius 10.0
                  , margin (Margin 15 0 15 20)
                  ]
                  [ linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , orientation VERTICAL
                      , gravity CENTER_VERTICAL
                      , padding (Padding 7 20 15 20)
                      , visibility VISIBLE
                      ]
                      [ linearLayout
                          [ width MATCH_PARENT
                          , height WRAP_CONTENT
                          , gravity CENTER_VERTICAL
                          ]
                          [ textView
                              [ height state.firstTextConfig.height
                              , width WRAP_CONTENT
                              , text (getTitle optionItem.category)
                              , visibility state.firstTextConfig.visibility
                              , color state.firstTextConfig.color
                              , fontStyle $ FontStyle.bold LanguageStyle
                              , textSize state.firstTextConfig.fontSize
                              , alpha state.firstTextConfig.alpha
                              ]
                          , linearLayout
                              [ height WRAP_CONTENT
                              , weight 1.0
                              ]
                              []
                          , textView
                              [ height state.secondTextConfig.height
                              , width WRAP_CONTENT
                              , text optionItem.createdAt
                              , visibility state.secondTextConfig.visibility
                              , color state.secondTextConfig.color
                              , fontStyle $ FontStyle.medium LanguageStyle
                              , textSize state.secondTextConfig.fontSize
                              , alpha state.secondTextConfig.alpha
                              ]
                          ]
                      , textView
                          [ height state.thirdTextConfig.height
                          , width state.thirdTextConfig.width
                          , text ((getTitle state.thirdTextConfig.text) <> optionItem.issueReportId)
                          , visibility state.thirdTextConfig.visibility
                          , color state.thirdTextConfig.color
                          , fontStyle $ FontStyle.medium LanguageStyle
                          , textSize state.thirdTextConfig.fontSize
                          , margin state.thirdTextConfig.margin
                          , alpha state.thirdTextConfig.alpha
                          ]
                      , linearLayout
                          [ width MATCH_PARENT
                          , height WRAP_CONTENT
                          , gravity CENTER_VERTICAL
                          , margin $ Margin 0 13 0 0
                          ]
                          [ textView
                              [ height state.fourthTextConfig.height
                              , width state.fourthTextConfig.width
                              , text state.fourthTextConfig.text
                              , color state.fourthTextConfig.color
                              , visibility state.fourthTextConfig.visibility
                              , color state.fourthTextConfig.color
                              , fontStyle $ FontStyle.medium LanguageStyle
                              , textSize state.fourthTextConfig.fontSize
                              , alpha state.fourthTextConfig.alpha
                              , onClick push (const (Remove optionItem.issueReportId))
                              ]
                          , textView
                              [ height WRAP_CONTENT
                              , width state.fifthTextConfig.width
                              , text state.fifthTextConfig.text
                              , color state.fifthTextConfig.color
                              , visibility state.fifthTextConfig.visibility
                              , margin $ state.fifthTextConfig.margin
                              , color state.fifthTextConfig.color
                              , fontStyle $ FontStyle.medium LanguageStyle
                              , textSize state.fifthTextConfig.fontSize
                              , alpha state.fifthTextConfig.alpha
                              , onClick push (const (CallSupportCenter))
                              ]
                          ]
                      ]
                  ]
            )
            state.issues
        )
    ]

--------------------------------- horizontalLineView and dummyTextView -------------------
horizontalLineView :: Int -> Int -> forall w. PrestoDOM (Effect Unit) w
horizontalLineView marginLeft marginRight =
  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , background Color.greyLight
    , margin (Margin marginLeft 0 marginRight 0)
    ]
    []

dummyTextView :: forall w. PrestoDOM (Effect Unit) w
dummyTextView =
  textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ]
