{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.IssueList.View where

import Components.IssueView as IssueView
import Prelude (Unit, const, map, ($), (/), (==), (<>), (<<<), (<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, gravity, height, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, scrollView, onBackPressed, afterRender, imageWithFallback, rippleColor, cornerRadius)
import Effect (Effect)
import Styles.Colors as Color
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import Animation as Anim
import Language.Types (STR(..))
import JBridge as JB
import Common.Types.App
import Components.IssueList.Controller (Action(..), IssueListFlowState)
import Helpers.Utils (fetchImage, FetchImageFrom(..))

view :: forall w. (Action -> Effect Unit) -> IssueListFlowState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
        , orientation VERTICAL
        , afterRender push (const AfterRender)
        , background $ Color.white900
        ]
        [ headerLayout state push
        , issueDetailView push state
        ]

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
            , rippleColor Color.rippleShade
            , cornerRadius 20.0
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

------------------------------------------ issueDetailView ---------------
setStateIssueView :: IssueListFlowState -> IssueView.IssueInfo -> IssueView.IssueState
setStateIssueView state info = state.issueViewConfig { issue = info }

issueDetailView :: (Action -> Effect Unit) -> IssueListFlowState -> forall w. PrestoDOM (Effect Unit) w
issueDetailView push state =
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding $ if EHC.os == "IOS" then PaddingBottom 80 else PaddingBottom 0
        ]
        (map (\optionItem -> IssueView.view (push <<< IssueViewAction) (setStateIssueView state optionItem)) state.issues)
    ]
