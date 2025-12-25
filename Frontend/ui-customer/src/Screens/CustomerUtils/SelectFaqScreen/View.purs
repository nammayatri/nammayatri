{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTIEHULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectFaqScreen.View where

import Common.Types.App
import Screens.CustomerUtils.SelectFaqScreen.ComponentConfig
import Animation as Anim
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (when, Unit, bind, const, discard, map, pure, unit, show, not, ($), (-), (/=), (<<<), (<=), (<>), (==), (||), (<), (<>), (&&))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), afterRender, alignParentRight, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, width, imageWithFallback, weight, layoutGravity, clickable, alignParentBottom, scrollView, adjustViewWithKeyboard, lineHeight, singleLine, alpha, accessibility, accessibilityHint)
import Screens.SelectFaqScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.SelectFaqScreen.Transformer 
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Mobility.Prelude (boolToVisibility)
import Locale.Utils
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.CommonView (emptyTextView)

screen :: ST.SelectFaqScreenState -> Screen Action ST.SelectFaqScreenState ScreenOutput
screen initialState =
  {
    initialState
  , view
  , name : "SelectFaqScreen"
  , globalEvents : []
  , eval : \state  action -> do
      let _ = spy  "SelectFaqScreen action " state
      let _ = spy  "SelectFaqScreen state " action
      eval state action
  }

view :: forall w . (Action -> Effect Unit) -> ST.SelectFaqScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  Anim.screenAnimation $
 relativeLayout
 [  height MATCH_PARENT
  , width MATCH_PARENT
 ]$[linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
    , onBackPressed push $ const BackPressed
    , afterRender push (const AfterRender)
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey900
        ][]
      , scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ][
              headingView state $ getString ALL_TOPICS,
              allTopicsView state push $ state.data.categories
            ]
          ]
      , apiFailureView state push  
    ]
  ]


allTopicsView :: ST.SelectFaqScreenState -> (Action -> Effect Unit) -> Array CategoryListType -> forall w . PrestoDOM (Effect Unit) w
allTopicsView state push topicList =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , visibility if state.props.apiFailure then GONE else VISIBLE
    , orientation VERTICAL
    ](DA.mapWithIndex (\index item ->
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding (Padding 20 0 20 0)
        , onClick push $ const $ OpenFaqScreen item
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , padding $ Padding 0 20 0 20
            ][  case item.categoryImageUrl of
                  Just imageUrl -> 
                    imageView
                    [ imageWithFallback imageUrl
                    , height $ V 17
                    , width $ V 17
                    ]
                  Nothing -> emptyTextView
              , textView $
                [ accessibilityHint $ item.categoryName <> " : Button"
                , accessibility ENABLE
                , text item.categoryName
                , color Color.darkCharcoal
                , margin $ MarginLeft 13
                ] <> FontStyle.paragraphText LanguageStyle
              , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                , gravity RIGHT
                , layoutGravity "center_vertical"
                ][  imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                    , height $ V 15
                    , width $ V 15  
                    ]
                  ]
              ]
            , linearLayout
              [ height $ V 1
              , width MATCH_PARENT
              , background Color.greyLight
              , visibility $ boolToVisibility $ not $ index == (DA.length (topicList)) - 1
              ][]
          ]) (topicList))

headingView :: ST.SelectFaqScreenState -> String -> forall w . PrestoDOM (Effect Unit) w
headingView state title =
  textView $
    [ text title
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (Padding 16 12 0 12)
    , background Color.catskillWhite
    , color Color.darkCharcoal
    ] <> FontStyle.body5 LanguageStyle

apiFailureView :: forall w. ST.SelectFaqScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit ) w
apiFailureView state push=
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , visibility if state.props.apiFailure then VISIBLE else GONE
  ][  ErrorModal.view (push <<< APIFailureActionController) (apiErrorModalConfig state)]
