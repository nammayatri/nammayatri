{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.WriteToUsScreen.View where

import Prelude (Unit, const, map, not, ($), (<<<), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), ScopedScreen, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, onBackPressed, visibility, afterRender, imageWithFallback)
import Effect (Effect)
import Screens.WriteToUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.WriteToUsScreen.ScreenData (viewsItemList)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Animation as Anim
import Language.Strings (getString)
import Language.Types(STR(..))
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import PrestoDOM.Animation as PrestoAnim
import Common.Types.App
import Screens.WriteToUsScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))


screen :: ST.WriteToUsScreenState -> ScopedScreen Action ST.WriteToUsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "WriteToUsScreen"
  , globalEvents : []
  , eval
  , parent: Nothing
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.WriteToUsScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ][ frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , weight 1.0
        ][ PrestoAnim.animationSet 
            [ Anim.fadeOut state.props.isThankYouScreen
            , Anim.fadeIn  (not state.props.isThankYouScreen)
            ] $ writeToUsScreen state push 
            , PrestoAnim.animationSet 
            [ Anim.fadeIn state.props.isThankYouScreen
            , Anim.fadeOut  (not state.props.isThankYouScreen)
            ]  $ thankYouForWritingUsScreen state
        ]
     , PrimaryButton.view (push <<< PrimaryButtonActionController state) (primaryButtonConfig state)
    ]


-------------------------------------------------- writeToUsScreen ----------------------
writeToUsScreen :: ST.WriteToUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
writeToUsScreen state push = 
 linearLayout
 [ width MATCH_PARENT
 , height MATCH_PARENT
 , orientation VERTICAL
 , visibility  if state.props.isThankYouScreen then GONE else VISIBLE 
 , alpha if state.props.isThankYouScreen then 0.0 else 1.0
 ][ headerLayout state push
  , noteView state push
  , editTextFields state push
 ]

headerLayout :: ST.WriteToUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push= 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding (Padding 5 0 5 0)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
        , gravity CENTER_VERTICAL
        , onClick push (const BackPressed)
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        ]
      , textView $
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text (getString WRITE_TO_US)
        , margin (MarginLeft 20)
        , color Color.black
        , gravity CENTER_VERTICAL
        , alpha 0.8
        ] <> FontStyle.h3 TypoGraphy
    ]
 ]


noteView :: ST.WriteToUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
noteView state push = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , padding (Padding 15 10 10 10)
 , background Color.grey800
 , orientation HORIZONTAL
 ][ textView $
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , text (getString NOTE)
    , gravity CENTER_VERTICAL
    , color Color.black800
    ] <> FontStyle.body9 TypoGraphy
  , textView (
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , text (getString VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS)
    , gravity CENTER_VERTICAL
    , margin (MarginLeft 2)
    ] <> FontStyle.body3 TypoGraphy
    )
 ]


editTextFields :: ST.WriteToUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
editTextFields state push =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , margin (MarginTop 25)
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (PaddingBottom 5)
    ] (map(\optionItem ->
            PrimaryEditText.view (push <<< PrimaryEditTextActionController) (primaryEditTextConfig optionItem)
          ) viewsItemList
    )
 ]


------------------------------------------- thankYouForWritingUsScreen ---------------------------

thankYouForWritingUsScreen :: ST.WriteToUsScreenState -> forall w . PrestoDOM (Effect Unit) w
thankYouForWritingUsScreen state = 
 linearLayout
 [ height WRAP_CONTENT
 , width MATCH_PARENT
 , orientation VERTICAL
 , layoutGravity "center"
 , visibility  if state.props.isThankYouScreen then VISIBLE else GONE 
 , alpha if state.props.isThankYouScreen then 1.0 else 0.0
 ][ scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        ][ imageView
            [ width ( V 150)
            , height ( V 200)
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_greetings"
            ]
            , textView (
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text (getString THANK_YOU_FOR_WRTITTING_US)
            , color Color.black
            ] <> FontStyle.h1 TypoGraphy
            )
            , textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text (getString WE_HAVE_RECIEVED_YOUR_ISSUE)
            , gravity CENTER_HORIZONTAL
            , padding (Padding 30 0 30 0)
            , color Color.inactive
            ] <> FontStyle.paragraphText TypoGraphy
        ]
    ]
]