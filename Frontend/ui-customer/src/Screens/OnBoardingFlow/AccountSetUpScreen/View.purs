{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.View where

import Animation as Anim 
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Effect (Effect)
import Engineering.Helpers.Commons as EHC 
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, ($), (<<<), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, alignParentBottom, background, color, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, relativeLayout, scrollView, singleLine, text, textView, weight, width)
import Screens.AccountSetUpScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Screens.AccountSetUpScreen.ComponentConfig
import Constant.Test as Id


screen :: ST.AccountSetUpScreenState -> Screen Action ST.AccountSetUpScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AccountSetUpScreen"
  , globalEvents: []
  , eval
  }

view ::
  forall w.
  (Action -> Effect Unit) -> ST.AccountSetUpScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.white900
        , onBackPressed push (const BackPressed)
        , afterRender push (const AfterRender)
        , Id.testId $ Id.Screen Id.accountSetUpScreen
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin (Margin 0 16 0 24)
            , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
            , background Color.white900
            , onBackPressed push (const BackPressed)
            ]
            [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig)
            , scrollView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , padding (Padding 16 0 16 0)
                    ]
                    [ textView
                        $ [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , text (getString SET_UP_YOUR_ACCOUNT)
                          , color Color.black800
                          , gravity LEFT
                          , singleLine true
                          ]
                        <> FontStyle.h1 TypoGraphy
                    , PrimaryEditText.view (push <<< NameEditTextActionController) (primaryEditTextConfigName state)
                    , linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , weight 1.0
                        , orientation VERTICAL
                        ]
                        []
                    ]
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , background Color.white900
            , padding (Padding 16 0 16 26)
            ]
            [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) ]
        , if state.props.backPressed then goBackPopUpView push state else emptyTextView
        ]

goBackPopUpView :: forall w. (Action -> Effect Unit) -> ST.AccountSetUpScreenState -> PrestoDOM (Effect Unit) w
goBackPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) goBackPopUpModelConfig ]

------------------------ emptyTextView ---------------------------
emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView []
