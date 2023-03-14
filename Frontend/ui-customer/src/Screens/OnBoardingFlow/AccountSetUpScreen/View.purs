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
