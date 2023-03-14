module Components.TutorialModal.View where

import Prelude(Unit, ($), const, (<>))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, clickable, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, width, textView, fontStyle, text, color, weight, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config (translateYAnimConfig)
import Animation (translateYAnim)
import Components.TutorialModal.Controller (Action(..), State)
import Styles.Colors as Color
import PrestoDOM.Properties(cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Language.Types (STR(..))
import Language.Strings (getString)
import Font.Style as FontStyle
import Common.Types.App

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity BOTTOM
    , orientation VERTICAL
    , background Color.black9000
    , onBackPressed push (const OnCloseClick)
    , clickable true
    ][  PrestoAnim.animationSet [
            translateYAnim translateYAnimConfig
        ] $ 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (PaddingHorizontal 20 20)
        , cornerRadii $ Corners 20.0 true true false false
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity RIGHT
            ][ imageView
                [ height $ V 18
                , width $ V 18
                , margin (MarginTop 20)
                , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
                , clickable true
                , onClick push (const OnCloseClick)
                ]
            ]
        ,   imageView
            [ height $ V 290
            , width WRAP_CONTENT
            , imageWithFallback state.imageUrl
            , weight 1.0
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding (PaddingVertical 40 40)
            , orientation VERTICAL
            , gravity LEFT
            ][  textView
                ([ height WRAP_CONTENT
                , width MATCH_PARENT
                , text (getString STILL_HAVE_SOME_DOUBT)
                , color Color.black700
                , margin $ MarginBottom 12
                ] <> FontStyle.body3 TypoGraphy)
            ,  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity LEFT
                , orientation HORIZONTAL
                , onClick push (const CallSupport)
                ][ imageView
                [ imageWithFallback "ny_ic_support,https://assets.juspay.in/nammayatri/images/driver/ny_ic_support.png"
                , height $ V 17
                , width $ V 20
                , margin $ (Margin 0 0 7 27)
                ]
                , textView
                    ([ text (getString CALL_SUPPORT_CENTER)
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy)
                ]
            ,  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity LEFT
                , orientation HORIZONTAL
                , onClick push (const Logout)
                ][ imageView
                [ imageWithFallback "ny_ic_logout_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_logout_grey.png"
                , height $ V 17
                , width $ V 20
                , margin $ MarginRight 7
                ]
                , textView
                    ([ text (getString LOGOUT)
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy)
                ]
            ]
        ]
    ]