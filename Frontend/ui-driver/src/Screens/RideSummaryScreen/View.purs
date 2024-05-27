module Screens.RideSummaryScreen.View where


import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Screens.SelectLanguageScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Components.PrimaryButton as PrimaryButton
import Components.SelectMenuButton as MenuButton
import Components.SeparatorView.View as SeparatorView
import Data.Array as DA
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (Unit, const, ($), (<<<), (==))
import PrestoDOM (visibility, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, maxLines, fontSize, shimmerFrameLayout, relativeLayout, stroke, clickable, frameLayout, afterRender, alpha, background, color, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, rippleColor, cornerRadius)
import PrestoDOM.Animation as PrestoAnim
import Prim.TypeError (class Warn)
import Screens.RideSummaryScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Styles.Colors as Color


screen :: RideSummaryScreenState -> Screen Action RideSummaryScreenState ScreenOutput
screen initialState =  
    { initialState
    , view : view
    , name: "RideSummaryScreen"
    , globalEvents : [] 
    , eval : (\action state -> do
        let _ = spy "RideRequestScreen state -----" state
        let _ = spy "RideRequestScreen action --------" action
        eval action state)
    }

view :: forall w. (Action -> Effect Unit) -> RideSummaryScreenState -> PrestoDOM (Effect Unit) w 
view push state  =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    -- , onBackPressed push $ const BackPressed
    -- , onAnimationEnd push $ const $  NoAction
    -- , background Color.grey700
    ]
    ([scrollView
        [ width MATCH_PARENT
        , height WRAP_CONTENT

        ]
        [linearLayout
            [height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [
            headerLayout state push
            , rideSummaryCard state push
            , includedChargesCard state push 
            , termsCard state push 
            , test2 state push
            , clickInnerBox state push
            ]
        ]
    ])






test2 :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
test2 state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ Margin 16 0 16 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 16.0
        , stroke ("1," <> Color.grey900)
        , gravity CENTER
        ]
        [ boxHeading "Excluded charges" state push 
        , linearLayout 
            [ width MATCH_PARENT
            , height $ V 20
            ][]
        , excludedChargesInner state push 
        , linearLayout 
            [ width MATCH_PARENT
            , height $ V 20
            ][]
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "These fare components are NOT included in the fare and must be settled separately with the customer"
            , color Color.black600
            ] <> FontStyle.body3 TypoGraphy 
        ]




excludedChargesInner :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
excludedChargesInner state push = 
    linearLayout
        [ width MATCH_PARENT
        , height $ V 108
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height $ V 20
            , orientation HORIZONTAL
            , margin $ MarginBottom 24
            ]
            [ imageView
                [ width $ V 20
                , height $ V 20
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_car_8"
                , margin $ MarginRight 8 
                , color Color.black900
                ]
            , textView $ 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text "Tolls"
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height $ V 20
            , orientation HORIZONTAL
            , margin $ MarginBottom 24
            ]
            [ imageView
                [ width $ V 20
                , height $ V 20
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_certificate"
                , margin $ MarginRight 8 
                , color Color.black900
                ]
            , textView $ 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text "State Permits"
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height $ V 20
            , orientation HORIZONTAL
            ]
            [ imageView
                [ width $ V 20
                , height $ V 20
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_parking"
                , margin $ MarginRight 8 
                , color Color.black900
                ]
            , textView $ 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text "Parking Charges"
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
            ]

        ]






clickInnerBox :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
clickInnerBox state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 16 16 16 16
        , gravity CENTER
        ]
        [ passBox state push 
        , linearLayout
            [ width $ V 20
            , height WRAP_CONTENT
            ][]
        , acceptBox state push 
        ]






acceptBox :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
acceptBox state push = 
    linearLayout 
        [ width $ V 232
        , height WRAP_CONTENT
        , padding $ Padding 16 18 16 18
        , cornerRadius 8.0
        , gravity CENTER
        , background Color.blue800
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Accept"
            , color Color.white900
            ] <> FontStyle.h2 TypoGraphy
        ]





passBox :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
passBox state push = 
    linearLayout
        [ width $ V 84
        , height MATCH_PARENT
        , stroke ("1," <> Color.black500)
        , padding $ Padding 24 18 24 18
        , cornerRadius 8.0
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Pass"
            ] <> FontStyle.subHeading1 TypoGraphy
        ]





termsCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsCard state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ Margin 16 0 16 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 16.0
        , stroke ("1," <> Color.grey900)
        , gravity CENTER
        ]
        [ boxHeading "Terms & Conditions" state push 
        , linearLayout
            [ width WRAP_CONTENT
            , height $ V 20
            ][]
        , termsFooter state push 
        ]





termsFooter :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsFooter state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "1. Cancellation before 1 hr from the scheduled ride start will lead to penalties on the platform. "
            , color Color.black600
            , margin $ MarginBottom 12
            ] <> FontStyle.body3 TypoGraphy
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "2. Be online before 45 minutes of Scheduled Ride Start "
            , color Color.black600
            , margin $ MarginBottom 12
            ] <> FontStyle.body3 TypoGraphy
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "3. Reach Customer pickup location before 15 minutes from scheduled ride start time"
            , color Color.black600

            ] <> FontStyle.body3 TypoGraphy

        ]





includedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesCard state push = 
    linearLayout
        [ width MATCH_PARENT
        , height $ V 286
        , orientation VERTICAL
        , margin $ Margin 16 0 16 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 16.0
        , stroke ("1," <> Color.grey900)
        , gravity CENTER
        ]
        [ boxHeading "Included charges" state push 
        , includedCharges state push 
        , includedFooter state push
        ]





includedFooter :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedFooter state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "*Extra Charges are only applicable if you exceed the included distance / time"
            , color Color.black600
            , margin $ MarginBottom 12
            ] <> FontStyle.body3 TypoGraphy
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "Night time charges: Driving between 10:00 pm and 5:00 am will have a flat night time charge added to the fare."
            , color Color.black600

            ] <> FontStyle.body3 TypoGraphy

        ]





includedCharges :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedCharges state push = 
    linearLayout
        [ width MATCH_PARENT
        , height $ V 96
        , orientation HORIZONTAL
        , margin $ MarginVertical 20 20
        -- , gravity CENTER
        ]
        [ linearLayout
            [ width $ V 136
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ includedTextBox "Per km charge*" "Rs.18/km"  state push
            , linearLayout 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , weight 1.0
                ][]
            , includedTextBox "Extra Time Charges" "Rs50/hr" state push 
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , weight 1.0
            ][]
        , linearLayout
            [ width $ V 136
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ includedTextBox "Driver Allowance*" "Rs.18/km"  state push
            , linearLayout 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , weight 1.0
                ][]
            , includedTextBox "Extra Time Charges" "Rs50/hr" state push 
            ]
        ]





includedTextBox :: String -> String -> RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedTextBox text1 text2 state push= 
    linearLayout 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text text1
            , color Color.black600
            , textSize FontSize.a_12
            ]
        , textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text text2
            , color Color.black900
            ] <> FontStyle.body1 TypoGraphy
        ]






boxHeading :: String -> RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
boxHeading heading state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 2 2 2 2
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text heading
            , weight 1.0
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
        , imageView 
            [ width $ V 24     
            , height $ V 24
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , color Color.black800
            ]
        ]






















rideSummaryCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
rideSummaryCard state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 16 16 16
    , cornerRadius 16.0
    , stroke ("1," <> Color.grey900)
    , gravity CENTER
    ]
    [ rideInfo2 state push
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.black700
        , margin $ Margin 16 12 16 12
        ][]
    , pickupInfo state push
    ]




pickupInfo :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
pickupInfo state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL

    ]
    [ linearLayout 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding $ Padding 4 0 4 0
        ]
        [ dateAndTime state push
        , linearLayout
            [ width WRAP_CONTENT
            , height $ V 5
            ][]
        , dateAndTime state push
        ]
    , linearLayout 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , weight 1.0
        ][]
    , rideInfo state push 
    ]
rideInfo2 :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
rideInfo2 state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , padding $ PaddingVertical 2 2
  ]
  [ imageView
      [ width $ V 43
      , height $ V 31
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_sedan"
      , margin $ MarginRight 4 
      -- , weight 1.0
      ]
  , cabInfo state push
  , linearLayout 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , weight 1.0
      ][]
  , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text "Rs.2300"
      , textSize FontSize.a_18
      , gravity LEFT 
      , color Color.black900
      ] <> FontStyle.h2 TypoGraphy
  ]




rideInfo :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
rideInfo state push = 
  linearLayout
    [ height $ V 35
    , width $ V 85
    , orientation HORIZONTAL
    , background Color.blue800
    , gravity CENTER
    , padding $ Padding 8 2 8 2
    , cornerRadius 8.0
    ]
    [ imageView
        [ height $ V 16
        , width $ V 16
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_intercity"
        ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Intercity Return"
        , color Color.white900
        , margin $ MarginLeft 4
        , textSize FontSize.a_12
        , gravity CENTER
        ] <> FontStyle.body27 TypoGraphy
    ]


cabInfo :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cabInfo state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , cornerRadius 38.0
    , background Color.blue600
    , padding $ Padding 4 4 12 4
    , gravity CENTER
    -- , weight 1.0
    ]
    [ nonACText state push
    , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text "Mini"
        , textSize FontSize.a_12
        , color Color.black700
        , gravity CENTER
        , margin $ MarginLeft 4
        ] <> FontStyle.tags TypoGraphy
    , imageView 
        [ width $ V 3
        , height $ V 3
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_inner_fill"
        -- , margin $ Margin 4 8 0 0
        , color Color.black700
        , margin $ MarginHorizontal 4 0
        ]  
    , peopleCount state push
    ]



nonACText :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
nonACText state push = 
  linearLayout 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , cornerRadius 18.0
    , background Color.black700
    , padding $ Padding 4 2 4 2
    , gravity CENTER
    ]
    [ imageView
        [ height $ V 12
        , width $ V 12
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_nonac"
        , color Color.white900
        -- , weight 1.0
        ]
    , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text "Non-AC"
        , textSize FontSize.a_12
        , color Color.white900
        , margin $ MarginLeft 2
        , gravity CENTER
        ] <> FontStyle.tags TypoGraphy
    ]



dateAndTime :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
dateAndTime state push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    ]
    [ textView $ 
        [ width WRAP_CONTENT 
        , height WRAP_CONTENT
        , text "Pickup:"
        , textSize FontSize.a_12
        ] <> FontStyle.tags TypoGraphy
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER
        ]
        [ textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , textSize FontSize.a_12
            , color Color.black700
            , text "31/05/2024"
            ] <> FontStyle.tags TypoGraphy
        , imageView 
            [ width $ V 3
            , height $ V 3
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_inner_fill"
            , color Color.black700
            -- , padding $ PaddingTop 5
            , margin $ MarginHorizontal 3 3
            ] 
        , textView $ 
            [ width WRAP_CONTENT 
            , height WRAP_CONTENT
            , text "7:35pm"
            , color Color.black700
            , textSize FontSize.a_12
            -- , margin $ MarginLeft 4
            ] <> FontStyle.tags TypoGraphy

        ]
    ]



peopleCount :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
peopleCount state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width $ WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    , margin $ MarginHorizontal 4 4
    ]
    [ imageView
        [ width $ V 12
        , height $ V 12
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_user_filled"
        , color Color.black700
        ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "4"
        , gravity CENTER
        , textSize FontSize.a_12
        , color Color.black700
        ] <> FontStyle.tags TypoGraphy
    ]
    







headerLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push  =
  linearLayout
    [ width MATCH_PARENT
      , height WRAP_CONTENT
    ,orientation HORIZONTAL
    , background Color.white900
    ]
    [  headerLeft state push
    ]
headerLeft :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLeft state push  =
   linearLayout
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 5 16 5 16
        , weight 1.0
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            -- , onClick push $ const $ BackPressed
            , margin $ Margin 5 0 0 0
            , rippleColor Color.rippleShade
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text "Ride Summary"
              , textSize FontSize.a_18
              , margin $ Margin 5 0 0 3
              , weight 1.0
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]

















