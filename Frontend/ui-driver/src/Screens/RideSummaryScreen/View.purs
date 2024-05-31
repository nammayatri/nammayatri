module Screens.RideSummaryScreen.View where
import Data.Maybe
import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Screens.RideSummaryScreen.ComponentConfig
import Components.SourceToDestination as SourceToDestination
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Components.PrimaryButton as PrimaryButton
import Components.SelectMenuButton as MenuButton
import Constants (defaultSeparatorCount, getSeparatorFactor)
import Data.Array as DA
import Data.Function.Uncurried (runFn1)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, os)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (Unit, const, ($), (<<<), (==))
import PrestoDOM (visibility, Visibility(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, maxLines, id, alignParentBottom, scrollBarY, fontSize, shimmerFrameLayout, relativeLayout, stroke, clickable, frameLayout, afterRender, alpha, background, color, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, rippleColor, cornerRadius)
import PrestoDOM.Animation as PrestoAnim
import Prim.TypeError (class Warn)
import Screens.RideSummaryScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
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
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    ([headerLayout state push
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey900
        ][]
     , scrollView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , weight 1.0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ rideInfoCard state push
            , pickUpCard state push 
            , includedChargesCard state push 
            , excludedChargesCard state push
            , termsAndConditionCard state push 
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey900
        ][]
     ,  buttonLayout state push
    ])



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
            , onClick push $ const $ BackPressed
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








rideInfoCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
rideInfoCard state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 16 16 16
    , cornerRadius 16.0
    , stroke ("1," <> Color.grey700)
    , gravity CENTER
    
    ]
    [ cabInfoLayout state push
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey700
        , margin $ Margin 16 12 16 12
        ][]
    , scheduleInfoLayout state push
    ]

cabInfoLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cabInfoLayout state push = 
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
  , vehicleInfo state push
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

vehicleInfo :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
vehicleInfo state push = 
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
    [ vehicleNonACTierPill state push
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
    , vehicleCapacity state push
    ]


scheduleInfoLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
scheduleInfoLayout state push = 
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
        [ scheduleTiming state push
        , linearLayout
            [ width WRAP_CONTENT
            , height $ V 5
            ][]
        , scheduleTiming state push
        ]
    , linearLayout 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , weight 1.0
        ][]
    , rideTypePill state push 
    ]

scheduleTiming :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
scheduleTiming state push =
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
        , color Color.black700
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
            , color Color.black700
            , text "31/05/2024"
            ] <> FontStyle.tags TypoGraphy
        , imageView 
            [ width $ V 3
            , height $ V 3
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_inner_fill"
            , color Color.black700
            , margin $ MarginHorizontal 3 3
            ] 
        , textView $ 
            [ width WRAP_CONTENT 
            , height WRAP_CONTENT
            , text "7:35pm"
            , color Color.black700
            , textSize FontSize.a_12
            ] <> FontStyle.tags TypoGraphy

        ]
    ]


rideTypePill :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
rideTypePill state push = 
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


vehicleNonACTierPill :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
vehicleNonACTierPill state push = 
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
        ]
    , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text "Non-AC"
        , color Color.white900
        , margin $ MarginLeft 2
        , gravity CENTER
        ] <> FontStyle.tags TypoGraphy
    ]



vehicleCapacity :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
vehicleCapacity state push = 
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
        , color Color.black700
        ] <> FontStyle.tags TypoGraphy
    ]
    




cardHeadingLayout :: Action -> String -> RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cardHeadingLayout action heading state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 2 2 2 2
        , onClick push $ const action
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





pickUpCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
pickUpCard state push = 
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
        [ cardHeadingLayout PickUpOpen "Pickup & Drop" state push

        , linearLayout
            [ width MATCH_PARENT
            , height $ V 160
            , margin $ MarginVertical 16 16
            , cornerRadius 16.0 
            , stroke ("1," <> Color.grey900)
            , visibility if state.props.pickUpOpen then VISIBLE else GONE
            ][]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , visibility if state.props.pickUpOpen then VISIBLE else GONE
            ]
            [
        SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)

            ]
        ]











includedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesCard state push = 
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
        [ cardHeadingLayout IncludedChargesOpen "Included charges" state push 
        , includedChargesBox state push 
        , includedChargesFooter state push
        ]





includedChargesFooter :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w

includedChargesFooter state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility if state.props.includedChargesOpen then VISIBLE else GONE
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





includedChargesBox :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
includedChargesBox state push = 
    linearLayout
        [ width MATCH_PARENT
        , height $ V 96
        , orientation HORIZONTAL
        , margin $ MarginVertical 20 20
        -- , gravity CENTER
        , visibility if state.props.includedChargesOpen then VISIBLE else GONE
        ]
        [ linearLayout
            [ width $ V 136
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ chargesTile "Per km charge" "Rs.18/km"  state push
            , linearLayout 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , weight 1.0
                ][]
            , chargesTile "Extra Time Charges*" "Rs50/hr" state push 
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
            [ chargesTile "Driver Allowance" "Rs.18/km"  state push
            , linearLayout 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , weight 1.0
                ][]
            , chargesTile "Extra Distance Charges*" "Rs50/hr" state push 
            ]
        ]





chargesTile :: String -> String -> RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
chargesTile text1 text2 state push= 
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





excludedChargesCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
excludedChargesCard state push = 
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
        [ cardHeadingLayout ExcludedChargesOpen "Excluded charges" state push 
        , linearLayout 
            [ width MATCH_PARENT
            , height $ V 20
            , visibility if state.props.excludedChargesOpen then VISIBLE else GONE
            ][]
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 108
            , orientation VERTICAL
            , visibility if state.props.excludedChargesOpen then VISIBLE else GONE
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
        , linearLayout 
            [ width MATCH_PARENT
            , height $ V 20
            , visibility if state.props.excludedChargesOpen then VISIBLE else GONE
            ][]
        , textView $ 
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text "These fare components are NOT included in the fare and must be settled separately with the customer"
            , color Color.black600
            , visibility if state.props.excludedChargesOpen then VISIBLE else GONE
            ] <> FontStyle.body3 TypoGraphy 
        ]




termsAndConditionCard :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionCard state push = 
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
        [ cardHeadingLayout TermsConditionOpen "Terms & Conditions" state push 
        , linearLayout
            [ width WRAP_CONTENT
            , height $ V 20
            , visibility if state.props.termsAndConditionOpen then VISIBLE else GONE 
            ][]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , visibility if state.props.termsAndConditionOpen then VISIBLE else GONE 
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
        ]









buttonLayout :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
buttonLayout state push = 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 16 16 16 16
        , gravity CENTER
        ]
        [ passButton state push 
        , linearLayout
            [ width $ V 20
            , height WRAP_CONTENT
            ][]
        , acceptButton state push 
        ]






acceptButton :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
acceptButton state push = 
    linearLayout 
        [ width $ V 232
        , height WRAP_CONTENT
        , padding $ Padding 16 18 16 18
        , cornerRadius 8.0
        , gravity CENTER
        , background Color.blue800
        , onClick push $ const AcceptClick
        ]
        [ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Accept"
            , color Color.white900
            ] <> FontStyle.h2 TypoGraphy
        ]





passButton :: RideSummaryScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
passButton state push = 
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







































