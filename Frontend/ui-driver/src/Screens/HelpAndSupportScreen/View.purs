module Screens.HelpAndSupportScreen.View where

import Prelude (Unit, ($), const, map, (<>), (<<<), (==), discard, bind, pure, unit, (&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, imageWithFallback)
import Effect (Effect)
import PrestoDOM.Types.DomAttributes as PTD
import PrestoDOM.Properties as PP
import Screens.HelpAndSupportScreen.Controller (Action(..), ScreenOutput, eval, getTitle)
import Screens.HelpAndSupportScreen.ScreenData (optionList, ListOptions(..))
import Components.SourceToDestination as SourceToDestination
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Animation as Anim
import Language.Strings (getString)
import Language.Types(STR(..))
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Aff (launchAff_)
import Control.Monad.Except (runExceptT)
import Effect.Class (liftEffect)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.Commons (flowRunner)
import Services.Backend as Remote
import Common.Types.App
import Screens.HelpAndSupportScreen.ComponentConfig

screen :: ST.HelpAndSupportScreenState -> Screen Action ST.HelpAndSupportScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "HelpAndSupportScreen"
  , eval
  , globalEvents : [( \push -> do
      launchAff_ $ flowRunner $ runExceptT $ runBackT $ do
        rideHistoryResponse <- Remote.getRideHistoryReqBT "1" "0" "false"
        lift $ lift $ doAff do liftEffect $ push $ RideHistoryAPIResponse rideHistoryResponse
      pure $ pure unit)]
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.HelpAndSupportScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ][ headerLayout state push
     , recentRideHeader state push (getString YOUR_RECENT_RIDE) (getString VIEW_ALL_RIDES)
     , recentRideDetails state push
     , recentRideHeader state push (getString ALL_TOPICS) ""
     , allOtherTopics state push
    ]

-------------------------------------------------- headerLayout --------------------------
headerLayout :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push = 
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
        , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
        , gravity CENTER_VERTICAL
        , onClick push (const BackPressed)
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        ]
      , textView
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text (getString Help_AND_SUPPORT)
        , textSize FontSize.a_19
        , margin (MarginLeft 20)
        , color Color.black
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , weight 1.0
        , gravity CENTER_VERTICAL
        , alpha 0.8
        ]
    ]
 ]

------------------------------------------ recentRide ------------------

recentRideHeader :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> String -> String -> forall w . PrestoDOM (Effect Unit) w
recentRideHeader state push leftText rightText = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , padding (Padding 15 10 10 10)
 , background Color.lightGreyBlue
 , onClick push $ const $ if (rightText == (getString VIEW_ALL_RIDES)) then ViewAllRides else NoRidesAction
 , visibility if ((rightText == (getString VIEW_ALL_RIDES)) && state.props.isNoRides) then GONE else VISIBLE
 ][ textView
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , text leftText
    , gravity CENTER_VERTICAL
    , fontStyle $ FontStyle.semiBold LanguageStyle
    , textSize FontSize.a_18
    , color Color.black800
    , lineHeight "25"
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ textView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , text rightText
        , textSize FontSize.a_17
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity RIGHT
        , color Color.blueTextColor
        ]
    ]
 ]

-------------------------------------------------- recentRideDetails --------------------
recentRideDetails :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
recentRideDetails state push = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , margin (Margin 10 10 10 10)
 , orientation VERTICAL
 , PP.cornerRadii $ PTD.Corners 8.0 true true true true  
 , stroke ("1," <> Color.grey900)
 , visibility if(state.props.isNoRides) then GONE else VISIBLE
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    ][  imageView 
        [ width $ V 150
        , height $ V 150
        , imageWithFallback state.data.mapImage
        , PP.cornerRadii $ PTD.Corners 8.0 true false false false  
        , visibility GONE
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , padding (Padding 10 10 10 10)
        ][  dateTimeView state
          , SourceToDestination.view (push <<< SourceToDestinationAction)  (sourceToDestinationConfig state)
          , driverRatingView state
        ]
    ]
    , horizontalLineView
  ]

------------------------------------------- dateTimeView -------------------------------------
dateTimeView :: ST.HelpAndSupportScreenState -> forall w . PrestoDOM (Effect Unit) w
dateTimeView state = 
 linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ][ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text state.data.date
        , textSize FontSize.a_13
        ]
        , linearLayout
        [ background Color.filterDisableButtonColor
        , cornerRadius 2.5
        , margin (Margin 5 0 5 0)
        , height (V 5)
        , width (V 5)
        ][]
        , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text state.data.time
        , textSize FontSize.a_13
        ]
    ]



------------------------------------------------- allOtherTopics ------------------------------
allOtherTopics :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
allOtherTopics state push =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , weight 1.0
 , scrollBarY false
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 5 0 5)
    ] (map(\optionItem ->
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , onClick push (const $ OptionClick optionItem.menuOptions)
            , visibility if (optionItem.menuOptions == CallSupportCenter) then VISIBLE else GONE
            ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              , padding (Padding 15 17 15 17)            
              ][  imageView
                  [ width $ V 20
                  , height $ V 20
                  , imageWithFallback optionItem.icon
                  ]
                  , textView
                  [ height WRAP_CONTENT
                  , weight 1.0
                  , text  (getTitle optionItem.menuOptions)
                  , margin (MarginLeft 10)
                  , color Color.black800
                  , textSize FontSize.a_17
                  ]
                  , imageView
                  [ width $ V 20
                  , height $ V 20
                  , imageWithFallback "ny_ic_chevron_right_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right_grey.png"
                  ]
              ]
              , horizontalLineView
            ]
          ) optionList
    )
 ]

-----------------------------------------------------------driverRatingView --------------------------------------------
driverRatingView :: ST.HelpAndSupportScreenState -> forall w . PrestoDOM (Effect Unit) w
driverRatingView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation HORIZONTAL
  , margin (MarginTop 20)
  , visibility GONE
  ][  textView
      [ text (getString YOU_RATED)
      , textSize FontSize.a_13
      , fontStyle $ FontStyle.medium LanguageStyle
      , color Color.black800
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin (MarginLeft 4)
      , gravity CENTER
      ](map 
        (\ item -> 
        linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin (MarginRight 4)
        ][imageView
            [ height $ V 14
            , width $ V 14
            , imageWithFallback "ny_ic_star_inactive,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_inactive.png"
            ]
          ]) [1,2,3,4,5])
    ]


--------------------------------------------------------------- horizontalLineView ----------------------------
horizontalLineView :: forall w . PrestoDOM (Effect Unit) w
horizontalLineView = 
 linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey800
  , alpha 0.9
  ][]
