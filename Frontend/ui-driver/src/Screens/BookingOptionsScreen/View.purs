module Screens.BookingOptionsScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (capitalizeFirstChar, getVehicleType)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, ($), (<<<), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width)
import Screens.BookingOptionsScreen.Controller (Action(..), ScreenOutput, eval, getVehicleCapacity)
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

screen :: ST.BookingOptionsScreenState -> Screen Action ST.BookingOptionsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BookingDetailsScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let
            _ = spy "BookingOptionsScreenState -----" state
          let
            _ = spy "BookingOptionsScreenState--------action" action
          eval state action
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background Color.white900
        , padding $ PaddingBottom 24
        ]
        [ headerLayout push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding $ Padding 16 16 16 16
            ]
            [ defaultVehicleView push state
            ]
        , downgradeOptionsView push state
        , linearLayout
          [ height MATCH_PARENT
          , width $ V 1
          , weight 1.0
          ] []
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
        ]

defaultVehicleView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
defaultVehicleView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , cornerRadius 8.0
    , padding $ Padding 10 20 10 30
    , stroke $ "1," <> Color.grey900
    ]
    [ vehicleDetailsView push state
    , vehicleLogoAndType push state
    ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleDetailsView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    ]
    [ linearLayout
        [ orientation VERTICAL
        , weight 1.0
        ]
        [ customTV (getString YOUR_VEHICLE) FontSize.a_12 FontStyle.body3 Color.black650
        , customTV (capitalizeFirstChar state.data.vehicleName) FontSize.a_20 FontStyle.h3 Color.black800
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , cornerRadius 6.0
        , background Color.golden
        , padding $ Padding 3 3 3 3
        ]
        [ textView
            $ [ width MATCH_PARENT
              , height MATCH_PARENT
              , padding $ Padding 5 3 5 3
              , text state.data.vehicleNumber
              , color Color.black800
              , gravity CENTER
              , cornerRadius 3.0
              , stroke $ "2," <> Color.black800
              ] <> FontStyle.body8 TypoGraphy
        ]
    ]

vehicleLogoAndType :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleLogoAndType push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 26
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ imageView
            [ imageWithFallback $ "ic_suv_ac," <> (getAssetStoreLink FunctionCall) <> "ic_suv_ac.png"
            , gravity LEFT
            , height $ V 48
            , width $ V 60
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , margin $ MarginLeft 7
            ]
            [ customTV (getVehicleType state.data.vehicleType) FontSize.a_20 FontStyle.h3 Color.black800
            , customTV (getVehicleCapacity state.data.vehicleType (Just state.data.vehicleCapacity)) FontSize.a_12 FontStyle.body3 Color.black650
            ]
        ]
    ]

headerLayout :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ PaddingVertical 10 10
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_chevron_left.png"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView $
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , text $ getString BOOKING_OPTIONS
            , margin $ MarginLeft 20
            , color Color.black
            , weight 1.0
            , gravity CENTER_VERTICAL
            , alpha 0.8
            ] <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

downgradeOptionsView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
downgradeOptionsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 56
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingHorizontal 16 16
        , margin $ MarginBottom 14
        ]
        [ customTV (getString MAKE_YOURSELF_AVAILABLE_FOR) FontSize.a_20 FontStyle.h3 Color.black
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , padding $ PaddingVertical 6 6
                  ]
                  [ ChooseVehicle.view (push <<< ChooseVehicleAC) item
                  ]
            ) state.data.downgradeOptions
        )
    ]

customTV :: forall w. String -> Int -> (LazyCheck -> forall properties. (Array (Prop properties))) -> String -> PrestoDOM (Effect Unit) w
customTV text' textSize' fontStyle' color' =
  textView
    $ [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text text'
      , textSize textSize'
      , color color'
      ]
    <> fontStyle' TypoGraphy

primaryButtonConfig :: ST.BookingOptionsScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = (getString CONFIRM_AND_CHANGE)
      }
      , margin = (Margin 16 0 16 0)
      , alpha = if state.props.isBtnActive then 1.0 else 0.4
      , isClickable = state.props.isBtnActive
      , id = "BookingOptionsScreenPrimaryButton"
      }
  in primaryButtonConfig'