module Components.ChooseYourRide.View where

import Common.Types.App

import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide.Controller (Action(..), Config)
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex, length)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<>), const, pure, unit, not, (<<<), (==), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, scrollView)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , margin $ MarginTop 10
    , clickable true
    , padding $ PaddingVertical 16 24
    , stroke $ "1," <> Color.grey900
    , gravity CENTER
    , cornerRadii $ Corners 24.0 true true false false
    ]
    [ textView (
        [ text (getString CHOOSE_YOUR_RIDE)
        , color Color.black800
        , gravity CENTER_HORIZONTAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        ] <> FontStyle.h1 TypoGraphy)
    , estimatedTimeAndDistanceView push config
    , quoteListView push config
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config)
    ]

estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
estimatedTimeAndDistanceView push config =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginTop 4
    ]
    [ textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDistance
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.5
        , background Color.black600
        , margin (Margin 6 2 6 0)
        ]
        []
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDuration
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy
    ]

quoteListView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
quoteListView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 16
    ]
    [ scrollView
      [ height $ getQuoteListViewHeight config
      , width MATCH_PARENT
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]( mapWithIndex
              ( \index item ->
                  ChooseVehicle.view (push <<< ChooseVehicleAC) (item)
              ) config.quoteList
          )]]

getQuoteListViewHeight :: Config -> Length
getQuoteListViewHeight config = 
    if length config.quoteList >= 4 then V 300 else if length config.quoteList == 1 then V 90 else V 164

primaryButtonRequestRideConfig :: Config -> PrimaryButton.Config
primaryButtonRequestRideConfig config = PrimaryButton.config
  { textConfig
    { text = (getString CONFIRM_AND_BOOK)
    , color = Color.yellow900

    }
  , background = Color.black900
  , margin = Margin 16 32 16 15
  }
