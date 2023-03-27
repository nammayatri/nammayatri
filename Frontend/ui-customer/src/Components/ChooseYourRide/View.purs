module Components.ChooseYourRide.View where

import Components.ChooseYourRide.Controller(Action(..),Config)
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Font.Size as FontSize
import Prelude (Unit,($),(<>),const,pure,unit,not,(<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..),  background, color, cornerRadius,  fontStyle,  gravity, height, imageView, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, clickable, imageWithFallback)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App
import Components.ChooseVehicle as ChooseVehicle


view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit ) w
view push state =
     linearLayout
        [ orientation VERTICAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , clickable true
        , padding (Padding 16 16 16 24)
        , stroke ("1," <> Color.grey900)
        , gravity CENTER
        , cornerRadii $ Corners 24.0 true true false false 
        ][  textView
            [ text state.mainText 
            , textSize FontSize.a_22
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
            , estimatedTimeAndDistanceView push state 
            , pickUpZoneView push state 
            , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig state)
        ]
estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
estimatedTimeAndDistanceView push state = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginTop 4
  ][ textView
      [ text state.rideDistance
      , textSize FontSize.a_14
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
      [height $ V 4
      , width $ V 4
      , cornerRadius 2.5
      , background Color.black600
      , margin (Margin 6 2 6 0)
      ][]
    , textView
      [ text state.rideDuration
      , textSize FontSize.a_14
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
  ]
pickUpZoneView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
pickUpZoneView push state =
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , cornerRadius 8.0
      , margin $ MarginTop 16
      ][
         ChooseVehicle.view (push <<< ChooseNonAcVehicleController) (chooseVehicleNonAcConfig state )
      ,  ChooseVehicle.view (push <<< ChooseAcVehicleController) (chooseVehicleAcConfig state ) 
      ]

chooseVehicleNonAcConfig :: Config -> ChooseVehicle.Config
chooseVehicleNonAcConfig state =
  let
    config = ChooseVehicle.config
    chooseVehicleConfig' =
      config
        { 
          mainText {
            text = state.vehicleCard1.mainText,
            textSize = FontSize.a_18,
            visibility = VISIBLE,
            colour = Color.black800
          },
          subText1 {
            text = state.vehicleCard1.subText1,
            textSize = FontSize.a_12,
            visibility = VISIBLE,
            colour = Color.black700 
          },
          subText2{
          text = state.vehicleCard1.subText2,
          textSize = FontSize.a_12,
          visibility = VISIBLE,
          colour = Color.black700
          },  
          cornerText{
          text = state.vehicleCard1.ridefare,
          textSize = FontSize.a_18,
          visibility = VISIBLE,
          colour = Color.black800
          },
          primaryimage{
            imageUrl= state.vehicleCard1.imageUrl  
          },
          onselect = state.selectedCar1,
          margin = state.vehicleCard1.margin
        }
  in
    chooseVehicleConfig'
chooseVehicleAcConfig :: Config -> ChooseVehicle.Config
chooseVehicleAcConfig state =
  let
    config = ChooseVehicle.config
    chooseVehicleConfig' =
      config
        { 
          mainText {
            text = state.vehicleCard2.mainText,
            textSize = FontSize.a_18,
            visibility = VISIBLE,
            colour = Color.black800
          },
          subText1 {
            text = state.vehicleCard2.subText1,
            textSize = FontSize.a_12,
            visibility = VISIBLE,
            colour = Color.black700 
          },
          subText2{
          text = state.vehicleCard2.subText2,
          textSize = FontSize.a_12,
          visibility = VISIBLE,
          colour = Color.black700
          },  
          cornerText{
          text = state.vehicleCard2.ridefare,
          textSize = FontSize.a_18,
          visibility = VISIBLE,
          colour = Color.black800
          },
          onselect =  not state.selectedCar1 ,
          primaryimage {
              imageUrl = state.vehicleCard2.imageUrl
          },
          margin = state.vehicleCard2.margin
        }
  in
    chooseVehicleConfig'

primaryButtonRequestRideConfig :: Config -> PrimaryButton.Config
primaryButtonRequestRideConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = state.primaryButtonText 
          , color = Color.yellow900
          , textSize = FontSize.a_16
          }
        , background = Color.black900
        , margin = (Margin 0 32 0 15)
        }
  in
    primaryButtonConfig'
