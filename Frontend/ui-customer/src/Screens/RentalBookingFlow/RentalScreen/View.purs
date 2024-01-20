{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.ChooseVehicle.View as ChooseVehicle
import Components.GenericHeader.View as GenericHeader
import Components.IncrementDecrementModel.View as IncrementDecrement
import Components.InputView.View as InputView
import Components.PrimaryButton as PrimaryButton
import Data.Array (singleton)
import Debug (spy)
import Effect (Effect)
import Font.Style as FontStyle
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, gravity, height, linearLayout, margin, onClick, orientation, padding, scrollBarY, scrollView, stroke, text, textView, visibility, weight, width, cornerRadius, id, onAnimationEnd)
import Screens.RentalBookingFlow.RentalScreen.ComponentConfig (genericHeaderConfig, incrementDecrementConfig, mapInputViewConfig, primaryButtonConfig)
import Screens.RentalBookingFlow.RentalScreen.Controller (Action(..), FareBreakupRowType(..), ScreenOutput, eval)
import Screens.Types (RentalScreenState, RentalScreenStage(..))
import Styles.Colors as Color
import Engineering.Helpers.Commons (getNewIDWithTag)
import JBridge(renderSlider, sliderConfig)
import Animation as Anim
import Effect.Uncurried (runEffectFn3)

rentalScreen :: RentalScreenState -> Screen Action RentalScreenState ScreenOutput
rentalScreen initialState =
  { initialState
  , view
  , name: "RentalScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let _ = spy "RentalScreen action " action
        let _ = spy "RentalScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ case state.data.currentStage of
        RENTAL_SELECT_PACKAGE -> rentalPackageSelectionView push state
        RENTAL_SELECT_VARIANT -> rentalVariantSelectionView push state
        RENTAL_CONFIRMATION -> fareBreakupView push state
    ]

rentalPackageSelectionView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
rentalPackageSelectionView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ InputView.view (push <<< InputViewAC) $ mapInputViewConfig state
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 16
      ]
      [ 
      --   IncrementDecrement.view (push <<< DurationIncrementDecrementAC) (incrementDecrementConfig "Duration" state) 
      -- , linearLayout [
      --     height WRAP_CONTENT
      --   , width WRAP_CONTENT
      --   , margin $ MarginLeft 12
      --   ]
      --   [ IncrementDecrement.view (push <<< DistanceIncrementDecrementAC) (incrementDecrementConfig "Distance" state) ]
      -- , 
      linearLayout [
          height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginLeft 12
        , background Color.squidInkBlue
        , cornerRadius 12.0 
        , orientation VERTICAL
        , padding $ Padding 16 32 16 32
        ]
        [ textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , text $ (show state.data.sliderVal) <> " hours"
          , color Color.white900
          ] <> FontStyle.heading TypoGraphy
        , sliderView push state
        ]
      ]
      , linearLayout [
          height MATCH_PARENT
        , width MATCH_PARENT
        , gravity BOTTOM
        , orientation VERTICAL
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , stroke $ "1," <> Color.grey900
          , padding $ Padding 16 16 16 16
          ] 
          [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
          ]
        ]
    ]

sliderView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 24
    ][  textView $ 
          [ text "2 hr"
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
      , Anim.screenAnimationFadeInOut $
          linearLayout
            [ height WRAP_CONTENT
            , weight 1.0 
            , id $ getNewIDWithTag "DurationSliderView"
            , background Color.squidInkBlue
            , onAnimationEnd 
                (\ action -> 
                  void $ runEffectFn3 renderSlider push SliderCallback 
                    sliderConfig
                    { id = (getNewIDWithTag "DurationSliderView")
                    , sliderMinValue = 2
                    , sliderMaxValue = 12
                    , sliderDefaultValue = 2
                    , toolTipId = getNewIDWithTag "DurationSliderViewToolTip"
                    , progressColor = Color.white900 
                    , thumbColor = Color.blue800
                    , bgColor = Color.white900
                    , bgAlpha = 1000 }
                    
                )(const NoAction)
              ][]
      , textView $ 
          [ text "12 hr"
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy

    ]
rentalVariantSelectionView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
rentalVariantSelectionView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ InputView.view (push <<< InputViewAC) $ mapInputViewConfig state
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ Padding 16 16 16 16
        ]
        [ textView $
          [ text $ getString RENTAL_OPTIONS
          ] <> FontStyle.h1 TypoGraphy
        , separatorView push state
        ]
    , scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin $ Margin 16 0 16 32
          , orientation VERTICAL
          ]
          ( map
            ( \item ->
                ChooseVehicle.view (push <<< ChooseVehicleAC) item
            )
            state.data.quoteList
          )
        ]
    , linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , gravity BOTTOM
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , stroke $ "1," <> Color.grey900
        , padding $ Padding 16 16 16 16
        ] [PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
      ]
    ]

fareBreakupView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
fareBreakupView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
      [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
      , separatorView push state
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ Margin 16 16 16 32
        ] $ map ( 
            \item -> if (item.index == item.activeIndex) then ChooseVehicle.view (\action -> pure unit) (item{showStroke = false, showInfo = false}) else dummyView 
            ) state.data.quoteList
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginHorizontal 16 16
        ]
        (map (\item -> 
          descriptionView push state (item)
        ) [BookingTime, BookingDistance, BaseFare, TollFee])
      , noteAndPrimaryButtonView push state
      ]

descriptionView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> FareBreakupRowType -> PrestoDOM (Effect Unit) w
descriptionView push state description =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getTitleFromDescription description true
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy
    , textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getTitleFromDescription description false
      , color Color.black700
      , margin $ MarginVertical 4 24
      ] <> FontStyle.paragraphText TypoGraphy
    ]
  where 
    getTitleFromDescription :: FareBreakupRowType -> Boolean -> String
    getTitleFromDescription description toShowTitle = 
      case description of
        BookingTime -> if toShowTitle then getString BOOKING_ON <> state.data.rentalBookingData.selectedDate <> ", " <> state.data.rentalBookingData.selectedTime <> " (" <> (show state.data.rentalBookingData.baseDuration) <> "hrs)" else getString FINAL_FARE_DESCRIPTION
        BookingDistance -> if toShowTitle then getString INCLUDED_KMS <> (show state.data.rentalBookingData.baseDuration) else getString EXCESS_DISTANCE_CHARGE_DESCRIPTION <> state.props.farePerKm <> "/km."
        BaseFare -> if toShowTitle then getString BASE_FARE else getString ADDITIONAL_CHARGES_DESCRIPTION
        TollFee -> if toShowTitle then getString TOLLS_AND_PARKING_FEES else getString PARKING_FEES_AND_TOLLS_NOT_INCLUDED

noteAndPrimaryButtonView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
noteAndPrimaryButtonView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 0.0
    , gravity BOTTOM
    , margin $ Margin 16 0 16 14
    , onClick push $ const NoAction
    ][ linearLayout 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginBottom 32
      ]
      [ (textView $ [
          text $ getString NOTE <> ": "
        , color Color.black900
        ] <> FontStyle.body3 TypoGraphy)
        , (textView $ [
            text $ getVarString NIGHT_TIME_FEE_DESCRIPTION $ singleton state.data.rentalBookingData.nightCharge
        ] <> FontStyle.body3 TypoGraphy)
      ]
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
    ]

dummyView :: forall w.PrestoDOM (Effect Unit) w
dummyView = linearLayout [ width $ V 0, height $ V 0, visibility GONE ] []

separatorView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , margin $ MarginVertical 5 5
    , background Color.grey900
    ]
    []