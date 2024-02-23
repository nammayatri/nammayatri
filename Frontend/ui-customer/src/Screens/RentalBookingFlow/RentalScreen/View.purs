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

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader.View as GenericHeader
import Components.IncrementDecrementModel.View as IncrementDecrement
import Components.InputView.View as InputView
import Components.PrimaryButton as PrimaryButton
import Components.RateCard as RateCard
import Components.RequestInfoCard as RequestInfoCard
import Data.Array (singleton, null, mapWithIndex, filter, head)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import ConfigProvider
import Font.Style as FontStyle
import Helpers.CommonView (emptyTextView)
import JBridge (renderSlider, sliderConfig, toast)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Services.API(GetQuotesRes(..), SearchReqLocationAPIEntity(..), RideBookingRes(..))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, color, cornerRadius, gravity, height, id, linearLayout, margin, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textView, weight, width, onBackPressed, visibility, shimmerFrameLayout, accessibility, imageView, imageWithFallback, alignParentBottom, singleLine, ellipsize, clickable, textFromHtml)
import Screens.RentalBookingFlow.RentalScreen.ComponentConfig (genericHeaderConfig, incrementDecrementConfig, mapInputViewConfig, primaryButtonConfig, locUnserviceablePopUpConfig, rentalPolicyInfoConfig)
import Screens.RentalBookingFlow.RentalScreen.Controller (Action(..), FareBreakupRowType(..), ScreenOutput, eval, dummyRentalQuote)
import Screens.Types (RentalScreenState, RentalScreenStage(..))
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import Types.App (GlobalState, defaultGlobalState)
import Styles.Colors as Color
import Services.Backend (getQuotes, rideBooking)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Helpers.Utils (decodeError, fetchImage, FetchImageFrom(..), getVariantDescription, getVehicleName)
import Log (printLog)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Lens ((^.))
import Accessor
import Mobility.Prelude
import Helpers.Utils(fetchAndUpdateCurrentLocation)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config (translateFullYAnimWithDurationConfig)
import Animation (translateYAnimFromTop, fadeInWithDelay)
import Components.PopUpModal.View as PopUpModal
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.String as DS

rentalScreen :: RentalScreenState -> Screen Action RentalScreenState ScreenOutput
rentalScreen initialState =
  { initialState
  , view
  , name: "RentalScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let _ = spy "RentalScreen action " action
        let _ = spy "RentalScreen state  " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  Anim.screenAnimation
    $ relativeLayout
    [ height MATCH_PARENT
    , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom 
    , width MATCH_PARENT
    , onBackPressed push $ const BackpressAction
    , orientation VERTICAL
    , onAnimationEnd (\action -> void $ fetchAndUpdateCurrentLocation push (UpdateLocAndLatLong) NoAction) $ const NoAction
    , background Color.white900
    ] $
    [ getRentalScreenView push state
    ] <> if state.props.showPopUpModal then [locUnserviceableView push state] else []
      <> if state.props.showRentalPolicy then [rentalPolicyExplainerView push state] else []

rentalPolicyExplainerView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
rentalPolicyExplainerView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
  $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][ RequestInfoCard.view (push <<< RequestInfoCardAction) (rentalPolicyInfoConfig state) ]

getRentalScreenView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
getRentalScreenView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ]
  [ if state.data.currentStage == RENTAL_SELECT_PACKAGE then rentalPackageSelectionView push state else emptyTextView
  , if state.data.currentStage == RENTAL_CONFIRMATION then fareBreakupView push state else emptyTextView
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
      [ linearLayout[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginBottom 16
        ][ textView  $ 
            [ text $ getString SELECT_PACKAGE 
            , color Color.black800
            ] <> FontStyle.h1 TypoGraphy
          , imageView
            [ height $ V 16
            , width $ V 16
            , margin $ Margin 4 (if EHC.os == "IOS" then 7 else 12) 0 0
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_black"
            , onClick push $ const RentalPolicyInfo
            ]  
          ]
      , linearLayout [
          height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.squidInkBlue
        , cornerRadius 12.0 
        , orientation VERTICAL
        , padding $ Padding 16 32 16 32
        , margin $ MarginBottom 24
        ]
        [ textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , text $ (show state.data.rentalBookingData.baseDuration) <> " " <> getString HOURS
          , color Color.white900
          ] <> FontStyle.heading TypoGraphy
        , sliderView push state
        ]
      , IncrementDecrement.view (push <<< DistanceIncrementDecrementAC) $ incrementDecrementConfig state
      ]
    , textView $ 
      [ text $ getString (RENTAL_SCREEN_EXPLAINER "10 Km")
      , color Color.black700 
      , margin $ Margin 16 0 16 16
      ] <> FontStyle.paragraphText TypoGraphy
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

sliderView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
sliderView push state = 
  let 
    updateSliderMethodExists = EHC.jBridgeMethodExists "updateSliderValue" 
    prefixText = if updateSliderMethodExists then "-" else "2 hr"
    suffixText = if updateSliderMethodExists then "+" else show state.props.maxDuration <> " hrs"
    buttonPadding = if updateSliderMethodExists then Padding 18 2 18 8 else Padding 0 0 0 0
    fontStyle = if updateSliderMethodExists then FontStyle.h1 TypoGraphy else FontStyle.body1 TypoGraphy
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginTop 24
    ][  textView $ 
          [ text prefixText
          , color Color.white900
          , padding $ buttonPadding
          , clickable updateSliderMethodExists
          , onClick push $ const $ UpdateSliderValue (state.data.rentalBookingData.baseDuration - 1 )
          ] <> fontStyle
      , PrestoAnim.animationSet [Anim.fadeIn true] $
          linearLayout
            [ height $ V 35
            , weight 1.0
            , gravity CENTER
            , id $ EHC.getNewIDWithTag "DurationSliderView"
            , background Color.squidInkBlue
            , onAnimationEnd 
                (\ action -> 
                  void $ renderSlider push SliderCallback 
                    sliderConfig
                    { id = (EHC.getNewIDWithTag "DurationSliderView")
                    , sliderMinValue = state.props.minDuration
                    , sliderMaxValue = state.props.maxDuration
                    , sliderDefaultValue = state.data.rentalBookingData.baseDuration
                    , stepFunctionForCoinConversion = 1
                    , toolTipId = EHC.getNewIDWithTag "DurationSliderViewToolTip"
                    , progressColor = Color.white900 
                    , enableToolTip = false
                    , getCallbackOnProgressChanged = true
                    , thumbColor = Color.blue800
                    , bgColor = Color.white900
                    , bgAlpha = 1000 }
                    
                )(const NoAction)
              ][]
      , textView $ 
          [ text suffixText
          , padding $ buttonPadding
          , clickable updateSliderMethodExists
          , onClick push $ const $ UpdateSliderValue (state.data.rentalBookingData.baseDuration + 1 )
          , color Color.white900
          ] <> fontStyle
    ]

fareBreakupView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
fareBreakupView push state = let 
  currency = getCurrency appConfig
  selectedQuote = maybe dummyRentalQuote identity (state.data.selectedQuote)
  in 
  relativeLayout
    [height MATCH_PARENT
    , width MATCH_PARENT
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , margin $ MarginBottom 80
        ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
          , separatorView push state
          , scrollView
            [ height WRAP_CONTENT
            , background Color.white900
            , width MATCH_PARENT
            ][  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ][  
                 linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , background Color.blue600
                    , cornerRadius 12.0
                    , margin $ Margin 12 16 16 32 
                    , padding $ Padding 8 12 8 12
                    ][  imageView
                        [ imageWithFallback selectedQuote.quoteDetails.vehicleImage
                        , height $ V if selectedQuote.quoteDetails.vehicleVariant == "AUTO_RICKSHAW" then 45 else 48
                        , width $ V 60
                        ]
                      , linearLayout
                        [ weight 1.0
                        , height WRAP_CONTENT
                        , orientation VERTICAL
                        , padding $ PaddingLeft 8
                        ][  textView
                            $ [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , singleLine true
                              , ellipsize true
                              , text $ getVehicleName selectedQuote.quoteDetails.vehicleVariant
                              , color Color.black800
                              ]
                            <> FontStyle.body7 TypoGraphy
                        , capacityView push selectedQuote.quoteDetails 
                        ]
                      , textView
                        $ [ width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , text selectedQuote.quoteDetails.price
                          , color Color.black800
                          ]
                        <> FontStyle.body7 TypoGraphy

                    ]
                  , linearLayout 
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , margin $ Margin 16 0 16 16
                    ]
                    (map (\item -> 
                      descriptionView push state (item)
                    ) [BookingFrom, BookingTime, BookingDistance, BaseFare, TollFee, ParkingCharges, NightTimeFee])
                  ]
              
              ]
          ]
      , noteAndPrimaryButtonView push state
      ]
    where 
    
    capacityView push config = 
      linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , padding $ PaddingTop 5
        , orientation VERTICAL
        ][ vehicleInfoView "ic_user_filled" config.capacity config.vehicleVariant true
        , textView
                $ [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ (getVariantDescription config.vehicleVariant).text
                  , visibility $ boolToVisibility $ DS.length (getVariantDescription config.vehicleVariant).text >= 20
                  , color Color.black700
                  ]
                <> FontStyle.tags TypoGraphy]

    vehicleInfoView imageName description vehicleVariant showAdditionalDesc = let 
      vehicleDesc = getVariantDescription vehicleVariant
      in
      linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        ][ imageView
            [ imageWithFallback $ fetchImage FF_ASSET imageName
            , width $ V 14
            , height $ V 14
            ]
          , textView
              $ [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text description
                , color Color.black700
                ]
              <> FontStyle.tags TypoGraphy
          , linearLayout[
              height $ WRAP_CONTENT
            , gravity CENTER
            , width MATCH_PARENT 
            , visibility $ boolToVisibility $ showAdditionalDesc && DS.length vehicleDesc.text < 20 
            ][  textView
                [ height $ V 3 
                , width $ V 3 
                , cornerRadius 1.5 
                , margin $ MarginHorizontal 2 2
                , background Color.black700
                ]
              , imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_snow"
                , width $ V 12
                , visibility $ boolToVisibility vehicleDesc.airConditioned
                , height $ V 12
                ]
              , textView
                $ [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ vehicleDesc.text
                  , color Color.black700
                  ]
                <> FontStyle.tags TypoGraphy]     
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
      , textFromHtml $ getTitleFromDescription description true
      , color Color.black800
      ] <> if description == BookingFrom then FontStyle.subHeading1 TypoGraphy else FontStyle.body1 TypoGraphy
    , textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , textFromHtml $ getTitleFromDescription description false
      , color Color.black700
      , margin $ MarginTop 4
      , visibility $ boolToVisibility $ description /= BookingFrom
      ] <> FontStyle.paragraphText TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical 11 11
      , visibility $ boolToVisibility $ description /= NightTimeFee
      ]
      [ separatorView push state ]
    ]
  where 
    getTitleFromDescription :: FareBreakupRowType -> Boolean -> String
    getTitleFromDescription description toShowTitle = 
      let baseDuration = show state.data.rentalBookingData.baseDuration
          startTimeUTC = if state.data.startTimeUTC == "" then EHC.getCurrentUTC "" else state.data.startTimeUTC
          activeQuote = head $ filter (\item -> (item.index == item.activeIndex)) state.data.rentalsQuoteList
          selectedQuote = maybe dummyRentalQuote identity (state.data.selectedQuote)
          currency = getCurrency appConfig
          rideEndTime = formatDateInHHMM $ EHC.getUTCAfterNSeconds startTimeUTC $ (state.data.rentalBookingData.baseDuration) * 60 * 60
      in case description of
          BookingFrom -> "<b>Booking from " <> formatDateInHHMM startTimeUTC <> " - " <> rideEndTime <> "</b>"
          BookingTime -> if toShowTitle then "Included Time: <b>" <> show state.data.rentalBookingData.baseDuration <> " hrs</b>" else getString FINAL_FARE_DESCRIPTION
          BookingDistance -> if toShowTitle then "Included Distance: <b>" <> show state.data.rentalBookingData.baseDistance <> " km</b>" else getString EXCESS_DISTANCE_CHARGE_DESCRIPTION <> " " <> currency <> (show selectedQuote.fareDetails.perExtraKmRate) <> "/km."
          BaseFare -> if toShowTitle then "Rental Base Fare: <b>" <> currency <> show selectedQuote.fareDetails.baseFare <> "</b>" else getString ADDITIONAL_CHARGES_DESCRIPTION
          TollFee -> if toShowTitle then "Toll Charges: <b>" <> currency <> "69" <> "</b>" else "Toll Charges are included in the fare."
          ParkingCharges -> if toShowTitle then "Parking and other charges" else getString PARKING_FEES_AND_TOLLS_NOT_INCLUDED
          NightTimeFee -> if toShowTitle then "Night Time Fees" else getVarString NIGHT_TIME_FEE_DESCRIPTION $ singleton $ currency <> state.data.rentalBookingData.nightCharge
    
    formatDateInHHMM :: String -> String
    formatDateInHHMM timeUTC = EHC.convertUTCtoISC timeUTC "hh" <> ":" <> EHC.convertUTCtoISC timeUTC "mm" <> " " <> EHC.convertUTCtoISC timeUTC "a"

noteAndPrimaryButtonView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
noteAndPrimaryButtonView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    , stroke $ "1," <> Color.grey900
    , background Color.white900
    , onClick push $ const NoAction
    ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]

separatorView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , margin $ MarginVertical 5 5
    , background Color.grey900
    ]
    []

noQuotesErrorModel :: forall w . RentalScreenState -> PrestoDOM (Effect Unit) w
noQuotesErrorModel state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background Color.white900
    , accessibility DISABLE
    , margin $ MarginVertical ((EHC.screenHeight unit)/7) 60
    ][ linearLayout
       [ weight 1.0
       , width MATCH_PARENT
       ][]
      , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ][imageView
        [ height $ V 115
        , width $ V 161
        , accessibility DISABLE
        , imageWithFallback $ fetchImage FF_ASSET $ "ny_ic_no_quotes_color"
        ]
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((EHC.screenWidth unit / 2) + (EHC.screenWidth unit /3))
        , color Color.black800
        , text (getString SORRY_WE_COULDNT_FIND_ANY_RIDES)
        , margin $ MarginVertical 20 4
        , gravity CENTER
        ] <> FontStyle.h2 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((EHC.screenWidth unit / 2) + (EHC.screenWidth unit /3))
        , text (getString IT_SEEMS_TO_BE_A_VERY_BUSY_DAY)
        , color Color.black700
        , gravity CENTER
        ] <> FontStyle.paragraphText TypoGraphy
    ]
    ]

locUnserviceableView :: forall w. (Action -> Effect Unit) -> RentalScreenState ->  PrestoDOM (Effect Unit) w
locUnserviceableView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER 
    , background Color.blackLessTrans
    ][ PrestoAnim.animationSet
        [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ]  $ 
        PopUpModal.view (push <<< PopUpModalAC) (locUnserviceablePopUpConfig state) ]

fetchSelectedQuote rentalsQuoteList = head $ filter (\item -> item.activeIndex == item.index) rentalsQuoteList