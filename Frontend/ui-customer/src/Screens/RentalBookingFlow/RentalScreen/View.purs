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
import Components.ChooseVehicle as ChooseVehicle
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
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, color, cornerRadius, gravity, height, id, linearLayout, margin, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textView, weight, width, onBackPressed, visibility, shimmerFrameLayout, accessibility, imageView, imageWithFallback, alignParentBottom)
import Screens.RentalBookingFlow.RentalScreen.ComponentConfig (genericHeaderConfig, incrementDecrementConfig, mapInputViewConfig, primaryButtonConfig, rentalRateCardConfig, locUnserviceablePopUpConfig, rentalPolicyInfoConfig)
import Screens.RentalBookingFlow.RentalScreen.Controller (Action(..), FareBreakupRowType(..), ScreenOutput, eval, dummyRentalQuote)
import Screens.Types (RentalScreenState, RentalScreenStage(..), RentalQuoteList)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import Types.App (GlobalState, defaultGlobalState)
import Styles.Colors as Color
import Services.Backend (getQuotes, rideBooking)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Helpers.Utils (decodeError, fetchImage, FetchImageFrom(..))
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


rentalScreen :: RentalScreenState -> Screen Action RentalScreenState ScreenOutput
rentalScreen initialState =
  { initialState
  , view
  , name: "RentalScreen"
  , globalEvents: [getEstimateEvent]
  , eval:
      \action state -> do
        let _ = spy "RentalScreen action " action
        let _ = spy "RentalScreen state  " state
        eval action state
  }

  where 
    getEstimateEvent push = do 
      when (null initialState.data.rentalsQuoteList) $ do 
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ getRentalQuotes GetRentalQuotes CheckFlowStatusAction 10 1000.0 push initialState
        pure unit
      pure $ pure unit

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
    ] <> if state.props.showRateCard then [RateCard.view (push <<< RateCardAC) (rentalRateCardConfig (maybe dummyRentalQuote identity (fetchSelectedQuote state.data.rentalsQuoteList)))] else []
      <> if state.props.showPopUpModal then [locUnserviceableView push state] else []
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
  , if state.data.currentStage == RENTAL_SELECT_VARIANT then rentalVariantSelectionView push state else emptyTextView
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
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginTop 24
    ][  textView $ 
          [ text "2 hr"
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
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
                    , toolTipId = EHC.getNewIDWithTag "DurationSliderViewToolTip"
                    , progressColor = Color.white900 
                    , thumbColor = Color.blue800
                    , bgColor = Color.white900
                    , bgAlpha = 1000 }
                    
                )(const NoAction)
              ][]
      , textView $ 
          [ text $ show state.props.maxDuration <> " hrs"
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
    ]

rentalVariantSelectionView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
rentalVariantSelectionView push state =
  relativeLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  ][
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
          , color Color.black800
          ] <> FontStyle.subHeading1 TypoGraphy
        , separatorView push state
        ]
    , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingBottom 82
        ]
        [ scrollView[height MATCH_PARENT
        , width MATCH_PARENT
        ][
          linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin $ MarginBottom 32
          , orientation VERTICAL
          ]
          [ if (null state.data.rentalsQuoteList) && state.props.showShimmer then shimmerChooseVehicleView push state
            else if null state.data.rentalsQuoteList then noQuotesErrorModel state 
            else chooseVehicleView push state
          ]]
        
        ]
    ]
  , linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , alignParentBottom "true,-1"
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , stroke $ "1," <> Color.grey900
      , background Color.white900
      , width MATCH_PARENT
      , padding $ Padding 16 16 16 16
      ] [PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
    ]
    ]

chooseVehicleView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
chooseVehicleView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    ( map 
      ( \quote ->
          ChooseVehicle.view (push <<< ChooseVehicleAC) quote.quoteDetails { showInfo = true, showStroke = true}
      ) (state.data.rentalsQuoteList))

fareBreakupView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
fareBreakupView push state = let 
  currency = getCurrency appConfig
  selectedQuote = maybe dummyRentalQuote identity (fetchSelectedQuote state.data.rentalsQuoteList)
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
                ][  linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , margin $ MarginVertical 16 32
                    ] $ map ( 
                        \quote -> let
                          item = quote.quoteDetails
                          in if (item.index == item.activeIndex) then ChooseVehicle.view (\action -> pure unit) (item{showStroke = false, showInfo = false}) else emptyTextView 
                        ) state.data.rentalsQuoteList
                  , linearLayout 
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , margin $ MarginHorizontal 16 16
                    ]
                    (map (\item -> 
                      descriptionView push state (item)
                    ) [BookingTime, BookingDistance, BaseFare, TollFee])
                  , linearLayout 
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation HORIZONTAL
                    , margin $ MarginBottom 32
                    , padding $ PaddingHorizontal 16 16
                    ]
                    [ textView $ [
                        text $ getString NOTE <> ": "
                      , color Color.black900
                      ] <> FontStyle.body3 TypoGraphy
                      , textView $ [
                          text $ getVarString NIGHT_TIME_FEE_DESCRIPTION $ singleton $ currency <> (show selectedQuote.fareDetails.nightShiftCharge)
                        , width MATCH_PARENT
                        , height WRAP_CONTENT
                      ] <> FontStyle.body3 TypoGraphy
                    ]
                  ]
              
              ]
          ]
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
      let baseDuration = show state.data.rentalBookingData.baseDuration
          startTimeUTC = if state.data.startTimeUTC == "" then EHC.getCurrentUTC "" else state.data.startTimeUTC
          activeQuote = head $ filter (\item -> (item.index == item.activeIndex)) state.data.rentalsQuoteList
          selectedQuote = maybe dummyRentalQuote identity (fetchSelectedQuote state.data.rentalsQuoteList)
          currency = getCurrency appConfig
      in case description of
          BookingTime -> if toShowTitle then getString BOOKING_ON <> " " <> EHC.convertUTCtoISC startTimeUTC "Do" <> " " <> EHC.convertUTCtoISC startTimeUTC "MMM" <> ", " <> EHC.convertUTCtoISC startTimeUTC "hh" <> ":" <> EHC.convertUTCtoISC startTimeUTC "mm" <> " " <> EHC.convertUTCtoISC startTimeUTC "a" <> " (" <> baseDuration <> "hrs)" else getString FINAL_FARE_DESCRIPTION
          BookingDistance -> if toShowTitle then getString INCLUDED_KMS <> show state.data.rentalBookingData.baseDistance else getString EXCESS_DISTANCE_CHARGE_DESCRIPTION <> " " <> currency <> (show selectedQuote.fareDetails.perExtraKmRate) <> "/km."
          BaseFare -> if toShowTitle then getString BASE_FARE <> maybe "" (\quote -> quote.quoteDetails.price) activeQuote else getString ADDITIONAL_CHARGES_DESCRIPTION
          TollFee -> if toShowTitle then getString TOLLS_AND_PARKING_FEES else getString PARKING_FEES_AND_TOLLS_NOT_INCLUDED

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


getRentalQuotes :: forall action. (GetQuotesRes -> action) -> action -> Int -> Number -> (action -> Effect Unit) -> RentalScreenState -> Flow GlobalState Unit
getRentalQuotes action flowStatusAction count duration push state = do
  if (state.data.currentStage == RENTAL_SELECT_VARIANT) then
    if (count > 0) then do
      resp <- getQuotes (state.data.searchId)
      _ <- pure $ printLog "caseId" (state.data.searchId)
      case resp of
        Right response -> do
          _ <- pure $ printLog "api Results " response
          let (GetQuotesRes resp) = response
          if not (null resp.quotes) || not (null resp.estimates) then do
            doAff do liftEffect $ push $ action response
            pure unit
          else do
            if (count == 1) then do
              doAff do liftEffect $ push $ action response
            else do
              void $ delay $ Milliseconds duration
              getRentalQuotes action flowStatusAction (count - 1) duration push state
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorMessage"
          if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_ALREADY_PRESENT" ) then do
            void $ pure $ toast "ACTIVE BOOKING ALREADY PRESENT"
            doAff do liftEffect $ push $ flowStatusAction
          else do
            void $ delay $ Milliseconds duration
            if (count == 1) then do
              let response = GetQuotesRes { quotes: [], estimates: [], fromLocation: SearchReqLocationAPIEntity { lat: 0.0, lon: 0.0 }, toLocation: Nothing }
              doAff do liftEffect $ push $ action response
            else do
              getRentalQuotes action flowStatusAction (count - 1) duration push state
    else
      pure unit
  else
    pure unit

shimmerChooseVehicleView :: forall w. (Action -> Effect Unit) -> RentalScreenState -> PrestoDOM (Effect Unit) w
shimmerChooseVehicleView push state = 
  shimmerFrameLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.transparent
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ PaddingHorizontal 16 16
        , orientation VERTICAL
        ](mapWithIndex (\index _ -> 
          shimmerItemView (index == 0)) [1,2,3] )
    ]

  where 
    shimmerItemView :: forall w. Boolean -> PrestoDOM (Effect Unit) w
    shimmerItemView hasTopMargin = 
      linearLayout
        [ width MATCH_PARENT
        , height $ V 80
        , margin $ MarginVertical (if hasTopMargin then 16 else 0)  16 
        , cornerRadius 8.0
        , background Color.greyDark
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