module Screens.RideBookingFlow.RiderRideCompletedCard.View where

import Screens.RideBookingFlow.RiderRideCompletedCard.Controller
import Screens.RideBookingFlow.RiderRideCompletedCard.Config

import PrestoDOM 
import Components.Banner.View as Banner
import Components.Banner as BannerConfig
import Data.Functor (map)
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn,fadeInWithDelay) as Anim
import Effect (Effect)
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (+), (<<<), (<>), (==), (>), (>=), (||), (<$>), (<=), show, void, (/=), when, max, mod )
import Common.Styles.Colors as Color
import Components.SelectListModal as CancelRidePopUp
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Data.Array (mapWithIndex, length, (!!), null, any, (..), head)
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime, safeMarginTop, screenHeight, getNewIDWithTag, liftFlow)
import Components.PrimaryButton as PrimaryButton
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties (cornerRadii)
import Language.Types (STR(..))
import Common.Types.App 
import Font.Style as FontStyle
import Font.Size as FontSize
import Halogen.VDom.DOM.Prop (Prop)
import Components.PopUpModal as PopUpModal
import JBridge as JB
import Data.Function.Uncurried (runFn1)
import Mobility.Prelude
import ConfigProvider
import Mobility.Prelude (boolToVisibility, layoutWithWeight)
import Engineering.Helpers.Commons as EHC
import Data.Maybe
import JBridge(renderBase64Image)
import Storage (getValueToLocalStore, KeyStore(..))
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Engineering.Helpers.Utils as Utils
import Data.Number (fromString)
import Data.Int (ceil)
import PrestoDOM.List
import CarouselHolder as CarouselHolder
import Language.Strings (getString)
import Debug
import Data.String as DS
import Common.Types.App (FeedbackAnswer)
import Data.Array (filter, elem)
import Screens.Types (RiderRideCompletedScreenState, AdditionalCharges(..), FareProductType(..))
import Components.RideCompletedCard.Controller (CustomerIssue(..))
import Components.RatingCard.Controller (FeedbackItem(..))
import Debug (spy)
import Effect.Uncurried (runEffectFn2)
import Data.Tuple (Tuple(..))
import PrestoDOM.Elements.Keyed as Keyed
import Engineering.Helpers.Commons (os)
import Animation (screenAnimation)
import Components.FavouriteDriverInfoCard.FavouriteDriverInfoCard as FavouriteDriverInfoCard
import Components.FavouriteDriverInfoCard.Controller as FavouriteDriverInfoCardController
import Resources.LocalizableV2.Strings (getEN)
import Components.BannerCarousel as BannerCarousel
import Effect.Aff (Milliseconds(..), launchAff)
import Presto.Core.Types.Language.Flow (callAPI, APIResult(..), Flow)
import Types.App (defaultGlobalState, GlobalState(..))
import Control.Monad.Except.Trans (lift)
import Components.RideCompletedCard as RideCompletedCard

screen :: RiderRideCompletedScreenState -> Screen Action RiderRideCompletedScreenState ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "RiderRideCompletedScreen"
    , globalEvents: [(\push ->  globalEvents' push initialState)]
    , eval:( \action state -> do
          let
            _ = spy "RiderRideCompletedScreen action" action
            _ = spy "RiderRideCompletedScreen state" state
          void $ case action of 
            FeedbackChanged _ _ -> pure unit
            KeyboardCallback _ -> pure unit
            _ -> do 
                when(state.isKeyBoardOpen) $ void $ pure $ JB.hideKeyboardOnNavigation true
                pure unit
          eval action state
        ) 
    }
    where
    globalEvents' :: (Action -> Effect Unit) -> RiderRideCompletedScreenState -> Effect (Effect Unit)
    globalEvents' push state = do 
      void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
      void $ runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
      when (isNothing state.customerIssue.bannerComputedView) $ void $ launchAff $ flowRunner defaultGlobalState $ computeIssueReportBanners push
      pure $ pure unit

computeIssueReportBanners :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeIssueReportBanners push = do
  bannerItem <- preComputeListItem $ RideCompletedCard.customerIssueCarousalView (push <<< RideCompletedAC) 
  void $ liftFlow $ push $ SetIssueReportBannerItems bannerItem

view :: forall w.  (Action -> Effect Unit) -> RiderRideCompletedScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let pading = Padding 0 EHC.safeMarginTop 0 (if (state.isKeyBoardOpen) then 0 else EHC.safeMarginBottom)
  in
  screenAnimation 
    $ relativeLayout[
          width MATCH_PARENT
        , height MATCH_PARENT
        , padding $ pading
        , background Color.white900
        ]$[
      scrollView
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , clickable true
      , background Color.white900
      , fillViewport true
      ] $ [  
          linearLayout[
            width MATCH_PARENT
          , height $ V $ (EHC.screenHeight unit - (EHC.safeMarginTop + EHC.safeMarginBottom))
          ][rideCompletedView state push]
      ]
    ] <> (if state.isRatingCard then 
          [
            linearLayout[
                width MATCH_PARENT
              , height MATCH_PARENT
              , orientation VERTICAL][
                  linearLayout[
                    width MATCH_PARENT
                  , weight 1.0
                  ][
                    rideRatingView state push
                  ]
                , stickyButtonView state push
              ]
            ]
          else [] )
          <> (if state.favDriverInfoCard then
            [
              linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , background Color.white900
                ][FavouriteDriverInfoCard.view (push <<< DriverInfocardAC) (FavouriteDriverInfoCardController.config)]
            ]
          else [])

rideCompletedView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideCompletedView config push = 
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility $ boolToVisibility $ not config.isRatingCard
      ][ topGradientView config push 
      ,  bottomCardView config push
    ]

topGradientView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topGradientView config push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ PaddingVertical 30 6 
    , background $ Color.blue600
    , id $ getNewIDWithTag "topViewId"
    , visibility $ boolToVisibility $ not config.isRatingCard
    ]
    [   
        priceAndDistanceUpdateView config push 
      , whiteHorizontalLine config
      , rideDetailsButtonView config push 
    ]

priceAndDistanceUpdateView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
priceAndDistanceUpdateView state push = 
  let rideDuration = fromMaybe 0 state.rideDuration
      sec = if rideDuration >= 3600 then 0 else if rideDuration < 60 then rideDuration else rideDuration `mod` 60
      min = if rideDuration >= 60 then 
              if sec /= 0 then 
                rideDuration/60
              else (rideDuration/60) `mod` 60
            else 0
      hour = if rideDuration >= 3600 then rideDuration/3600 else 0
      timeString = (
        if hour == 0 then 
          if min == 0 then 
            show sec <> " " <> getEN SEC 
          else 
            show min <> getEN MINS <> " " <> show sec <> " " <> getEN SEC 
        else 
          show hour <> " " <> getEN HOUR <> " " <> show min <> getEN MINS
        )
  in
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , layoutGravity "center_vertical"
      ][
          linearLayout[
            width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          ][
              layoutWithWeight
            , imageView
                [ width $ V 55
                , height $ V 47
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_tick"
                , margin $ MarginLeft $ if state.showSafetyCenter then 80 else 0
                ]
            , layoutWithWeight
            , sosButtonView state push 
          ]
        , textView $
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , text $ getString $ if state.driverInfoCardState.fareProductType == DELIVERY then DELIVERED_IN_JUST timeString else REACHED_DESTINATION timeString
          , gravity CENTER
          , weight 1.0
          , color Color.black800
          , singleLine false
          , margin $ Margin 75 12 75 0
          ] <> FontStyle.h3 TypoGraphy 
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , margin $ MarginHorizontal 16 16
          ][ textView $ 
              [ text if state.isFreeRide then "₹0" else "₹" <> (show state.topCard.finalAmount)
              , accessibilityHint $ "Ride Complete: Final Fare ₹"  <> (show state.topCard.finalAmount)
              , accessibility state.accessibility
              , color Color.black900
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              ] <> (FontStyle.body28 TypoGraphy)
          , textView $
              [ textFromHtml $ "<strike> ₹" <> (show state.topCard.initialAmount) <> "</strike>"
              , accessibilityHint $ "Your Fare Has Been Updated From ₹"  <> (show state.topCard.initialAmount) <> " To ₹" <> (show state.topCard.finalAmount)
              , accessibility state.accessibility
              , margin $ Margin 8 5 0 0
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.black700
              , visibility if state.topCard.fareUpdatedVisiblity then VISIBLE else GONE
              ] <> (FontStyle.title1 TypoGraphy)
          ]
        , linearLayout [
            height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          , margin $ MarginTop 10
          ] $ additionalChargesView <$> state.additionalCharges
        , pillView state push
      ]

additionalChargesView :: forall w. AdditionalCharges -> PrestoDOM (Effect Unit) w
additionalChargesView config = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginBottom 5
  , visibility config.visibility
  ][
    imageView[
      height $ V 20
    , width $ V 20
    , imageWithFallback config.image 
    ]
  , textView $
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text config.text 
    , color config.textColor
    , margin $ MarginLeft 4
    ] <> FontStyle.body1 TypoGraphy
  ]

pillView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pillView state push =
  linearLayout
    [ width MATCH_PARENT 
    , height WRAP_CONTENT
    , padding $ Padding 16 8 8 8
    , margin $ Margin 15 15 15 5
    , background "#E5ECFF"
    , alpha 1.0
    , cornerRadius 8.0
    , gravity CENTER
    , visibility state.topCard.infoPill.visible
    ]
    [ imageView
        [ width $ V 18
        , height $ V 18
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_parallel_arrows"
        , visibility state.topCard.infoPill.imageVis
        , margin $ MarginRight 10
        ]
    , textView $
        [ height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , text $ state.topCard.infoPill.text
        , color $ Color.black700
        , weight 1.0
        ] <> (FontStyle.tags LanguageStyle)
    ]

rideDetailsButtonView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideDetailsButtonView state push = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT 
  , layoutGravity "bottom"
  , padding $ PaddingHorizontal 16 16
  ][
    linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , onClick push $ const RideDetails
          , accessibility ENABLE
          , accessibilityHint "Ride Details : Button"
          , cornerRadius 4.0
          , padding $ Padding 4 16 4 16
           , rippleColor Color.rippleShade
          ][  textView $
              [ height WRAP_CONTENT
              , text $ getString $ if state.driverInfoCardState.fareProductType == DELIVERY then DELIVERY_DETAILS else RIDE_DETAILS
              , color Color.black800
              , weight 1.0
              ] <> (FontStyle.body2 TypoGraphy)
            , imageView
              [ width $ V 18
              , height $ V 18
              , accessibility DISABLE
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_grey"
              ]
          ]
  ]

-------------------------------- Top Card Ends --------------------------------------------------------------------------------------------------------------------------------------------

bottomCardView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomCardView state push =
  let customerIssue :: CustomerIssue
      customerIssue = {
        currentIndex : state.customerIssue.currentPageIndex
      , currentPageIndex : state.customerIssue.currentPageIndex
      , bannerComputedView : state.customerIssue.bannerComputedView
      , customerIssueCards : issueReportBannerConfigs state
      , showIssueBanners : state.customerIssue.showIssueBanners
      }
  in
  linearLayout
  [ width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 12 16 0
  , weight 1.0
  , alignParentBottom "true,-1"
  , background Color.white900
  ] [
      if state.showRentalRideDetails then rentalTripDetailsViewWrapper state push else customerIssueView push customerIssue state,
      rideCustomerExperienceView state push
    ]

---------------------------------------------------- customerIssueView ------------------------------------------------------------------------------------------------------------------------------------------
customerIssueView :: forall w. (Action -> Effect Unit) -> CustomerIssue -> RiderRideCompletedScreenState -> PrestoDOM (Effect Unit) w
customerIssueView push config state = 
  case config.bannerComputedView , config.showIssueBanners, (null config.customerIssueCards) of 
    Just listView , true, false ->
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , weight 1.0
      ][
        if length config.customerIssueCards == 1 then customerIssuePopupView push config else customerIssueCarouselView listView push config ,
        linearLayout[weight 1.0][] ,
        linearLayout[
            width MATCH_PARENT
          , height WRAP_CONTENT
          , cornerRadii $ Corners 16.0 true true false false
          , background Color.white900
          , padding $ Padding 16 16 16 16
          , adjustViewWithKeyboard "true"
          ][PrimaryButton.view (push <<< PrimaryButtonCarousel) (primaryButtonConfigForCarousel state)]
      ]
    _,_,_-> textView[
        width $ V 0
      , height $ V 0
      ] 


customerIssueCarouselView :: forall w. ListItem -> (Action -> Effect Unit) -> CustomerIssue ->  PrestoDOM (Effect Unit) w
customerIssueCarouselView listView push config  = 
    linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ] [
        linearLayout[
          width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey800
        ][CarouselHolder.carouselView push $ getCarouselConfig listView config]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER
        , margin $ MarginTop 8
        , visibility $ if length config.customerIssueCards > 1 then VISIBLE else GONE
        ] $ mapWithIndex (\ idx elem -> 
            linearLayout[
              height $ V $ if idx == config.currentIndex then 8 else 6
            , width $ V $ if idx == config.currentIndex then 8 else 6
            , cornerRadius $ if idx == config.currentIndex then 4.0 else 3.0
            , background $ if idx == config.currentIndex then Color.black900 else Color.rippleShade
            , margin $ if idx > 0 then MarginLeft 4 else MarginLeft 0
            ] []
          ) $ 0 .. ((length config.customerIssueCards) -1)
      ]

customerIssuePopupView :: forall w. (Action -> Effect Unit) ->  CustomerIssue -> PrestoDOM (Effect Unit) w
customerIssuePopupView  push config = 
  let 
    cardsArray = config.customerIssueCards
    firstCard  = head cardsArray 
    title      = maybe "" (\x -> x.title) firstCard
    selectedYes = maybe Nothing (\x -> x.selectedYes) firstCard
    yestext    = maybe "" (\x -> x.yesText) firstCard
    notext     = maybe "" (\x -> x.noText) firstCard
    subtitle   = maybe "" (\x -> x.subTitle) firstCard
  in
  linearLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 16 24 16 24
  , cornerRadius 6.0
  , backgroundColor Color.white900
  , orientation VERTICAL
  , stroke $ "1,"<>Color.grey800
  ][
    textView $ 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text title
    , color Color.black800 
    , gravity CENTER
    , padding $ PaddingBottom 4
    ] <> FontStyle.h3 TypoGraphy
  , textView $ 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text subtitle 
    , color  Color.black700
    , gravity CENTER
    ] <> FontStyle.paragraphText TypoGraphy 
  , linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ MarginTop 16
    ][
      linearLayout
      [ height WRAP_CONTENT
      , weight 1.0
      , gravity CENTER
      , padding $ Padding 1 1 1 1
      , cornerRadius 6.0
      , background $ if selectedYes == Just true then Color.blue900 else Color.grey800
      ][
        linearLayout
        [ height WRAP_CONTENT
        ,  weight 1.0
        , gravity CENTER
        , padding $ PaddingVertical 14 14
        , onClick push $ const $ SelectButton true 0
        , accessibility ENABLE
        , accessibilityHint $ if selectedYes == Just true then  "Yes : selected" else "Yes : unselected "
        , cornerRadius 6.0
        , background $ if selectedYes == Just true then Color.blue600 else Color.white900
        ][
          textView $ 
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ yestext 
          , color Color.black800
          ] <> FontStyle.subHeading1 TypoGraphy
        ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , weight 1.0
      , gravity CENTER
      , padding $ Padding 1 1 1 1
      , margin $ MarginLeft 10
      , cornerRadius 6.0
      , background $ if selectedYes == Just false then Color.blue900 else Color.grey800
      ][
        linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , gravity CENTER
        , onClick push $ const $ SelectButton false 0
        , padding $ PaddingVertical 14 14
        , accessibility ENABLE
        , accessibilityHint $  if selectedYes == Just false then  "No : Selected" else "No : Unselected "
        , cornerRadius 6.0
        , background $ if selectedYes == Just false then Color.blue600 else Color.white900
        ][
          textView $ 
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ notext
          , color   Color.black800 
          ] <> FontStyle.subHeading1 TypoGraphy
        ]
      ]
    ]
  ]

---------------------------------------------- (Driver Card 7) rentalRideDetailsView  ------------------------------------------------------------------------------------------------------------------------------------------
rentalTripDetailsViewWrapper :: forall w . RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rentalTripDetailsViewWrapper config push =
  linearLayout
    [ weight 1.0
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ rentalTripDetailsView config push
    , linearLayout[weight 1.0][] 
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadii $ Corners 16.0 true true false false
      , background Color.white900
      , padding $ Padding 16 16 16 16
      , adjustViewWithKeyboard "true"
      ][PrimaryButton.view (push <<< PrimaryBtnRentalTripDetailsAC) (primaryBtnConfigForRentalTripDetails config)]
    ]

rentalTripDetailsView :: forall w . RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rentalTripDetailsView config push =
  let rentalRowDetails = config.rentalRowDetails
      fareDiff = config.topCard.finalAmount - config.topCard.initialAmount
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ textView $
      [ text rentalRowDetails.rideDetailsTitle
      , color Color.black800
      , margin $ MarginVertical 4 8
      ] <> FontStyle.body1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 9.0
      , background Color.white900
      , padding $ Padding 16 16 16 16
      , stroke $ "1,"<> Color.grey900
      , orientation VERTICAL
      ] 
      [ 
        rentalTripRowView config push RideStartedAt
      , rentalTripRowView config push RideEndedAt
      ]
    , textView $
      [ text rentalRowDetails.fareUpdateTitle
      , color Color.black800
      , visibility $ boolToVisibility (fareDiff > 0)
      , margin $ MarginVertical 24 8
      ] <> FontStyle.body1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 9.0
      , background Color.white900
      , padding $ Padding 16 16 16 16
      , stroke $ "1,"<> Color.grey900
      , orientation VERTICAL
      , visibility $ boolToVisibility (fareDiff > 0)
      ]
      [ rentalTripRowView config push EstimatedFare
      , rentalTripRowView config push ExtraTimeFare
      , rentalTripRowView config push ExtraDistanceFare
      , rentalTripRowView config push Surcharges
      , separatorView
      , rentalTripRowView config push TotalFare
      ]
    ]
  
  where 
    separatorView = 
      linearLayout
      [ height $ V 1
      , width MATCH_PARENT 
      , margin $ MarginTop 16
      , background Color.grey700
      ][]

rentalTripRowView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> RentalRowView -> PrestoDOM (Effect Unit) w
rentalTripRowView config push description =
  let rentalBookingData = config.rentalBookingData
      textConfig = getTextConfig config description
  in 
    linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginTop if any ( _ == description) [EstimatedFare, RideStartedAt] then 0 else 16
    , visibility $ boolToVisibility (textConfig.estimatedValue /= "₹0")
    ] 
    [ linearLayout
      [ height WRAP_CONTENT
      , weight 0.1
      , orientation VERTICAL 
      ][  textView $ [
            text $ textConfig.title
          , gravity LEFT
          ] <> FontStyle.body1 TypoGraphy
        , textView $ [
            text $ textConfig.subTitle
          , gravity LEFT 
          , visibility $ boolToVisibility (textConfig.subTitle /= "")
          ] <> FontStyle.body1 TypoGraphy
        ]
    , linearLayout [
        height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity RIGHT
      ] 
      [ textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , ellipsize true
        , singleLine true
        , text $ textConfig.actualValue
        , color $ textConfig.color
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ textConfig.estimatedValue
        , color Color.black700
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]
    where
      getTextConfig :: RiderRideCompletedScreenState -> RentalRowView -> RentalTextConfig
      getTextConfig config' description' = 
        let rentalBookingData = config'.rentalBookingData
            rentalRowDetails = config'.rentalRowDetails
            extraTimeFare = ceil ( fromMaybe 0.0 (fromString rentalBookingData.extraTimeFare))
            extraDistFare = ceil ( fromMaybe 0.0 (fromString rentalBookingData.extraDistanceFare))
        in
          case description' of
            RideTime -> mkRentalTextConfig rentalRowDetails.rideTime  "" (" / " <> show rentalBookingData.baseDuration <> "hr") ( if rentalBookingData.finalDuration == 0 then "0 hr" else Utils.formatMinIntoHoursMins rentalBookingData.finalDuration) (showRedOrBlackColor ((rentalBookingData.finalDuration / 60) > rentalBookingData.baseDuration))
            RideDistance -> mkRentalTextConfig rentalRowDetails.rideDistance rentalRowDetails.rideDistanceInfo (" / " <> show rentalBookingData.baseDistance <> "km") (show rentalBookingData.finalDistance <> " km") (showRedOrBlackColor (rentalBookingData.finalDistance > rentalBookingData.baseDistance))
            RideStartedAt -> mkRentalTextConfig rentalRowDetails.rideStartedAt "" rentalBookingData.rideStartedAt "" Color.black600
            RideEndedAt -> mkRentalTextConfig rentalRowDetails.rideEndedAt "" rentalBookingData.rideEndedAt "" Color.black600
            EstimatedFare -> mkRentalTextConfig rentalRowDetails.estimatedFare "" ("₹" <> show config'.topCard.initialAmount) "" Color.black600
            ExtraTimeFare -> mkRentalTextConfig rentalRowDetails.extraTimeFare "" ("₹" <> show extraTimeFare) "" Color.black600
            ExtraDistanceFare -> mkRentalTextConfig rentalRowDetails.extraDistanceFare "" ("₹" <> show extraDistFare ) "" Color.black600
            TotalFare -> mkRentalTextConfig rentalRowDetails.totalFare "" ("₹" <> show config'.topCard.finalAmount) "" Color.black600
            Surcharges -> mkRentalTextConfig rentalRowDetails.surcharges "" ("₹" <> show (config'.topCard.finalAmount - (config'.topCard.initialAmount + extraDistFare + extraTimeFare))) "" Color.black600

            
      mkRentalTextConfig :: String -> String -> String -> String -> String -> RentalTextConfig
      mkRentalTextConfig title' subTitle' estimatedValue' actualValue' color' = { title: title', subTitle: subTitle', estimatedValue: estimatedValue', actualValue: actualValue', color: color'}
      showRedOrBlackColor :: Boolean -> String
      showRedOrBlackColor isRed =
        if isRed then Color.red else Color.black900

rideCustomerExperienceView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideCustomerExperienceView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , weight 1.0
  , visibility $ boolToVisibility $ not $ state.customerIssue.showIssueBanners || state.showRentalRideDetails
  ]
  [
    linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginTop 10
      ][
        needHelpPillView state push 
      ]
  , customerRatingDriverView state push
  , linearLayout[weight 1.0][]
  , linearLayout [ gravity CENTER, width MATCH_PARENT, height WRAP_CONTENT][
      skipButton push
    ]
  ]

------------------------------------------- customerRatingDriverView -------------------------------------------------------------------------------------------------------------------------------------------------------------
customerRatingDriverView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerRatingDriverView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey800
  , padding $ Padding 10 0 10 20
  , gravity CENTER
  ][
      commonTextView state push (if state.driverInfoCardState.fareProductType == DELIVERY then getString RATE_YOUR_DELIVERY_WITH <> " " <> state.driverInfoCardState.driverName else getString HOW'S_TRIP) "#14171F" (FontStyle.h3 TypoGraphy) 10
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , margin $ MarginTop 15
      ](mapWithIndex (\_ item ->
          linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , margin $ MarginHorizontal 5 5
          , onClick push $ const $ RateClick item
          ][imageView
              [ height $ V 30
              , width $ V 30
              , accessibilityHint $ (show item <> "star" ) <> if item <= state.ratingCard.rating then " : Selected " else " : Un Selected "
              , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item <= state.ratingCard.rating then "ny_ic_yellowstar_active" else "ny_ic_yellowstar_inactive"
              ]
          ]) [1,2,3,4,5])
  ]

needHelpPillView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
needHelpPillView state push = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , padding $ Padding 10 12 10 12
    , cornerRadius if os == "IOS" then 20.0 else 24.0
    , onClick push $ const $ HelpAndSupportAC
    , margin $ MarginVertical 0 5
    , rippleColor Color.rippleShade
    , accessibility ENABLE
    , accessibilityHint $ "Need help : Button"
    ][imageView [
        width $ V 16
      , height $ V 16
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blackheadphones"
      , margin $ MarginRight 8
      ]
     , textView $ [
        text $ getString NEED_HELP 
      , color Color.black800
      ] <> FontStyle.paragraphText TypoGraphy
    ]

stickyButtonView ::  forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
stickyButtonView state push =
  screenAnimation 
    $ linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadii $ Corners 16.0 true true false false
      , background Color.white900
      , padding $ Padding 16 0 16 0
      , adjustViewWithKeyboard "true"
      ][PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)]

skipButton :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
skipButton push =
  linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 20
    ][
      textView $ [
      width MATCH_PARENT
    , text $ getString SKIP
    , color "#000000"
    , gravity CENTER_HORIZONTAL
    , padding $ PaddingBottom 35
    , onClick push $ const Skip
    ] <> FontStyle.subHeading1 TypoGraphy
    ]

--------------------------------- Helpers ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
commonTextView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> PrestoDOM (Effect Unit) w
commonTextView state push text' color' fontStyle marginTop =
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity CENTER
  , margin $ MarginTop marginTop
  ] <> fontStyle


horizontalLine :: forall w. Margin -> String -> PrestoDOM (Effect Unit) w 
horizontalLine margin' color = 
  linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background color
          , margin margin'
          ,gravity CENTER
          ][] 

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  linearLayout
    [ width WRAP_CONTENT
    , height $ V 0
    ][]

whiteHorizontalLine :: forall w . RiderRideCompletedScreenState -> PrestoDOM (Effect Unit) w
whiteHorizontalLine config = 
  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , margin $ Margin 16 30 16 0
    , background Color.grey900
    ][]

getBottomCardHeight :: String -> Length 
getBottomCardHeight id = V $ (screenHeight unit) - (runFn1 JB.getLayoutBounds $ getNewIDWithTag id).height - 82

sosButtonView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sosButtonView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity RIGHT
  , cornerRadius if os == "IOS" then 18.0 else 25.0
  , background Color.blue900
  , padding $ Padding 8 8 8 8
  , margin $ MarginRight 18
  , onClick push $ const GoToSOS
  , visibility $ boolToVisibility $ config.showSafetyCenter
  ]
  [ imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_shield_blue"
      , height $ V 24
      , width $ V 24
      , accessibilityHint $ "S O S Button, Select to view S O S options"
      , accessibility ENABLE
      ]
  ]

rideRatingView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideRatingView state push = 
  screenAnimation 
    $ scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , clickable true
        , background Color.white900
        , fillViewport true
        , id (getNewIDWithTag "RideCompletedScrollView")
        ] $ [  
            linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , visibility $ boolToVisibility state.isRatingCard
              , onBackPressed push $ const Back
              , adjustViewWithKeyboard "true"
              , padding $ PaddingBottom 40
              ][ 
                topPartView state push 
              , bottomPartView state push
              ]   
        ]

topPartView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topPartView state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background $ "#F4F7FF"
    ]
    [  
      header state push
    , profile state push 
    , driverRating state push
    , favComponent state push
    ]

header :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ if os == "IOS" then Padding 15 20 10 6 else Padding 15 10 10 6
    , orientation HORIZONTAL
    ]
    [
      imageView
        [ width $ V 28
        , height $ V 28
        , margin $ MarginTop 5
        , onClick push $ const $ Back
        , imageWithFallback $ fetchImage FF_ASSET "jp_toolbarbackarrow_1_2c2f3a_2023_06_23_18_36_07"
        ]
    ]

profile :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
profile state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ PaddingHorizontal 15 10
    , orientation HORIZONTAL
    ]
    [
      imageView
        [ width $ V 68
        , height $ V 68
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_profile_image"
        ]
    , linearLayout
      [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ Margin 6 15 0 0
      ][
        linearLayout
        [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ MarginLeft 2
        , onClick push $ const GoToDriverProfile
        ][
          textView
            $ [ 
                text $ state.driverInfoCardState.driverName
              , color Color.black900
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , imageView
          [ width $ V 20
          , height $ V 20
          , visibility $ boolToVisibility $ state.config.riderRideCompletedCard.showDriverProfile
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
          , margin $ if os == "IOS" then Margin 2 0 0 0 else Margin 2 2 0 0
          ]
        ] 
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , background "#E5ECFF"
          , cornerRadius 50.0
          , padding (Padding 7 0 7 3)
          , margin $ MarginTop 5
          , visibility $ boolToVisibility $ state.driverInfoCardState.favCount > 0
          ][
            imageView [
              imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_heart"
              , height $ V 15
              , width $ V 15
              , visibility VISIBLE 
              , margin $ MarginTop 2
            ]
          , textView $
              [ 
                accessibilityHint $ "liked by customers"
              , text $ getString BY <> " " <> show state.driverInfoCardState.favCount <> " " <> if state.driverInfoCardState.favCount > 1 then getString CUSTOMERS else getString CUSTOMER
              , color Color.blue800
              , maxLines 1
              , gravity CENTER
              , margin $ Margin 4 0 2 0
              ] <> FontStyle.tags LanguageStyle
          ]
      ]
    ]

starRatingView :: forall w . RiderRideCompletedScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
starRatingView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginLeft 8
    ][ 
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ](mapWithIndex (\index item ->
            linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , margin (MarginHorizontal 5 5)
            , onClick push $ const $ RateClick item
            ][imageView
                [ height $ V 22
                , width $ V 22
                , accessibilityHint (show item <> " Star : " <> (if item <= state.ratingCard.rating then "Selected" else "Un Selected") )
                , imageWithFallback  $ fetchImage FF_COMMON_ASSET $ if item <= state.ratingCard.rating then "ny_ic_yellowstar_active" else "ny_ic_yellowstar_inactive"
                ]
            ]) [1,2,3,4,5])
    ]

driverRating :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverRating state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ PaddingHorizontal 18 10
    , margin $ MarginVertical 18 15
    , orientation HORIZONTAL
    ][
      textView $
        [ 
          accessibilityHint $ "Rating given by customer"
        , text  $ getString YOU_RATED <> ":"
        , color Color.black700
        , maxLines 1
        , margin $ MarginTop 2
        ] <> FontStyle.body3 LanguageStyle
    , starRatingView state push
    ]

favComponent :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
favComponent state push = 
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding $ Padding 10 10 10 10
      , margin $ Margin 18 0 24 15
      , orientation HORIZONTAL
      , background $ Color.white900
      , cornerRadius 12.0
      , visibility $ boolToVisibility $ state.driverInfoCardState.isAlreadyFav
      , gravity CENTER_VERTICAL
      ][
        textView $
          [ 
            height WRAP_CONTENT
          , accessibilityHint $ "Your Favourite Driver"
          , text $ getString YOU_FAVOURITED <> " " <> state.rideRatingState.driverName
          , color $ "#2F2935"
          , maxLines 1
          , fontWeight $ FontWeight 400
          , textSize $ FontSize.a_14
          , lineHeight "14"
          ]
      , linearLayout[weight 1.0][]
      , imageView [
            imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_bluecircle_white_heart"
            , height $ V 35
            , width $ V 35
            , visibility VISIBLE 
          ]
      ]

bottomPartView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomPartView state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 19 25 16 50
    ]
    [
      feedbackPills state push
    , riderFeedback state push
    , title state
    , relativeLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , visibility $ boolToVisibility $ state.driverInfoCardState.isAlreadyFav || state.ratingCard.favDriver || state.ratingCard.rating < 4
      ][
        editTextView state push
      , audioRecorder state push
      ]
    ]

feedbackPillView :: forall w. RiderRideCompletedScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
feedbackPillView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 15
    ](map  
      (\list1 ->  
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , margin $ MarginBottom 6
        ](map 
            (\item -> 
              let isSelected = checkPillSelected item.text state.ratingCard.feedbackList item.id
              in
                linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , cornerRadius 20.0
                  , stroke ("1," <> if isSelected then Color.blue900 else Color.grey900)
                  , margin $ Margin 6 6 6 6
                  , background if isSelected then Color.blue600 else Color.white900
                  , onClick push $ const $ SelectPill item.text item.id
                  ][ textView
                      [ height WRAP_CONTENT
                      , textSize FontSize.a_12
                      , fontStyle $ FontStyle.medium LanguageStyle
                      , text item.text
                      , accessibilityHint $ item.text <> if isSelected then " : Selected" else " : Un Selected"
                      , color if isSelected then Color.blue900 else Color.black800
                      , padding $ Padding 12 12 12 12
                      ]
                  ]
            )list1
          )
      ) (getFeedbackPillData state.ratingCard.rating state.ratingCard.feedbackPillData)
    ) 

getFeedbackPillData :: Int -> Array (Array (Array FeedbackItem)) -> Array (Array FeedbackItem)
getFeedbackPillData rating feedbackPillData = fromMaybe [] $ (feedbackPillData) !! (rating - 1)

checkPillSelected :: String -> Array FeedbackAnswer -> String -> Boolean
checkPillSelected feedbackItem feedbackList itemId =
  let
    selectedItems = filter (\item -> item.questionId == itemId) feedbackList
  in
    any (\item -> feedbackItem `elem` item.answer) selectedItems

feedbackPills :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
feedbackPills state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][
      textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , accessibilityHint $ "FeedBack pills"
        , text  $ getString PROVIDED_FEEDBACK
        , maxLines 1
        , color $ Color.black
        ] <> FontStyle.subHeading1 LanguageStyle
    , feedbackPillView state push
    ]

riderFeedback :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
riderFeedback state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ MarginTop 30
    , visibility $ boolToVisibility $ (state.ratingCard.rating > 3 && not state.driverInfoCardState.isAlreadyFav)
    ][
      linearLayout[
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , weight 1.0
      , margin $ MarginTop 10
      ][
        linearLayout[
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][
          textView $ [ 
            text  $ getString FAVOURITE_DRIVER
            , color Color.black
            , maxLines 1
            ] <> FontStyle.subHeading1 LanguageStyle
        , imageView [
            imageWithFallback $ fetchImage FF_COMMON_ASSET $ "ny_ic_info"
            , height $ V 15
            , width $ V 15
            , visibility VISIBLE 
            , margin $ Margin 10 5 0 0
            , onClick push $ const DriverInfoCard
          ]
         ]
      , textView $ [ 
          text  $ getString PREFER_DRIVER
          , color Color.black700
          , maxLines 2
          , margin $ MarginTop 3
          ] <> FontStyle.body3 LanguageStyle
      ]
    , linearLayout[
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity $ RIGHT
      ][
        imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.ratingCard.favDriver == false then "ny_ic_favourite_driver_unselected" else "ny_ic_favourite_driver_selected"
          , height $ V 60
          , width $ V 60
          , visibility VISIBLE 
          , onClick push $ const MakeFavourite
        ]
      ]
    ]

title :: forall w. RiderRideCompletedScreenState -> PrestoDOM (Effect Unit) w
title state = 
  textView $ [ 
    text $ getString WRITE_REVIEW
    , color Color.black
    , maxLines 1
    , margin $ MarginTop 20
    , visibility $ boolToVisibility $ not state.ratingCard.favDriver && not state.recordedView && (state.driverInfoCardState.isAlreadyFav || state.ratingCard.rating < 4)
    ] <> FontStyle.subHeading1 LanguageStyle


editTextView :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout
  [ height $ V 66
  , width MATCH_PARENT
  , background Color.blue600
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ Margin 0 20 0 24
  , padding $ Padding 16 0 16 16
  , stroke $ "1," <> Color.grey800
  , visibility $ boolToVisibility $ not state.recordedView
  ][
    (editText)
      $
      [ height MATCH_PARENT
      , width $ WRAP_CONTENT
      , gravity LEFT
      , padding $ Padding 0 0 0 0
      , background Color.blue600
      , color Color.black
      , hint "Write a review"
      , weight 1.0
      , pattern "[^\n]*,255"
      , singleLine true 
      , margin $ MarginTop 20
      , id $ getNewIDWithTag "review-editText"
      , onChange push (FeedbackChanged ( EHC.getNewIDWithTag "reviewBox-editText")) 
      ]  <> FontStyle.body1 TypoGraphy
  , linearLayout[
      height MATCH_PARENT
    , width $ WRAP_CONTENT
    , margin $ MarginTop 8
    ][
      imageView 
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_microphone"
        , height $ V 46
        , width $ V 34
        , onClick push $ const $ OnClickRecord push
        ]
    ]
  ]

audioRecorder :: forall w. RiderRideCompletedScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
audioRecorder state push =
  linearLayout
  [ height $ V 66
  , width MATCH_PARENT
  , background Color.blue600
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ Margin 0 20 0 24
  , padding $ Padding 16 0 8 0
  , stroke $ "1," <> Color.grey800
  , visibility $ boolToVisibility $ state.recordedView
  ][
    imageView 
      [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_delete_icon"
      , height $ V 20
      , width $ V 20
      , margin $ MarginTop 18
      , onClick push $ const $ OnClickClose
      ]
    , relativeLayout 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ Margin 7 12 0 0
        , orientation VERTICAL
        ][
          imageView
            [ width $ V $ (screenWidth unit) - 170
            , height $ V 36
            , margin $ MarginRight 2
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_paused_audio"
            , visibility $ boolToVisibility $ (not state.ratingCard.recordAudioState.isRecording && not state.ratingCard.recordAudioState.isListening )
            ] 
        , lottieAnimationView
            [ width $ V $ (screenWidth unit) - 170
            , padding $ PaddingRight 8 
            , height $ V 36
            , color Color.white900
            , id $ getNewIDWithTag "recordAnimation1"
            , visibility $ boolToVisibility $ ( state.ratingCard.recordAudioState.isRecording || state.ratingCard.recordAudioState.isListening )
            , afterRender
                ( \action -> do
                    void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig { rawJson = "record_audio_animation.json", lottieId = (getNewIDWithTag "recordAnimation1"), scaleType = "FIT_CENTER", speed = 1.0 }
                    pure unit
                )
                (const NoAction)
            ]
          ]
    , linearLayout[
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , id $ getNewIDWithTag "actionButtonRecord"
      , visibility GONE
      ][]
    , linearLayout[
        height MATCH_PARENT
      , width $ MATCH_PARENT
      , margin $ MarginTop 15
      , gravity RIGHT
      ][
        linearLayout[
          height WRAP_CONTENT
        , width $ MATCH_PARENT
        , orientation HORIZONTAL
        , gravity RIGHT
        , visibility $ boolToVisibility $ state.ratingCard.recordAudioState.isRecording
        ][
          textView 
          ([ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.ratingCard.recordAudioState.timer
          , margin $ if os == "IOS" then Margin 0 7 3 0 else Margin 1 7 0 0
          , weight 1.0
          ] <> FontStyle.body3 TypoGraphy )
        ,  imageView 
          [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pause_icon"
          , height $ V 30
          , width $ V 30
          , onClick push $ const $ OnClickStop
          ]
        ]
      , linearLayout[
          height WRAP_CONTENT
        , width $ MATCH_PARENT
        , orientation HORIZONTAL
        , gravity RIGHT
        , visibility $ boolToVisibility $ not state.ratingCard.recordAudioState.isRecording
        ][
          relativeLayout[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          ][
            textView 
              ([ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text state.timerValue
              , margin $ if os == "IOS" then Margin 0 7 3 0 else Margin 0 7 7 0
              , weight 1.0
              , visibility $ boolToVisibility $ not state.ratingCard.recordAudioState.isListening
              ] <> FontStyle.body3 TypoGraphy )
          , textView 
              ([ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text state.countDownValue
              , margin $ if os == "IOS" then Margin 0 7 3 0 else Margin 0 7 7 0
              , weight 1.0
              , visibility $ boolToVisibility $ state.ratingCard.recordAudioState.isListening
              ] <> FontStyle.body3 TypoGraphy )
          ]
        ,  relativeLayout[
              height WRAP_CONTENT
            , width WRAP_CONTENT
          ][
            imageView 
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_play_icon"
            , height $ V 30
            , width $ V 30
            , onClick push $ const $ OnClickPlay
            , visibility $ boolToVisibility $ not state.ratingCard.recordAudioState.isListening
            ]
          ,  imageView 
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pause"
            , height $ V 30
            , width $ V 30
            , onClick push $ const $ OnClickPause
            , visibility $ boolToVisibility $ state.ratingCard.recordAudioState.isListening
            ] 
          ]
        ]
        ]
    ]