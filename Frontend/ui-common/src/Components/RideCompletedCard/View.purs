module Components.RideCompletedCard.View where

import Components.RideCompletedCard.Controller 

import PrestoDOM 
import Components.Banner.View as Banner
import Components.Banner as BannerConfig
import Data.Functor 
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn,fadeInWithDelay) as Anim
import Effect (Effect)
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (+), (<<<), (<>), (==), (>), (>=), (||), (<=), show, void, (/=), when, max, mod )
import Common.Styles.Colors as Color
import Components.SelectListModal as CancelRidePopUp
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Data.Array (mapWithIndex, length, (!!), null, any, (..),head)
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime, safeMarginTop, screenHeight, getNewIDWithTag)
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
import Mobility.Prelude (boolToVisibility)
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

view :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
view config push =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , clickable true
  , background if config.isDriver then Color.grey700 else Color.white900
  ] $ [  mainView config push 
    , stickyButtonView config push 
  ] <> if config.showContactSupportPopUp then [contactSupportPopUpView config push] else []

mainView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
mainView config push = 
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ topGradientView config push 
        , bottomCardView config push
      ]

topGradientView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topGradientView config push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ if config.isDriver then Padding 16 16 16 6 else PaddingVertical 40 6
    , gradient $ Linear 0.0 config.topCard.gradient
    , cornerRadii $ Corners radii false false true true
    , stroke $ borderWidth <> Color.grey900
    , id $ getNewIDWithTag "topViewId"
    ]
    [  topPillAndSupportView config push
      , priceAndDistanceUpdateView config push 
      , whiteHorizontalLine config
      , rideDetailsButtonView config push 
    ]
    where 
      borderWidth = if config.isDriver then "1," else "0,"
      radii = if config.isDriver then 16.0 else 0.0
    

additionalChargesView :: forall w. AdditionalCharges -> PrestoDOM (Effect Unit) w
additionalChargesView config = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginBottom 8
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



rideTierAndCapacity :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideTierAndCapacity push config = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , background if config.isDriver then Color.white800 else Color.tealBlue
  , gravity CENTER
  , padding $ Padding 8 8 8 8
  , margin $ MarginVertical 12 12
  , cornerRadius 4.0
  , visibility GONE
  ][ textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text config.serviceTierAndAC
      , color if config.isDriver then Color.black700 else Color.white800
      ] <> FontStyle.body1 TypoGraphy
    , imageView
      [ height $ V 16
      , width $ V 16
      , imageWithFallback $ fetchImage FF_ASSET "ic_profile_active"
      , visibility $ boolToVisibility $ isJust config.capacity
      , margin $ MarginHorizontal 4 2
      ]
    , textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , color Color.black700
      ] <> FontStyle.body1 TypoGraphy
        <> case config.capacity of
            Just cap -> [text $ show cap ]
            Nothing -> [visibility GONE]
  ] 

topPillAndSupportView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topPillAndSupportView config push = 
  relativeLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ PaddingTop safeMarginTop
        ][linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER 
          , margin $ MarginTop 5
          , orientation VERTICAL
          ][  if config.topCard.topPill.visible then topPillView config push else linearLayout[visibility GONE][]
            , rideTierAndCapacity push config
          ]
        , linearLayout [
            width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity RIGHT
          ][   imageView
              [ height $ V 40
              , width $ V 40
              , margin $ MarginLeft 10
              , accessibility config.accessibility
              , visibility $ boolToVisibility config.enableContactSupport
              , accessibilityHint "Contact Support : Button"
              , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if config.theme == LIGHT then "ny_ic_black_headphone" else "ny_ic_headphone"
              , onClick push $ const Support
              , rippleColor Color.rippleShade
              ]
            ]
        ]
topPillView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
topPillView config push = 
  linearLayout[
    width WRAP_CONTENT
  , height WRAP_CONTENT
  , background config.topCard.topPill.background
  , gravity CENTER
  , padding $ Padding 16 6 16 10
  , cornerRadius 22.0
  ][imageView
    [ width $ V 12
    , height $ V 12
    , margin $ Margin 0 2 6 0
    , visibility $ boolToVisibility (isJust $ config.topCard.topPill.icon)
    , imageWithFallback $ fetchImage FF_COMMON_ASSET $ fromMaybe "" config.topCard.topPill.icon
    ]
  ,  textView $ [
      text config.topCard.topPill.text
    , color config.topCard.topPill.textColor 
    ] <> FontStyle.body1 TypoGraphy
  ]


priceAndDistanceUpdateView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
priceAndDistanceUpdateView config push = 
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , layoutGravity "center_vertical"
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER
          , margin $ MarginHorizontal 16 16
          , padding $ PaddingLeft if config.isDriver ||  config.topCard.topPill.visible || not config.showSafetyCenter then 0 else 40
          ][ textView $
             [ height WRAP_CONTENT
             , text config.topCard.title
             , gravity CENTER
             , weight 1.0
             , color $ if config.theme == LIGHT then Color.black800 else config.topCard.titleColor
             ] <> if config.theme == LIGHT then FontStyle.h3 TypoGraphy else FontStyle.h1 TypoGraphy
           , sosButtonView config push
          ]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , margin $ MarginHorizontal 16 16
          ][ textView $ 
              [ text $ if config.isDriver then Utils.intPriceToBeDisplayed config.topCard.finalAmountWithCurrency true else if config.isFreeRide then "₹0" else "₹" <> (show config.topCard.finalAmount)
              , accessibilityHint $ "Ride Complete: Final Fare ₹"  <> (show config.topCard.finalAmount)
              , accessibility config.accessibility
              , color $ if config.theme == LIGHT then Color.black800 else Color.white900
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              ] <> (FontStyle.body28 TypoGraphy)
          , textView $
              [ textFromHtml $ "<strike> ₹" <> (show config.topCard.initialAmount) <> "</strike>"
              , accessibilityHint $ "Your Fare Has Been Updated From ₹"  <> (show config.topCard.initialAmount) <> " To ₹" <> (show config.topCard.finalAmount)
              , accessibility config.accessibility
              , margin $ Margin 8 5 0 0
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.black600
              , visibility if config.topCard.fareUpdatedVisiblity then VISIBLE else GONE
              ] <> (FontStyle.title1 TypoGraphy)
          ]
        , linearLayout [
            height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          ] $ additionalChargesView <$> config.additionalCharges
        , pillView config push
      ]

pillView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pillView config push =
  linearLayout
    [ width MATCH_PARENT 
    , height WRAP_CONTENT
    , padding config.topCard.infoPill.padding
    , margin config.topCard.infoPill.margin
    , background config.topCard.infoPill.background
    , alpha config.topCard.infoPill.alpha
    , cornerRadius config.topCard.infoPill.cornerRadius
    , gravity CENTER
    , stroke config.topCard.infoPill.stroke
    , visibility config.topCard.infoPill.visible
    ]
    [ imageView
        [ width $ V 20
        , height $ V 20
        , imageWithFallback config.topCard.infoPill.image 
        , margin $ MarginRight 12
        ]
    , textView $
        [ height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , text config.topCard.infoPill.text 
        , color config.topCard.infoPill.color
        , weight 1.0
        ] <> (FontStyle.getFontStyle config.topCard.infoPill.fontStyle LanguageStyle)
    ]

rideDetailsButtonView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideDetailsButtonView config push = 
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
          , accessibility config.accessibility
          , accessibilityHint "Ride Details : Button"
          , cornerRadius 4.0
          , padding $ Padding 4 16 4 16
           , rippleColor Color.rippleShade
          ][  textView $
              [ height WRAP_CONTENT
              , text config.topCard.bottomText
              , color $ if config.theme == LIGHT then Color.black700 else Color.white900
              , weight 1.0
              ] <> (FontStyle.body2 TypoGraphy)
            , imageView
              [ width $ V 18
              , height $ V 18
              , accessibility DISABLE
              , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if config.theme == LIGHT then "ny_ic_chevron_right_grey" else "ny_ic_chevron_right_white"
              ]
          ]
  ]

-------------------------------- Top Card Ends --------------------------------------------------------------------------------------------------------------------------------------------

bottomCardView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomCardView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 70
  , background if config.isDriver then Color.grey700 else Color.white900
  , gravity CENTER
  ] if config.isDriver then  [driverSideBottomCardsView config push] else [customerSideBottomCardsView config push]

customerSideBottomCardsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerSideBottomCardsView config push = 
  let bottomCardPadding = if config.showRentalRideDetails then Padding 8 16 8 16 else Padding 16 16 16 16
  in 
    scrollView[
      width MATCH_PARENT,
      height $ if os == "IOS" then getBottomCardHeight "topViewId" else WRAP_CONTENT
    ][
      linearLayout[
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding bottomCardPadding
      , background Color.white900
      , gravity CENTER
      ]
      [ if config.showRentalRideDetails then rentalTripDetailsView config push
        else rideCustomerExperienceView config push 
      ]
    ]

rideCustomerExperienceView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideCustomerExperienceView config push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  ]
  [ customerIssueView push config.customerIssue 
  , customerRatingDriverView config push
  , needHelpPillView config push
  ]

---------------------------------------------------- customerIssueView ------------------------------------------------------------------------------------------------------------------------------------------
customerIssueView :: forall w. (Action -> Effect Unit) -> CustomerIssue ->  PrestoDOM (Effect Unit) w
customerIssueView push config  = 
  case config.bannerComputedView , config.showIssueBanners, (null config.customerIssueCards) of 
    Just listView , true, false ->
      if length config.customerIssueCards == 1 then customerIssuePopupView push config else customerIssueCarouselView listView push config 
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
 

customerIssueCarousalView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerIssueCarousalView  push = 
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 24 16 24
    , cornerRadiusHolder  "cornerRadius"
    , backgroundHolder "background"
    , orientation VERTICAL
    ][
      textView $ [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , textHolder "title"
      , colorHolder "titleColor" 
      , gravity CENTER
      , padding $ PaddingBottom 4
      ] <> FontStyle.h3 TypoGraphy
    , textView $ [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , textHolder "subTitle" 
      , colorHolder "subTitleColor"
      , gravity CENTER
      ] <> FontStyle.paragraphText TypoGraphy 
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginTop 16
      ][
        linearLayout[
          height WRAP_CONTENT
        , weight 1.0
        , gravity CENTER
        , padding $ Padding 1 1 1 1
        , cornerRadiusHolder  "cornerRadius"
        , backgroundHolder "yesStroke"
        ][
          linearLayout[
            height WRAP_CONTENT
          ,  weight 1.0
          , gravity CENTER
          , padding $ PaddingVertical 14 14
          , onClickHolder push  $  SelectButton true
          , cornerRadiusHolder  "cornerRadius"
          , backgroundHolder "yesBackground"
          ][
            textView $ [
              width WRAP_CONTENT
            , height WRAP_CONTENT
            , textHolder "yesText" 
            , colorHolder "yesTextColor" 
            ] <> FontStyle.subHeading1 TypoGraphy
          ]
        ]
      , linearLayout[
          height WRAP_CONTENT
        , weight 1.0
        , gravity CENTER
        , padding $ Padding 1 1 1 1
        , margin $ MarginLeft 10
        , cornerRadiusHolder  "cornerRadius"
        , backgroundHolder "noStroke"
        ][
          linearLayout[
            height WRAP_CONTENT
          , weight 1.0
          , gravity CENTER
          , onClickHolder push  $ SelectButton false
          , padding $ PaddingVertical 14 14
          , cornerRadiusHolder  "cornerRadius"
          , backgroundHolder "noBackground"  
          ][
            textView $ [
              width WRAP_CONTENT
            , height WRAP_CONTENT
            , textHolder "noText" 
            , colorHolder "noTextColor" 
            ] <> FontStyle.subHeading1 TypoGraphy
          ]
        ]
      ]
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
        , onClick push $ const  $  SelectButton true 0
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





------------------------------------------- customerRatingDriverView -------------------------------------------------------------------------------------------------------------------------------------------------------------
customerRatingDriverView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerRatingDriverView config push =
  let 
    ratingVisibility = boolToVisibility $  ((not config.customerIssue.showIssueBanners)  || (null config.customerIssue.customerIssueCards))
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility ratingVisibility
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey800
  , padding $ Padding 10 10 10 20
  , gravity CENTER
  ][ imageView [
      imageWithFallback config.customerBottomCard.driverImage
      , height $ V 56
      , width $ V 56
    ]
    , commonTextView config push config.customerBottomCard.title Color.black800 (FontStyle.h3 TypoGraphy) 10
    , commonTextView config push config.customerBottomCard.subTitle Color.black800 (FontStyle.paragraphText TypoGraphy) 10
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
              , accessibilityHint $ (show item <> "star" ) <> if item <= config.customerBottomCard.selectedRating then " : Selected " else " : Un Selected "
              , accessibility config.accessibility
              , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item <= config.customerBottomCard.selectedRating then "ny_ic_star_active" else "ny_ic_star_inactive"
              ]
          ]) [1,2,3,4,5])
  ]

needHelpPillView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
needHelpPillView config push = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , background Color.blue600
    , gravity CENTER
    , padding $ Padding 10 12 10 12
    , cornerRadius if os == "IOS" then 20.0 else 24.0
    , onClick push $ const $ HelpAndSupportAC
    , margin $ MarginVertical 20 20
    , rippleColor Color.rippleShade
    , accessibility ENABLE
    , accessibilityHint $ "Need help : Button"
    ][imageView [
        width $ V 16
      , height $ V 16
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_headphone_black"
      , margin $ MarginRight 8
      ]
     , textView $ [
        text config.needHelpText
      , color Color.black700
      ] <> FontStyle.tags TypoGraphy
    ]



------------------------------------- Driver Side Bottom Cards View --------------------------------------------------------------------------------------------------------------------------------------------------------------
driverSideBottomCardsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverSideBottomCardsView config push = 
  scrollView[
    width MATCH_PARENT
  , height MATCH_PARENT
  , scrollBarY false 
  ][
    linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background config.bottomBackground
    , gravity CENTER
    , visibility $ if config.isDriver then VISIBLE else GONE 
    ] $ map (\item -> getViewsByOrder config item push) config.viewsByOrder 
  ]


getViewsByOrder :: forall w. Config -> RideCompletedElements -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
getViewsByOrder config item push = 
  case item of 
    BANNER -> rideEndBannerView config.bannerConfig push
    QR_VIEW -> driverUpiQrCodeView config push
    NO_VPA_VIEW -> noVpaView config
    BADGE_CARD -> badgeCardView config push
    DRIVER_BOTTOM_VIEW -> driverFareBreakUpView config push
    RENTAL_RIDE_VIEW -> rentalRideInfoView push config


------------------------------------ (Driver Card 1) rideEndBannerView -------------------------------------------------------------------------------------------------------------------------
rideEndBannerView :: forall w. BannerConfig.Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideEndBannerView config push =
  linearLayout
    [ height MATCH_PARENT
    , width  MATCH_PARENT
    , orientation VERTICAL
    , margin (Margin 10 10 10 10)
    , visibility if config.isBanner then VISIBLE else GONE
    , gravity BOTTOM
    , weight 1.0
    ][
        Banner.view (push <<< BannerAction) (config)
    ]

------------------------------------- (Driver Card 2) driverUpiQrCodeView --------------------------------------------------------------------------------------------------------------
driverUpiQrCodeView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
driverUpiQrCodeView config push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ if config.lottieQRAnim.visible then lottieQRView config push else dummyTextView
  , linearLayout 
    [
      height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , cornerRadius 16.0
    , gravity CENTER
    , margin $ MarginVertical 10 14
    ][linearLayout 
    [
      height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.yellow800
    , orientation VERTICAL
    , gravity CENTER
    , cornerRadii $ Corners 16.0 true true false false
    , padding $ PaddingVertical 15 12
    ]
    [
      textView $ [
        text config.driverUpiQrCard.text
        , margin $ MarginBottom 8
      ] <> (FontStyle.body2 TypoGraphy)
      , linearLayout [
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.white900
        , cornerRadius 28.0
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
      ][
        imageView [
          width $ V 24
        , height  $ V 24
        , margin $ Margin 5 5 5 5
        , imageWithFallback config.driverUpiQrCard.vpaIcon
        ]
        , textView $ [
          text config.driverUpiQrCard.vpa
          , margin $ MarginHorizontal 5 5
        ] <> FontStyle.body2 TypoGraphy
      ]
      ]
      , PrestoAnim.animationSet [ Anim.fadeInWithDelay 250 true ] $ imageView [
          height $ V 165
        , width $ V 165
        , margin $ MarginVertical 8 13
        , id $ getNewIDWithTag config.driverUpiQrCard.id
        , onAnimationEnd push (const (UpiQrRendered $ getNewIDWithTag config.driverUpiQrCard.id))
      ]
    ]
  ]

--------------------------------------------------- (Driver Card 3) noVpaView --------------------------------------------------------------------------------------------------------------------------------------------
noVpaView :: forall w. Config -> PrestoDOM (Effect Unit) w 
noVpaView config = 
   linearLayout 
    [
      height WRAP_CONTENT
    , width WRAP_CONTENT
    , background Color.yellow800
    , stroke $ "1," <> Color.yellow500
    , cornerRadius 16.0
    , gravity CENTER
    , padding $ Padding 16 16 16 16
    , margin $ MarginBottom 24
    ][
        imageView
        [ height $ V 24
        , width $ V 24
        , margin $ MarginVertical 8 13
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_orange"
        ]
      , textView $
        [ text config.noVpaCard.title
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , padding $ PaddingLeft 8
        , color Color.black800
        ] <> FontStyle.body2 TypoGraphy
    ]
---------------------------------------------- (Driver Card 5) badgeCardView ------------------------------------------------------------------------------------------------------------------------------------------
badgeCardView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
badgeCardView config push = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER 
  , background config.badgeCard.background
  , cornerRadius 8.0
  , margin $ MarginBottom 24 
  , stroke config.badgeCard.stroke
  ][
    imageView[
      imageWithFallback config.badgeCard.image
    , height config.badgeCard.imageHeight
    , width config.badgeCard.imageWidth
    , margin $ MarginTop 15
    ]
  , textView $ [
      text config.badgeCard.text1 
    , color config.badgeCard.text1Color
    ] <> FontStyle.body1 TypoGraphy
  , textView $ [
      text config.badgeCard.text2
    , color config.badgeCard.text2Color
    , margin $ MarginBottom 24
    ] <> FontStyle.body4 TypoGraphy
  ]

---------------------------------------------- (Driver Card 6) driverFareBreakUpView  ------------------------------------------------------------------------------------------------------------------------------------------
driverFareBreakUpView ::  forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
driverFareBreakUpView config push =
  let currency = (getAppConfig appConfig).currency
  in
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.linen
  , stroke ("1," <> Color.grey900)
  , orientation HORIZONTAL
  , cornerRadius 8.0
  , margin $ MarginBottom 24
  , padding $ Padding 12 12 12 12
  , gravity CENTER
  ][
    linearLayout[
      width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    ][linearLayout[
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ](mapWithIndex (\ index item -> 
            linearLayout[
              height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ][
              linearLayout[
                width WRAP_CONTENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER
              , padding $ PaddingHorizontal 8 8
              ][
                textView $ [
                  text $ currency <> (show item.amount)
                , color Color.pigmentGreen
                , margin $ MarginRight 8
                ] <> FontStyle.h0 LanguageStyle
              , textView $ [
                  text item.reason
                , color Color.black800
                , singleLine false
                , lineHeight "20"
                ]<> FontStyle.body6 LanguageStyle
              ]
              , if index /= ((length config.driverBottomCard.savedMoney)-1) then horizontalLine (Margin 12 8 12 8) Color.almond else dummyTextView
            ]
          ) config.driverBottomCard.savedMoney)
      ]
    , linearLayout[
      height MATCH_PARENT,
      weight 1.0
      ][]
    , imageView[
        height $ V 100
      , width $ V 100
      , gravity CENTER
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_wallet_with_coin"
      ]
  ]

stickyButtonView ::  forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
stickyButtonView config push = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadii $ Corners 16.0 true true false false
  , background Color.white900
  , layoutGravity "bottom"
  , alignParentBottom "true,-1"
  , padding $ Padding 16 16 16 if os == "IOS" then max safeMarginBottom 16 else 16
  ][PrimaryButton.view (push <<< SkipButtonActionController) (config.primaryButtonConfig)]

contactSupportPopUpView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
contactSupportPopUpView config push = 
  linearLayout [
    width MATCH_PARENT,
    height MATCH_PARENT
  ][PopUpModal.view (push <<< ContactSupportPopUpAC) config.contactSupportPopUpConfig] 

---------------------------------------------- (Driver Card 7) rentalRideDetailsView  ------------------------------------------------------------------------------------------------------------------------------------------

rentalTripDetailsView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
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
      --   rentalTripRowView config push RideTime -- TODO :: Mercy - uncomment once we start getting proper value from backend 
      -- , rentalTripRowView config push RideDistance
      -- , separatorView
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

rentalTripRowView :: forall w. Config -> (Action -> Effect Unit) -> RentalRowView -> PrestoDOM (Effect Unit) w
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
      getTextConfig :: Config -> RentalRowView -> RentalTextConfig
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

--------------------------------- Helpers ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
commonTextView :: forall w. Config -> (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> PrestoDOM (Effect Unit) w
commonTextView config push text' color' fontStyle marginTop =
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

whiteHorizontalLine :: forall w . Config -> PrestoDOM (Effect Unit) w
whiteHorizontalLine config = 
  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , margin $ MarginTop 20
    , background config.topCard.horizontalLineColor
    ][]

getBottomCardHeight :: String -> Length 
getBottomCardHeight id = V $ (screenHeight unit) - (runFn1 JB.getLayoutBounds $ getNewIDWithTag id).height - 82

lottieQRView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
lottieQRView config push = 
  PrestoAnim.animationSet [ Anim.fadeInWithDelay 250 true ] $
    lottieAnimationView
      [ id $ EHC.getNewIDWithTag "QRLottie"
      , background Color.white900
      , cornerRadius 16.0
      , height WRAP_CONTENT
      , padding $ PaddingTop 5
      , width MATCH_PARENT
      , onAnimationEnd (\_-> void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig{ rawJson = config.lottieQRAnim.url , lottieId = (EHC.getNewIDWithTag "QRLottie"), speed = 1.0 , scaleType = "CENTER_CROP"})(const UpiQrRendered)
      ]

rentalRideInfoView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rentalRideInfoView push config = 
  let 
    rideStartedTime = not $ DS.null $ config.rentalRideConfig.rideStartedAt
    rideEndTime = not $ DS.null $ config.rentalRideConfig.rideEndedAt
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL 
  , cornerRadius 8.0
  , padding $ Padding 16 16 16 16
  , margin $ MarginVertical 14 24
  , background Color.white900
  , stroke $ "2," <> Color.grey800
  ]([  
    -- linearLayout -- TODO :: Mercy - Uncomment once we start getting proper value from backend
    --   [ height WRAP_CONTENT
    --   , width MATCH_PARENT
    --   , orientation if config.isDriver then HORIZONTAL else VERTICAL
    --   ][  infoCardView config (if config.isDriver then "VERTICAL" else "HORIZONTAL") $ getRentalRideInfoCardView config "RideCompletedCardImage1" config.rentalRideTextConfig.rideTime "" config.rentalRideConfig.actualRideDuration config.rentalRideConfig.baseRideDuration
    --     , infoCardView config (if config.isDriver then "VERTICAL" else "HORIZONTAL") $ getRentalRideInfoCardView config "RideCompletedCardImage2" config.rentalRideTextConfig.rideDistance "" config.rentalRideConfig.actualRideDistance config.rentalRideConfig.baseRideDistance
    --   ]
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility $ boolToVisibility $ (rideStartedTime || rideEndTime )
      ][  if rideStartedTime then infoCardView config "HORIZONTAL" $ getRentalRideInfoCardView config "" config.rentalRideTextConfig.rideStartedAt "" config.rentalRideConfig.rideStartedAt "" else textView[],
          textView[height $ V 2, width MATCH_PARENT],
          if rideEndTime then infoCardView config "HORIZONTAL" $ getRentalRideInfoCardView config "" config.rentalRideTextConfig.rideEndedAt "" config.rentalRideConfig.rideEndedAt "" else textView[]
      ]
  ] <>
      if (config.rentalRideConfig.showRideOdometerReading) then
       [
        horizontalLine (MarginVertical 16 16) Color.grey900
        , textView $ 
          [ height WRAP_CONTENT
          , margin $ MarginBottom 16
          , width MATCH_PARENT
          , text $ (config.rentalRideTextConfig.odometerReading) <> ": "
          , visibility if config.isDriver then VISIBLE else GONE
          , color Color.black700 
          ] <> FontStyle.body2 TypoGraphy
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation if config.isDriver then HORIZONTAL else VERTICAL
          ][  infoCardView config (if config.isDriver then "VERTICAL" else "HORIZONTAL") $ getRentalRideInfoCardOdometerView config "RideCompletedCardImage3" config.rentalRideConfig.startRideOdometerImage (config.rentalRideTextConfig.rideStart) "" (config.rentalRideTextConfig.rideStartedAt) (config.rentalRideConfig.rideStartODOReading)
            , infoCardView config (if config.isDriver then "VERTICAL" else "HORIZONTAL") $ getRentalRideInfoCardOdometerView config "RideCompletedCardImage4" config.rentalRideConfig.endRideOdometerImage (config.rentalRideTextConfig.rideEnd) "" (config.rentalRideTextConfig.rideEndedAt) (config.rentalRideConfig.rideEndODOReading)
          ]]
      else [])
    

getRentalRideInfoCardOdometerView :: forall w. Config -> String -> String -> String -> String -> String -> String -> (InfoCardConfig)
getRentalRideInfoCardOdometerView config image renderImage heading headingInfo' heading' subHeading1  = 
  { id : image, 
    margin : (MarginBottom 0 ), 
    height : WRAP_CONTENT, 
    width : V ((screenWidth unit) / 2) ,
    image : {
      visibility : GONE, 
      height  : V 72, width : V 110, 
      renderImage : if config.isDriver then (renderImage) else ""
      } , 
    heading : {
      text : if config.isDriver then heading else heading' , 
      color : Color.black700 , 
      fontStyle : FontStyle.body1 TypoGraphy, 
      visibility : VISIBLE
      }, 
    headingInfo : {
      text : headingInfo' , 
      color : Color.black700 , 
      fontStyle : FontStyle.body1 TypoGraphy, 
      visibility : boolToVisibility $ not $ DS.null headingInfo'
    },
    subHeading1 : {
      text :  subHeading1, 
      color : Color.black800 , 
      fontStyle : FontStyle.body2 TypoGraphy, 
      visibility : VISIBLE
      }, 
    subHeading2 : {
      text : "" , 
      color : Color.black600 , 
      fontStyle : FontStyle.body2 TypoGraphy, 
      visibility : GONE
      }}

getRentalRideInfoCardView :: forall w. Config -> String -> String -> String -> String -> String -> (InfoCardConfig)
getRentalRideInfoCardView config image heading headingInfo' subHeading1 subHeading2 =
  { id : image,
    margin : (MarginBottom 0 ), 
    height : WRAP_CONTENT, 
    width : V ((screenWidth unit) / 2) ,
    image : {
      visibility : GONE , 
      height  : V 100, 
      width : V 100, 
      renderImage : ""
      }, 
    headingInfo : {
      text : headingInfo' , 
      color : Color.black700 , 
      fontStyle : FontStyle.body1 TypoGraphy, 
      visibility : boolToVisibility $ not $ DS.null headingInfo'
    },
    heading : {
      text : heading, 
      color : Color.black700, 
      fontStyle : FontStyle.body1 TypoGraphy, 
      visibility : VISIBLE
      }, 
    subHeading1 : {
      text : subHeading1, 
      color : Color.black800, 
      fontStyle : FontStyle.body2 TypoGraphy, 
      visibility : VISIBLE
      }, 
    subHeading2 : {
      text : " / " <> subHeading2, 
      color : Color.black600, 
      fontStyle : FontStyle.body2 TypoGraphy, 
      visibility :  boolToVisibility $ not $ DS.null subHeading2
      }}


infoCardView :: forall w. Config -> String -> InfoCardConfig -> PrestoDOM (Effect Unit) w
infoCardView config orientation' infoCardConfig = 
  linearLayout
  [ height infoCardConfig.height
  , weight 1.0
  , width MATCH_PARENT
  , margin infoCardConfig.margin
  , gravity LEFT
  , orientation if orientation' == "VERTICAL" then VERTICAL else HORIZONTAL
  ][  PrestoAnim.animationSet [Anim.fadeIn config.isDriver ]$ linearLayout
    [ width infoCardConfig.image.width
    , height infoCardConfig.image.height
    , background Color.black
    , gravity CENTER
    , margin $ MarginBottom 8
    , cornerRadius 4.0
    , id (getNewIDWithTag infoCardConfig.id)
    , visibility infoCardConfig.image.visibility
    , onAnimationEnd
        ( \action -> do 
            when (infoCardConfig.image.renderImage /= "") do
              renderBase64Image infoCardConfig.image.renderImage (getNewIDWithTag (infoCardConfig.id)) true "FIT_CENTER"
        ) (const NoAction)
    ][]
    , textView $
      [ height WRAP_CONTENT
      , width  WRAP_CONTENT
      , text infoCardConfig.heading.text 
      , color infoCardConfig.heading.color
      , margin $ MarginBottom 4
      ] <> infoCardConfig.heading.fontStyle
    , linearLayout
      [ weight 1.0
      , visibility $ boolToVisibility $ orientation' == "HORIZONTAL"
      , height WRAP_CONTENT][]
    , linearLayout[
        height WRAP_CONTENT
      , width WRAP_CONTENT
      ][textView $
        [ height WRAP_CONTENT
        , width  WRAP_CONTENT
        , text infoCardConfig.subHeading1.text
        , color infoCardConfig.subHeading1.color
        , visibility infoCardConfig.subHeading1.visibility
        ] <> infoCardConfig.subHeading1.fontStyle
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text infoCardConfig.subHeading2.text
        , color infoCardConfig.subHeading2.color
        , visibility infoCardConfig.subHeading2.visibility
        ] <> infoCardConfig.subHeading2.fontStyle
        ]
    ]

sosButtonView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
sosButtonView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity RIGHT
  , cornerRadius if os == "IOS" then 18.0 else 25.0
  , background Color.black150
  , padding $ Padding 8 8 8 8
  , visibility $ boolToVisibility $ (not config.topCard.topPill.visible) && config.showSafetyCenter
  , onClick push $ const GoToSOS
  ]
  [ imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_shield_blue"
      , height $ V 24
      , width $ V 24
      , accessibilityHint $ "S O S Button, Select to view S O S options"
      , accessibility ENABLE
      ]
  , textView
      $ [ text config.safetyTitle
        , color Color.white900
        , margin $ MarginBottom 1
        , visibility GONE
        ]
      <> FontStyle.body6 TypoGraphy
  ]
