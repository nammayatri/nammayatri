module Components.RideCompletedCard.View where

import Components.RideCompletedCard.Controller (Config, Action(..), Theme(..), RideCompletedElements(..))

import PrestoDOM ( Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), singleLine, scrollView, background, clickable, color, cornerRadius, disableClickFeedback, ellipsize, fontStyle, gradient, gravity, height, id, imageView, imageWithFallback, lineHeight, linearLayout, margin, onClick, alpha, orientation, padding, relativeLayout, stroke, text, textFromHtml, textSize, textView, url, visibility, webView, weight, width, layoutGravity, accessibility, accessibilityHint, afterRender, alignParentBottom)
import Components.Banner.View as Banner
import Components.Banner as BannerConfig
import Data.Functor (map)
import PrestoDOM.Animation as PrestoAnim
import Effect (Effect)
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (<<<), (<>), (==), (>), (>=), (||), (<=), show, void, (/=))
import Common.Styles.Colors as Color
import Components.SelectListModal as CancelRidePopUp
import Helpers.Utils (getAssetStoreLink, getAssetsBaseUrl, getCommonAssetStoreLink)
import Data.Array (mapWithIndex, length, (!!), null)
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime, safeMarginTop, screenHeight, getNewIDWithTag)
import Components.PrimaryButton as PrimaryButton
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties (cornerRadii)
import Language.Types (STR(..))
import Common.Types.App (LazyCheck(..))
import Font.Style as FontStyle
import Font.Size as FontSize
import Halogen.VDom.DOM.Prop (Prop)
import Components.PopUpModal as PopUpModal
import MerchantConfig.Utils (getValueFromConfig)
import Language.Strings (getString)
import JBridge as JB
import Data.Function.Uncurried (runFn1)

view :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
view config push =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , clickable true
  , background if config.isDriver then Color.grey700 else Color.white900
  ] $ [  mainView config push 
    , stickyButtonView config push 
  ] <> if config.customerIssueCard.reportIssueView then [reportIssueView config push] else []
    <> if config.showContactSupportPopUp then [contactSupportPopUpView config push] else []

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
    , padding $ Padding 16 16 16 16
    , gradient $ Linear (if os == "IOS" then 90.0 else 0.0) config.topCard.gradient
    , id $ getNewIDWithTag "topViewId"
    ]
    [  topPillAndSupportView config push
      , priceAndDistanceUpdateView config push 
      , whiteHorizontalLine config
      , rideDetailsButtonView config push 
    ]

topPillAndSupportView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
topPillAndSupportView config push = 
  relativeLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ PaddingTop safeMarginTop
        ][linearLayout[
            width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER 
          , margin $ MarginTop 5
          ] $ if config.topCard.topPill.visible then [topPillView config push] else []
        , linearLayout [
            width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity RIGHT
          ][imageView
              [ height $ V 40
              , width $ V 40
              , accessibility config.accessibility
              , accessibilityHint "Contact Support : Button"
              , imageWithFallback if config.theme == LIGHT then ("ny_ic_black_headphone," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_black_headphone.png") else ("ny_ic_headphone," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_headphone.png")
              , onClick push $ const Support
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
  ][
    textView $ [
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
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.topCard.title
          , color $ if config.theme == LIGHT then Color.black800 else Color.grey900
          ] <> if config.theme == LIGHT then FontStyle.h3 TypoGraphy else FontStyle.h1 TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ][ textView $ 
              [ text $ "₹" <> (show config.topCard.finalAmount)
              , accessibilityHint $ "Ride Complete: Final Fare ₹"  <> (show config.topCard.finalAmount)
              , accessibility config.accessibility
              , color $ if config.theme == LIGHT then Color.black800 else Color.white900
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              ] <> (FontStyle.title0 TypoGraphy)
          , textView $
              [ textFromHtml $ "<strike> ₹" <> (show config.topCard.initalAmount) <> "</strike>"
              , accessibilityHint $ "Your Fare Has Been Updated From ₹"  <> (show config.topCard.initalAmount) <> " To ₹" <> (show config.topCard.finalAmount)
              , accessibility config.accessibility
              , margin $ Margin 8 5 0 0
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.black600
              , visibility if config.topCard.fareUpdatedVisiblity then VISIBLE else GONE
              ] <> (FontStyle.title1 TypoGraphy)
          ]
        , pillView config push
      ]

pillView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pillView config push =
  linearLayout
    [ width WRAP_CONTENT
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
        , visibility config.topCard.infoPill.imageVis
        , margin $ MarginRight 12
        ]
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , text config.topCard.infoPill.text 
        , color config.topCard.infoPill.color
        ] <> (FontStyle.getFontStyle config.topCard.infoPill.fontStyle LanguageStyle)
    ]

rideDetailsButtonView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideDetailsButtonView config push = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT 
  , layoutGravity "bottom"
  ][
    linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ MarginTop 10
          , gravity CENTER_VERTICAL
          , onClick push $ const RideDetails
          , accessibility config.accessibility
          , accessibilityHint "Ride Details : Button"
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
              , imageWithFallback $ if config.theme == LIGHT then ("ny_ic_chevron_right_grey," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey.png") else ("ny_ic_chevron_right_white," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_white.png")
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
  scrollView[
    width MATCH_PARENT,
    height $ if os == "IOS" then getBottomCardHeight "topViewId" else WRAP_CONTENT
  ][
    linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    , background Color.white900
    , gravity CENTER
    ][
      customerIssueView config push
    , customerRatingDriverView config push
    ]
  ]


---------------------------------------------------- customerIssueView ------------------------------------------------------------------------------------------------------------------------------------------
customerIssueView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerIssueView config push =
  scrollView [
    width MATCH_PARENT 
  , height WRAP_CONTENT
  , visibility $ if config.customerIssueCard.issueFaced then VISIBLE else GONE
  ][
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , cornerRadius 8.0
    , stroke $ "1,"<>Color.grey800
    , orientation VERTICAL
    , padding $ Padding 10 10 10 10
    ][  commonTextView config push config.customerIssueCard.title Color.black800 (FontStyle.h3 TypoGraphy) 0
      , commonTextView config push config.customerIssueCard.subTitle Color.black800 (FontStyle.paragraphText TypoGraphy) 10
      , yesNoRadioButton config push
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , margin $ MarginTop 15
        , orientation VERTICAL
        , visibility if config.customerIssueCard.selectedYesNoButton == 0 then VISIBLE else GONE
        ](mapWithIndex (\ index item ->
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingHorizontal 10 10
            , orientation VERTICAL
            , onClick push $ const $ IssueReportIndex index
            ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ MarginBottom 15
                ][  textView $ 
                    [ height WRAP_CONTENT
                    , text item
                    , color Color.black900
                    , weight 1.0
                    ] <> (FontStyle.body2 TypoGraphy)
                  , imageView
                    [ width $ V 15
                    , height $ V 15
                    , imageWithFallback $ "ny_ic_chevron_right," <> (getCommonAssetStoreLink FunctionCall)<> "ny_ic_chevron_right.png"
                    ]
                ]
              , linearLayout
                [ width MATCH_PARENT
                , height $ V 1
                , background Color.grey900
                , margin $ MarginBottom 15
                , visibility if index == 0 then VISIBLE else GONE
                ][]
            ]) [config.customerIssueCard.option1Text, config.customerIssueCard.option2Text])
    ]
  ]

yesNoRadioButton :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
yesNoRadioButton config push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ PaddingHorizontal 10 10
  , margin $ MarginVertical 15 10
  , gravity CENTER
  ](mapWithIndex (\index item ->
      linearLayout
      [ weight 1.0
      , height WRAP_CONTENT
      , orientation VERTICAL
      , cornerRadius 8.0
      , stroke $ "1,"<> if config.customerIssueCard.selectedYesNoButton == index then Color.blue900 else Color.grey800
      , background if config.customerIssueCard.selectedYesNoButton == index then Color.blue600 else Color.white900
      , gravity CENTER
      , onClick push $ const $ SelectButton index
      , margin $ MarginLeft if index == 0 then 0 else 10
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text item
          , color Color.black800
          , padding $ Padding 10 12 10 12
          ] <> FontStyle.subHeading1 TypoGraphy
      ]
    ) [(config.customerIssueCard.yesText), (config.customerIssueCard.noText)])


------------------------------------------- customerRatingDriverView -------------------------------------------------------------------------------------------------------------------------------------------------------------
customerRatingDriverView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerRatingDriverView config push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey800
  , padding $ Padding 10 10 10 10
  , margin $ MarginBottom 24
  , gravity CENTER
  ][ imageView [
      imageWithFallback $ "ny_ic_driver_avatar,"<> (getAssetStoreLink FunctionCall) <> "ny_ic_driver_avatar.png"
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
              , imageWithFallback if item <= config.customerBottomCard.selectedRating then "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_active.png"
                                      else "ny_ic_star_inactive,"<> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_inactive.png"
              ]
          ]) [1,2,3,4,5])
  ]
------------------------------------- Driver Side Bottom Cards View --------------------------------------------------------------------------------------------------------------------------------------------------------------
driverSideBottomCardsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverSideBottomCardsView config push = 
  scrollView[
    width MATCH_PARENT
  , height MATCH_PARENT
  ][
    linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.grey700
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
    [
      height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , cornerRadius 16.0
    , gravity CENTER
    , margin $ MarginBottom 24
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
      , imageView [
          height $ V 165
        , width $ V 165
        , margin $ MarginVertical 8 13
        , id $ getNewIDWithTag config.driverUpiQrCard.id
        , afterRender push (const (UpiQrRendered $ getNewIDWithTag config.driverUpiQrCard.id))
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
        , imageWithFallback $ "ny_ic_info_orange," <> (getAssetStoreLink FunctionCall) <> "ny_ic_info_orange.png"
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
                  text $ (getValueFromConfig "currency") <> (show item.amount)
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
              , if index /= ((length config.driverBottomCard.savedMoney)-1) then horizontalLine else dummyTextView
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
      , imageWithFallback $ "ny_ic_wallet_with_coin," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet_with_coin.png"
      ]
  ]

stickyButtonView ::  forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
stickyButtonView config push = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  , cornerRadii $ Corners 16.0 true true false false
  , background Color.white900
  , layoutGravity "bottom"
  , alignParentBottom "true,-1"
  , padding $ Padding 16 16 16 16
  ][PrimaryButton.view (push <<< SkipButtonActionController) (config.primaryButtonConfig)]

reportIssueView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
reportIssueView  config push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ][ CancelRidePopUp.view (push <<< IssueReportPopUpAC) (config.customerIssueCard.reportIssuePopUpConfig)]

contactSupportPopUpView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
contactSupportPopUpView config push = 
  linearLayout [
    width MATCH_PARENT,
    height MATCH_PARENT
  ][PopUpModal.view (push <<< ContactSupportPopUpAC) config.contactSupportPopUpConfig]

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


horizontalLine :: forall w. PrestoDOM (Effect Unit) w 
horizontalLine = 
  linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.almond
          , margin $ Margin 12 8 12 8 
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
    , background if config.isDriver then Color.white900 else Color.black800
    ][]

getBottomCardHeight :: String -> Length 
getBottomCardHeight id = V $ (screenHeight unit) - (runFn1 JB.getLayoutBounds $ getNewIDWithTag id).height - 82