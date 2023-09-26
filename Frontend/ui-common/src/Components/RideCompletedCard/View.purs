module Components.RideCompletedCard.View where

import Components.RideCompletedCard.Controller (Config, Action(..), Theme(..))

import PrestoDOM ( Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), scrollView, background, clickable, color, cornerRadius, disableClickFeedback, ellipsize, fontStyle, gradient, gravity, height, id, imageView, imageWithFallback, lineHeight, linearLayout, margin, onClick, alpha, orientation, padding, relativeLayout, stroke, text, textFromHtml, textSize, textView, url, visibility, webView, weight, width, layoutGravity, accessibility, accessibilityHint,afterRender, alignParentBottom)
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
import Language.Strings (getString)

view :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
view config push =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , clickable true
  ][  scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.grey700
      ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][ topCardView config push
        , bottomCardView config push
      ]
      ]
      , if config.isPrimaryButtonSticky then 
        relativeLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , weight 1.0
        , cornerRadii $ Corners 16.0 true true false false
        , background Color.white900
        , layoutGravity "bottom"
        , gravity BOTTOM
        , alignParentBottom "true,-1"
        , padding $ Padding 16 16 16 16
        ][ PrimaryButton.view (push <<< SkipButtonActionController) (config.primaryButtonConfig)]
        else linearLayout [][]
    , if config.customerIssueCard.reportIssueView then reportIssueView config push else dummyTextView
    , if config.showContackSupportPopUp then contactSupportPopUpView config push else dummyTextView
  ]
  
topCardView :: forall w. Config -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
topCardView config push =
  linearLayout
  ([ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 16
  , gradient $ Linear (if os == "IOS" then 90.0 else 0.0) config.topCard.gradient
  , gravity CENTER
  ]<> if config.theme == LIGHT then [stroke $ "1,"<>Color.grey800
                                    , cornerRadii $ Corners 16.0 false false true true
                                    ] 
                                else [])
  [  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity RIGHT
      , layoutGravity "right"
      , padding $ PaddingTop safeMarginTop
      ][  imageView
          [ height $ V 40
          , width $ V 40
          , accessibility config.accessibility
          , accessibilityHint "Contact Support : Button"
          , imageWithFallback if config.theme == LIGHT then ("ny_ic_black_headphone," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_black_headphone.png") else ("ny_ic_headphone," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_headphone.png")
          , onClick push $ const Support
          ]
      ]
    , if config.topCard.topPill.visible then topPillView config push else dummyTextView
    , linearLayout
      [ width MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      , gravity CENTER
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.topCard.title
          , color $ if config.theme == LIGHT then Color.black800 else Color.grey900
          ] <> FontStyle.h1 TypoGraphy
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
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.white900
      ][]
    , linearLayout
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

bottomCardView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomCardView config push =
   scrollView
  [ width  MATCH_PARENT
  , height  MATCH_PARENT
  , background Color.grey700
  ]
  [linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 60
  , background Color.grey700
  , gravity CENTER
  , weight 1.0
  ][  if config.customerIssueCard.issueFaced then customerIssueView config push
        else if config.customerBottomCard.visible then customerBottomCardView config push
          else dummyTextView
          -- else if config.badgeCard.visible then badgeCardView config push -- Removed temporarily till endride screen redesigned
            -- else if config.driverBottomCard.visible then driverBottomCardView config push else dummyTextView  -- Removed temporarily till endride screen redesigned
    , if config.qrVisibility then driverUpiQrCodeView config push else dummyTextView 
    , if config.qrVisibility then collectCashView config.driverUpiQrCard.collectCashText else dummyTextView 
    , if config.noVpaVisibility then collectCashView config.noVpaCard.collectCashText else dummyTextView 
    , if config.noVpaVisibility then noVpaView config push  else dummyTextView
    , if not config.isPrimaryButtonSticky then 
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , weight 1.0
      , gravity BOTTOM
      , padding $ PaddingBottom safeMarginBottom
      ][ PrimaryButton.view (push <<< SkipButtonActionController) (config.primaryButtonConfig)]
      else linearLayout [] []
  ]
  ]

customerIssueView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerIssueView config push =
  scrollView [
    width MATCH_PARENT ,
    height WRAP_CONTENT
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


customerBottomCardView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerBottomCardView config push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 8.0
  , stroke $ "1,"<>Color.grey800
  , padding $ Padding 10 10 10 10
  , gravity CENTER
  ][ imageView [
      imageWithFallback $ "ny_ic_driver_avatar,"<> (getAssetStoreLink FunctionCall) <> "ny_ic_driver_avatar.png"
      , height $ V 56
      , width $ V 56
    ]
    , commonTextView config push ( config.customerBottomCard.title) Color.black800 (FontStyle.h3 TypoGraphy) 10
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

reportIssueView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
reportIssueView  config push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ][ CancelRidePopUp.view (push <<< IssueReportPopUpAC) (config.customerIssueCard.reportIssuePopUpConfig)]


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

driverBottomCardView ::  forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
driverBottomCardView config push = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.linen
  , orientation HORIZONTAL
  , cornerRadius 8.0
  , padding $ Padding 12 12 12 12
  , gravity CENTER
  ][
    linearLayout[
      width $ V 230
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ](mapWithIndex (\ index item -> 
        linearLayout[
          height WRAP_CONTENT,
          width MATCH_PARENT,
          orientation VERTICAL, 
          gravity CENTER
        ][
          linearLayout[
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , margin $ Margin 0 8 12 8
          , padding $ PaddingHorizontal 8 8
          , gravity CENTER
          ][
            textView $ [
              text $ "₹" <> (show item.amount)
            , color Color.pigmentGreen
            , gravity CENTER
            , margin $ MarginRight 8
            ] <> FontStyle.h0 LanguageStyle
          , textView $ [
              text item.reason
            , color Color.black800
            , gravity LEFT
            ]<> FontStyle.body6 LanguageStyle
          ]
          , if index /= ((length config.driverBottomCard.savedMoney)-1) then horizontalLine else dummyTextView
        ]
    ) config.driverBottomCard.savedMoney)
  , linearLayout[
      height WRAP_CONTENT
    , width $ V 100
    , gravity CENTER 
    ][
      imageView[
        height $ V 100
      , width $ V 100
      , gravity CENTER
      , imageWithFallback $ "ny_ic_wallet_with_coin," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet_with_coin.png"
      ]
    ]
  ]

contactSupportPopUpView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
contactSupportPopUpView config push = 
  linearLayout [
    width MATCH_PARENT,
    height MATCH_PARENT
  ][PopUpModal.view (push <<< ContactSupportPopUpAC) config.contactSupportPopUpConfig]

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

badgeCardView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
badgeCardView config push = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  , gravity CENTER 
  , background config.badgeCard.background
  , cornerRadius 8.0
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
    , margin $ MarginBottom 8
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
        , orientation HORIZONTAL
        , cornerRadius 28.0
        , stroke $ "1,"<> "#E5E7EB"
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

collectCashView ::forall w. String -> PrestoDOM (Effect Unit) w 
collectCashView title = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingVertical 14 14
    , gravity CENTER
    ][
      textView $
        [ text title
        ]  <> FontStyle.body5 TypoGraphy
    ]

noVpaView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
noVpaView config push = 
   linearLayout 
    [
      height WRAP_CONTENT
    , width WRAP_CONTENT
    , background Color.yellow800
    , stroke $ "1," <> Color.yellow500
    , cornerRadius 16.0
    , gravity CENTER
    , padding $ Padding 16 16 16 16
    , margin $ MarginBottom 26
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

topPillView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
topPillView config push = 
  linearLayout[
    width WRAP_CONTENT
  , height WRAP_CONTENT
  , background config.topCard.topPill.background
  , gravity CENTER
  , padding $ Padding 16 6 16 6
  , cornerRadius 22.0
  ][
    textView $ [
      text config.topCard.topPill.text
    , color config.topCard.topPill.textColor 
    ] <> FontStyle.body1 TypoGraphy
  ]