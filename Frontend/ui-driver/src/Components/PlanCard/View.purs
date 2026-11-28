module Components.PlanCard.View where

import Prelude
import Data.Maybe
import Effect (Effect)
import Data.Array as DA
import Data.String as DS
import Screens.Types as ST
import Services.API as API
import Helpers.Utils as HU
import Styles.Colors as Color
import Mobility.Prelude as MP
import Font.Style as FontStyle
import Language.Types (STR(..))
import Components.Banner as Banner
import Common.Types.App (LazyCheck(..))
import PrestoDOM.Properties (cornerRadii)
import Language.Strings (getString, getVarString)
import Components.PlanCard.Controller (Action(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Locale.Utils (getLanguageLocale, languageKey)
import Engineering.Helpers.Utils (getFixedTwoDecimals)
import PrestoDOM (alignParentRight, alignParentLeft, Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gradient, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarX, scrollBarY, scrollView, shimmerFrameLayout, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)

view :: forall w. (Action -> Effect Unit) -> ST.PlanCardState -> PrestoDOM (Effect Unit) w
view push state =
    let isSelected = state.isSelected
        clickable' = state.clickable
        showBanner = state.showBanner
        isMyPlan = state.isMyPlan
        isSelectedLangTamil = state.isSelectedLangTamil
        isActivePlan = state.isActivePlan
        offerBannerProps = state.offerBannerProps
        isIntroductory = state.isIntroductory
        offerBannerPlans = state.offerBannerPlans
        mbCoinDiscountUpto = state.mbCoinDiscountUpto
        dummyOfferConfig = { showOfferBanner : false, offerBannerValidTill : "", offerBannerDeadline : "", payAmount : "", offerBannerPlans : []}
        gradient' = Linear 180.0 if isSelected && not isMyPlan then [Color.darkMint, Color.blue800] else [Color.darkMint, Color.grey900]
        coinDiscountUpto = maybe "" (\coinDiscount -> if coinDiscount > 0.0 then getFixedTwoDecimals coinDiscount else "") mbCoinDiscountUpto
        cardMargin = case isMyPlan, isIntroductory of
                      true, _ -> Margin 16 13 16 0 
                      _, _ -> MarginVertical 6 6
  in
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , clickable if isMyPlan then true else clickable'
  , alpha if not clickable' && isMyPlan then 0.5 else 1.0
  ][ linearLayout
    ([ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 1 1 1 1
    , margin cardMargin
    , cornerRadius 8.0
    , orientation VERTICAL
   ] <> if isActivePlan then [gradient gradient'] else [])
   [ linearLayout
      [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , cornerRadius 8.0
        , cornerRadii $  if isActivePlan && (not $ DS.null coinDiscountUpto) then (Corners 8.0 true true false false) else (Corners 8.0 true true true true)
        ][
        linearLayout
        ([ height WRAP_CONTENT
        , width MATCH_PARENT
        , background if isSelected && not isMyPlan && not isIntroductory then Color.blue600 else Color.white900
        , stroke $ "1," <> (if isIntroductory then Color.grey900
                            else if isSelected && isActivePlan then Color.transparent
                            else if isSelected && not isMyPlan then Color.blue800 
                            else Color.grey900)
        , padding $ Padding 16 12 16 (if isMyPlan then 16 else 12)
        , cornerRadius 8.0
        , orientation VERTICAL
        ] <> if isIntroductory 
               then []
               else [onClick push $ const $ OnClick $ mkPlanCardConfig state])
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER_VERTICAL
          , margin $ MarginBottom 5
          ][ textView $
              [ text state.title
              , weight 1.0
              , color if isSelected && not isMyPlan then Color.blue900 else Color.black700
              ] <> if isSelectedLangTamil then FontStyle.body17 TypoGraphy else FontStyle.body22 TypoGraphy
            , planPriceView state.priceBreakup state.frequency isSelectedLangTamil isIntroductory
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ][ textView $
              [ text state.description
              , color Color.black600
              , weight 1.0
              , visibility $ MP.boolToVisibility $ not DS.null state.description
              ] <> if isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
            , if state.showOffer && DA.length state.offers > 1 then offerCountView (DA.length state.offers) isSelected else linearLayout[visibility GONE][]
            ]
          , horizontalScrollView 
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , scrollBarX false
            , margin $ MarginTop 8
            , visibility if DA.length state.offers == 1 || (isSelected && DA.length state.offers > 0) then VISIBLE else GONE
            ][ linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              ](map  (\item -> promoCodeView push item isIntroductory isSelectedLangTamil) state.offers)
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , visibility if isSelected && (DA.length state.offers > 0) || isIntroductory then VISIBLE else GONE
            ](map (\item ->
                linearLayout
                  ([ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , padding $ Padding 8 8 8 8
                  , margin $ MarginTop 8
                  , background if isMyPlan || isIntroductory then Color.grey700 else Color.white900
                  , cornerRadius 4.0
                  ] <> case item.offerDescription of 
                        Just desc -> [text desc, visibility if (isSelected || isIntroductory) && not DS.null desc then VISIBLE else GONE]
                        Nothing -> [visibility GONE])
                  [ textView $
                    [ textFromHtml $ fromMaybe "" item.offerDescription
                    , color Color.black600
                    ] <> if isSelectedLangTamil then FontStyle.captions TypoGraphy else FontStyle.body3 TypoGraphy
                  ]
              )state.offers)
          , offerCardBannerView push false (isJust offerBannerProps && (DA.any (_ == state.id) offerBannerPlans) && showBanner) true (fromMaybe dummyOfferConfig offerBannerProps) false
            
          ]
      ]
      ,
      linearLayout 
        [height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation HORIZONTAL
        , visibility $ MP.boolToVisibility (isActivePlan && coinDiscountUpto /= "")
        , background Color.blue600
        , cornerRadii $ (Corners 8.0 false false true true)
        , padding $ Padding 2 4 2 4
        ]
        [ 
          imageView
          [ width $ V 12
          , height $ V 12
          , margin (MarginRight 4)
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_discount"
          ] 
        , textView $
          [ color Color.black800
          , singleLine true
          ,text $ getString $  DISCOUNT_POINTS_UPTO (coinDiscountUpto <> " ")
          ]  <> (if isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy)
        ]
    ]
   , linearLayout 
     [  height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , visibility if isActivePlan then VISIBLE else GONE
     ][ textView $ [
        text $ getString ACTIVE_PLAN
      , background Color.green900
      , color Color.white900
      , padding $ Padding 8 5 8 5
      , cornerRadius 100.0
      ] <> FontStyle.tags TypoGraphy
    ] 
  ]
  where 
    mkPlanCardConfig state = {
        id : state.id,
        title : state.title,
        description : state.description,
        isSelected : state.isSelected,
        offers : state.offers,
        priceBreakup : state.priceBreakup,
        frequency : state.frequency,
        freeRideCount : state.freeRideCount,
        showOffer : state.showOffer
    }

offerCardBannerView :: forall w. (Action -> Effect Unit) -> Boolean -> Boolean -> Boolean -> ST.OfferBanner -> Boolean -> PrestoDOM (Effect Unit) w
offerCardBannerView push useMargin visibility' isPlanCard offerBannerProps isFreezed =
  let horizontalMargin = if useMargin then 16 else 0
  in
  linearLayout
    [ height MATCH_PARENT
    , width  MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin horizontalMargin 0 horizontalMargin 0
    , visibility if visibility' && offerBannerProps.showOfferBanner && not HU.isDateGreaterThan offerBannerProps.offerBannerValidTill then VISIBLE else GONE
    , weight 1.0
    , clickable false
    , alpha if isFreezed then 0.6 else 1.0
    ][
        Banner.view (push <<< OfferCardBanner) (offerCardBannerConfig isPlanCard offerBannerProps)
    ]

promoCodeView :: forall w. (Action -> Effect Unit) -> ST.PromoConfig -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w 
promoCodeView push state isIntroductory isSelectedLangTamil =
  linearLayout 
  ([ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  , visibility if state.title == Nothing then GONE else VISIBLE
  ] <> if not isIntroductory then [stroke $ "1," <> Color.grey900] else []
    <> if state.isGradient then [gradient (Linear 90.0 state.gradient)] else [])
   [ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if state.hasImage then VISIBLE else GONE
     , imageWithFallback state.imageURL
     ] 
   , textView $
     [ color Color.blue900
     , singleLine true
     , padding $ PaddingBottom 3
     ]  <> (if isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy)
        <> case state.title of
            Nothing -> [visibility GONE]
            Just txt -> [text txt]
  ]

offerCountView :: forall w. Int -> Boolean -> PrestoDOM (Effect Unit) w
offerCountView count isSelected = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , visibility if (count > 0 && not isSelected) then VISIBLE else GONE
  ][ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , cornerRadius 100.0
      , stroke $ "1," <> Color.blue700
      , background Color.blue600
      , padding $ Padding 10 2 10 2
      , gravity CENTER_VERTICAL
      ][ imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_discount"
        , width $ V 12
        , height $ V 12
        , margin $ MarginRight 4
        ]
      , textView $
        [ text $ show count <> " " <> if count == 1 then getString OFFER else getString OFFERS
        , color Color.blue900
        , padding $ PaddingBottom 3
        ] <> FontStyle.body17 TypoGraphy
      ]
  ]

planPriceView :: forall w. Array API.PaymentBreakUp -> String -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
planPriceView fares frequency isSelectedLangTamil isIntroductory =
  let finalFee = "₹" <> (HU.getPlanPrice fares "FINAL_FEE") <> "/" <> case frequency of
                                                                    "PER_RIDE" -> getString RIDE
                                                                    "DAILY" -> getString DAY
                                                                    _ -> getString DAY
  in
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER_VERTICAL
  ][ textView $ 
     [ textFromHtml $ "<strike> ₹" <> HU.getPlanPrice fares "INITIAL_BASE_FEE" <> "</stike>"
     , visibility if (HU.getAllFareFromArray fares ["INITIAL_BASE_FEE", "FINAL_FEE"]) > 0.0 && not isIntroductory then VISIBLE else GONE
     , color Color.black600
     ] <> FontStyle.body7 TypoGraphy
   , textView $
      [ textFromHtml if isIntroductory then "<strike>" <> finalFee <> "</stike>" else finalFee
      , margin $ MarginLeft 3
      , color if isIntroductory then Color.black600 else Color.black800
      ] <> if isSelectedLangTamil then FontStyle.body4 TypoGraphy else FontStyle.body7 TypoGraphy
   ]

offerCardBannerConfig :: Boolean -> ST.OfferBanner ->  Banner.Config
offerCardBannerConfig isPlanCard bannerProps= 
  let 
    strArray = DS.split (DS.Pattern "-*$*-") bannerProps.offerBannerDeadline
    getLanguage len = do
        case getLanguageLocale languageKey of
            "KN_IN" | len > 1 -> 1
            "HI_IN" | len > 2 -> 2
            "TA_IN" | len > 3 -> 3
            "BN_IN" | len > 4 -> 4
            "ML_IN" | len > 5 -> 5
            "TE_IN" | len > 6 -> 6
            _ -> 0
    date = fromMaybe "" (strArray DA.!! (getLanguage (DA.length strArray)))
    title' = getString $ OFFER_CARD_BANNER_TITLE "OFFER_CARD_BANNER_TITLE" date bannerProps.payAmount
    config = Banner.config
    config' = config  
      {
        backgroundColor = Color.yellow800,
        title = title',
        titleColor = Color.black800,
        actionText {
          text = getString OFFER_CARD_BANNER_DESC,
          textColor = Color.black800
        },
        alertText {
          text = getString OFFER_CARD_BANNER_ALERT,
          textColor = Color.red
        },
        imageUrl = case getLanguageLocale languageKey of
                      "HI_IN" -> HU.fetchImage HU.FF_ASSET "ny_ic_autopay_setup_banner_hi"
                      "BN_IN" -> HU.fetchImage HU.FF_ASSET "ny_ic_autopay_setup_banner_bn"
                      _       -> HU.fetchImage HU.FF_ASSET "ny_ic_autopay_setup_banner",
        isBanner = true,
        alertTextVisibility = true,
        showActionArrow = false,
        bannerClickable = false,
        titleStyle = if isPlanCard then FontStyle.Body6 else FontStyle.Body7,
        imageHeight = if isPlanCard then (V 80) else (V 95),
        imageWidth = if isPlanCard then (V 98) else (V 108),
        margin = MarginTop 8,
        padding = PaddingTop 0,
        actionTextVisibility = false
      }
  in config'