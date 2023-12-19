module Screens.OnBoardingSubscriptionScreen.View where

import Prelude

import Common.Types.App (Version(..), LazyCheck(..), Event)
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int (round, toNumber, fromString)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (screenWidth, liftFlow)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Utils (getDateAfterNDays)
import JBridge (getWidthFromPercent)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Font.Style as FontStyle
import Font.Size as FontSize
import Services.Backend as Remote
import Data.Either (Either(..))
import Debug (spy)
import Types.App (defaultGlobalState)
import Engineering.Helpers.Commons as EHC
import Effect.Aff (launchAff)
import Data.Array (mapWithIndex)
import Screens.Types (PlanCardConfig, PromoConfig)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Array as DA
import Services.API (GetCurrentPlanResp(..), GetDriverInfoResp(..), OrderStatusRes(..), UiPlansResp(..), PaymentBreakUp(..), KioskLocationResp(..), KioskLocationRes(..))
import Screens.SubscriptionScreen.Controller (getAllFareFromArray, getPlanPrice)
import Components.PrimaryButton as PrimaryButton
import Storage (KeyStore(..), getValueToLocalNativeStore, getValueToLocalStore)
import JBridge as JB
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), ScopedScreen, afterRender, alpha, background, clickable, color, cornerRadius, fontSize, fontStyle, frameLayout, gradient, gravity, height, horizontalScrollView, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollBarX, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.OnBoardingSubscriptionScreen.ComponentConfig (joinPlanButtonConfig, popupModalConfig)
import Screens.OnBoardingSubscriptionScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.SubscriptionScreen.Controller (getAllFareFromArray, getPlanPrice)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (PlanCardConfig, PromoConfig)
import Screens.Types as ST
import Services.API (GetCurrentPlanResp(..), GetDriverInfoResp(..), OrderStatusRes(..), UiPlansResp(..), PaymentBreakUp(..), KioskLocationResp(..), KioskLocationRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalNativeStore, getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Components.PopUpModal as PopUpModal
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim


screen :: ST.OnBoardingSubscriptionScreenState -> ScopedScreen Action ST.OnBoardingSubscriptionScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "OnBoardingSubscriptionScreen"
  , globalEvents : [(\push -> do 
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
        uiPlans <- Remote.getUiPlans ""
        case uiPlans of
          Right plansResp -> do 
            liftFlow $ push $ LoadPlans plansResp
            pure unit
          Left err -> do 
            pure unit
      pure (pure unit)
    )]
  , eval:
      ( \state action -> do
          let _ = spy "OnBoarding SubscriptionScreen ----- state" state
          let _ = spy "OnBoarding SubscriptionScreen --------action" action
          eval state action
      )
  , parent: Nothing
  }

view :: forall w. (Action -> Effect Unit) -> ST.OnBoardingSubscriptionScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push $ const BackPressed
    ][
      scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , scrollBarY false
      ][
        linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][
          headerLayout push state
        , infoView push state
        , workFlowView push state
        -- , subscriptionPlanView push state -- TODO:: will be used in future
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , padding $ Padding 16 24 16 0
          ][
            linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , orientation VERTICAL
            ][
              commonTV push (getString $ CHOOSE_YOUR_PLAN "CHOOSE_YOUR_PLAN") Color.black800 FontStyle.subHeading1 LEFT 0 NoAction false
            , let date = getDateAfterNDays (fromMaybe 0 (fromString (getValueToLocalNativeStore FREE_TRIAL_DAYS)) -1) in 
              commonTV push ( case getValueToLocalStore LANGUAGE_KEY of 
                                "EN_US" -> (getString FREE_UNTIL <> date)
                                _ -> (date <> getString FREE_UNTIL)
                            ) Color.black700 FontStyle.body3 LEFT 0 NoAction false
            ]
          , linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , gravity RIGHT
            ][ imageView
                [ width $ V 85
                , height $ V 20
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_upi_autopay"
                ]
            ]
          ]
        , let selectedPlan = fromMaybe dummyPlanConfig state.data.selectedPlanItem in  
          linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding $ Padding 16 24 16 16
          ](map(
              (\item -> planCardView push item (item.id == selectedPlan.id) (item.id == selectedPlan.id || item.freeRideCount==0) true SelectPlan state.props.isSelectedLangTamil)
              ) state.data.plansList)
        , bottomView push state
        ]
      ]
    , if state.props.supportPopup then PrestoAnim.animationSet [ Anim.fadeIn state.props.supportPopup ] $ PopUpModal.view (push <<< PopUpModalAC) (popupModalConfig state)
      else linearLayout[visibility GONE][]
    ]

headerLayout :: forall w. (Action -> Effect Unit) -> ST.OnBoardingSubscriptionScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.black900
  , padding $ Padding 16 16 16 32
  , orientation VERTICAL
  ][
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.black900
    , gravity CENTER_VERTICAL
    ][
      linearLayout
      [
        weight 1.0
      , height WRAP_CONTENT
      ][
        imageView
        [ width $ V 30
        , height $ V 30
        , visibility GONE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
        , onClick push $ const BackPressed
        , padding $ Padding 2 2 2 2
        ]
      ]
    , linearLayout
      [
        height WRAP_CONTENT
      ][
        linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginRight 8
        , onClick push $ const CallSupport
        ][
          imageView
          [ width $ V 16
          , height $ V 16
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_phone_filled_yellow"
          , padding $ Padding 2 2 2 2
          ]
        , commonTV push (getString SUPPORT) Color.yellow900 FontStyle.body3 CENTER 0 CallSupport false
        ]
      , linearLayout
        [ height MATCH_PARENT
        , width $ V 1
        , background Color.white900
        , margin $ Margin 0 2 0 1
        , alpha 0.3][]
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginLeft 8
        , onClick push $ const GoToRegisteration
        ][
          imageView
          [ width $ V 16
          , height $ V 16
          , imageWithFallback $ fetchImage FF_ASSET "ic_chevrons_right_white"
          , padding $ Padding 2 2 2 2
          , margin $ MarginRight 5
          ]
        , commonTV push (getString SKIP) Color.white900 FontStyle.body3 CENTER 0 GoToRegisteration false
        ]
      ]
    ]
  , commonTV push (getString $ MY_PLAN_TITLE "MY_PLAN_TITLE") Color.white900 FontStyle.h1 LEFT 20 NoAction false
  ]

infoView :: forall w. (Action -> Effect Unit) -> ST.OnBoardingSubscriptionScreenState -> PrestoDOM (Effect Unit) w
infoView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 16
  , background Color.blue650
  , cornerRadii $ Corners 24.0 false false true true
  , gravity CENTER
  ][
    commonTV push (getString SEVEN_DAY_FREE_TRIAL_ACTIVATED) Color.black900 FontStyle.h2 CENTER 0 NoAction false
  , commonTV push (getString TAKE_UNLIMITED_RIDES_FOR_THE_NEXT_SEVEN_DAYS) Color.black700 FontStyle.subHeading2 CENTER 1 NoAction false
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gradient (Linear 90.0 [Color.yellow500 , Color.yellow700])
    , cornerRadius 12.0
    , margin $ MarginTop 16
    , orientation HORIZONTAL
    , padding $ Padding 16 16 16 16
    , gravity CENTER_VERTICAL
    ][
      linearLayout
      [ height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , width $ V $ round (toNumber (screenWidth unit - 66)*0.5 )
      ][ 
        imageView
        [ width $ V 32
        , height $ V 32
        , imageWithFallback $ fetchImage FF_ASSET "ic_creative_zero"
        ]
      , commonTV push (getString EVERY_RIDE_AT_ZERO_COMMISSION) Color.black900 FontStyle.h3 CENTER 0 NoAction false
      ]
    , linearLayout
      [ width (V 1)
      , height (V 56)
      , background Color.white900
      , alpha 0.2
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      , width $ V $ round (toNumber (screenWidth unit - 64)*0.5 )
      ][ 
        imageView
        [ width $ V 32
        , height $ V 32
        , imageWithFallback $ fetchImage FF_ASSET "ic_mingcute_currency_rupee"
        ]
      , commonTV push (getVarString EARN_UPTO_PER_DAY [show state.data.subscriptionConfig.earnAmountInADay]) Color.black900 FontStyle.h3 CENTER 0 NoAction false
      ]
    ]
  ]

workFlowView :: forall w. (Action -> Effect Unit) -> ST.OnBoardingSubscriptionScreenState -> PrestoDOM (Effect Unit) w
workFlowView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 24 16 0
  , padding $ Padding 20 20 20 20
  , background Color.blue600
  , orientation VERTICAL
  , cornerRadius 8.0
  ][
    commonTV push (getString HOW_THIS_WORKS) Color.black800 FontStyle.subHeading1 LEFT 0 NoAction false
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 16
    ][
      imageView
      [ width $ V 24
      , height $ V 146
      , imageWithFallback $ fetchImage FF_ASSET "ic_subscription_flow"
      ]
    , let lang = getValueToLocalStore LANGUAGE_KEY
          check = lang == "KN_IN" || lang == "TA_IN"
          font2 = if check then FontStyle.body16 else FontStyle.body3 in
      linearLayout
      [ weight 1.0
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginLeft 16
      ][
        commonTV push (getString TODAY) Color.black800 FontStyle.body1 LEFT 0 NoAction false
      , commonTV push (getString SIGN_UP_FOR_AUTOPAY_BY_PAYING_JUST) Color.black700 font2 LEFT 0 NoAction false
      , commonTV push (getString FREE_TRIAL_REMINDER) Color.black800 FontStyle.body1 LEFT 25 NoAction false
      , commonTV push (getString GET_REMINDED_ABOUT_YOUR_PLAN_SETUP) Color.black700 font2 LEFT 0 NoAction false
      , commonTV push (getString PLAN_STARTS) Color.black800 FontStyle.body1 LEFT 25 NoAction false
      , commonTV push (getString EASY_AUTOMATIC_PAYMENTS_START) Color.black700 font2 LEFT 0 NoAction false
      ]
    ]
  ]

  -- TODO:: will be used in future
-- subscriptionPlanView :: forall w. (Action -> Effect Unit) -> ST.OnBoardingSubscriptionScreenState -> PrestoDOM (Effect Unit) w
-- subscriptionPlanView push state =
--   linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   , padding $ Padding 16 24 16 16
--   ][
--     linearLayout
--     [ width MATCH_PARENT
--     , height WRAP_CONTENT
--     , gravity CENTER_VERTICAL
--     ][
--       linearLayout
--       [ height WRAP_CONTENT
--       , weight 1.0
--       , orientation VERTICAL
--       ][
--         commonTV push (getString CHOOSE_YOUR_PLAN) Color.black800 FontStyle.subHeading1 LEFT 0 NoAction false
--       , commonTV push "Free until D7/MM/YY." Color.black700 FontStyle.body3 LEFT 0 NoAction false
--       ]
--     , linearLayout
--       [ weight 1.0
--       , height WRAP_CONTENT
--       , gravity RIGHT
--       ][ imageView
--           [ width $ V 85
--           , height $ V 20
--           , imageWithFallback "ny_ic_upi_autopay,"
--           ]
--       ]
--     ]
--   , scrollView
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , orientation HORIZONTAL
--     , margin $ MarginTop 24
--     ][
--       linearLayout
--       [ height WRAP_CONTENT
--       , width MATCH_PARENT
--       ](mapWithIndex 
--             (\index item -> planCardView push item (index == state.props.selectedPlanIndex) index
--         ) state.data.plansList)
--     ]
--   ]

-- planCardView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> Int-> PrestoDOM (Effect Unit) w
-- planCardView push state isSelected index =
--   frameLayout
--   [ height WRAP_CONTENT
--   , width $ V $ round (toNumber (screenWidth unit - 45)*0.5 )
--   , margin if index ==0 then  MarginRight 12 else MarginRight 0
--   ][
--     linearLayout
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , orientation VERTICAL
--     , padding $ PaddingTop 10
--     ][
--       linearLayout
--       [ height WRAP_CONTENT
--       , width MATCH_PARENT
--       , background if isSelected then Color.blue600 else Color.white900
--       , stroke $ "1," <> (if isSelected then Color.blue800 else Color.grey900)
--       , padding $ Padding 20 20 24 20
--       , cornerRadius 8.0
--       , orientation VERTICAL
--       , onClick push $ const $ SelectPlan index
--       ][ 
--         linearLayout
--         [ height WRAP_CONTENT
--         , width MATCH_PARENT
--         , orientation VERTICAL
--         ][
--           textView
--           [ height WRAP_CONTENT
--           , width MATCH_PARENT
--           , gravity LEFT
--           , text state.title
--           , color if isSelected then Color.blue900 else Color.black700
--           , textSize FontSize.a_16
--           , fontStyle $ FontStyle.bold LanguageStyle
--           ]
--         , horizontalScrollView 
--           [ height WRAP_CONTENT
--           , width MATCH_PARENT
--           , scrollBarX false
--           , margin $ MarginVertical 8 8
--           , visibility if (DA.length state.offers > 0) then VISIBLE else GONE
--           ][ linearLayout
--             [ height WRAP_CONTENT
--             , width MATCH_PARENT
--             , orientation HORIZONTAL
--             ](map  (\item -> 
--                     linearLayout
--                     [ height WRAP_CONTENT
--                     , width MATCH_PARENT
--                     , margin if item.title == Nothing then MarginTop 25 else MarginTop 0
--                     ][
--                       promoCodeView push item
--                     ]
--               ) state.offers)
--           ]
--         , planPriceView state.priceBreakup state.frequency
--         , textView
--           [ height WRAP_CONTENT
--           , width MATCH_PARENT
--           , text state.description
--           , textSize FontSize.a_12
--           , fontStyle $ FontStyle.medium LanguageStyle
--           , color Color.black600
--           , weight 1.0
--           , margin $ MarginTop 8
--           ]
--         ]
--       ]
--     ]
--   , linearLayout
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , gravity CENTER
--     , visibility if index == 0 then VISIBLE else GONE
--     ][
--       textView
--       [ height WRAP_CONTENT
--       , width WRAP_CONTENT
--       , text "POPULAR"
--       , background Color.blue900
--       , cornerRadius 100.0
--       , padding $ Padding 10 4 10 4
--       , color Color.white900
--       , textSize FontSize.a_10
--       , fontStyle $ FontStyle.medium LanguageStyle
--       ]
--     ]
--   ]

planCardView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> Boolean -> Boolean -> (PlanCardConfig -> Action) -> Boolean -> PrestoDOM (Effect Unit) w
planCardView push state isSelected isExpanded clickable' action isSelectedLangTamil =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , stroke $ "1," <> (if isSelected then Color.blue800 else Color.grey900)
  , padding $ Padding 16 12 16 12
  , cornerRadius 8.0
  , orientation VERTICAL
  , margin $ MarginBottom 16
  , clickable clickable'
  , onClick push $ const $ action state
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , gravity CENTER_VERTICAL
     , margin $ MarginBottom 5
     ][ textView
        [ text state.title
        , textSize if isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
        , weight 1.0
        , fontStyle $ (if isSelected then FontStyle.bold else FontStyle.semiBold) LanguageStyle
        , color if isSelected then Color.blue900 else Color.black700
        ]
      , planPriceView state.priceBreakup state.frequency isSelectedLangTamil
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ textView
         [ text state.description
         , textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
         , fontStyle $ FontStyle.medium LanguageStyle
         , color Color.black600
         , weight 1.0
         ]
       , if state.showOffer then offerCountView (DA.length state.offers) isExpanded else linearLayout[visibility GONE][]
       ]
    , horizontalScrollView 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , scrollBarX false
      , margin $ MarginVertical 8 8
      , visibility if isExpanded && (DA.length state.offers > 0) then VISIBLE else GONE
      ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ](map  (\item -> promoCodeView push item) state.offers)
       ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility if isExpanded && (DA.length state.offers > 0) then VISIBLE else GONE
      ](map (\item ->
          linearLayout
            ([ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding $ Padding 8 8 8 8
            , margin $ MarginTop if isExpanded then 0 else 8
            , background Color.grey700
            , cornerRadius 4.0
            ] <> case item.offerDescription of 
                  Just desc -> [text desc, visibility if isExpanded then VISIBLE else GONE]
                  Nothing -> [visibility GONE])
            [ textView
              [ textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
              , textFromHtml $ fromMaybe "" item.offerDescription
              , fontStyle $ FontStyle.regular LanguageStyle
              , color Color.black600
              , lineHeight "20"
              ]
            ]
         )state.offers)
    ]

offerCountView :: forall w. Int -> Boolean -> PrestoDOM (Effect Unit) w
offerCountView count isSelected = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , stroke $ "1," <> Color.blue700
  , background Color.blue600
  , padding $ Padding 10 2 10 2
  , visibility if (count > 0 && not isSelected) then VISIBLE else GONE
  , gravity CENTER_VERTICAL
  ][ imageView
     [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_discount"
     , width $ V 12
     , height $ V 12
     , margin $ MarginRight 4
     ]
   , textView
     [ text $ show count <> " " <> if count == 1 then getString OFFER else getString OFFERS
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.semiBold LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  ]


promoCodeView :: forall w. (Action -> Effect Unit) -> PromoConfig -> PrestoDOM (Effect Unit) w 
promoCodeView push state =
  linearLayout 
  ([ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  , visibility if state.title == Nothing then GONE else VISIBLE
  ]<> if state.isGradient then [gradient (Linear 90.0 state.gradient)] else [])
   [ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if state.hasImage then VISIBLE else GONE
     , imageWithFallback state.imageURL
     ] 
   , textView $
     [ textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , singleLine true
     , padding $ PaddingBottom 3
     ] <> case state.title of
            Nothing -> [visibility GONE]
            Just txt -> [text txt]
  ]

planPriceView :: forall w. Array PaymentBreakUp -> String -> Boolean -> PrestoDOM (Effect Unit) w
planPriceView fares frequency isSelectedLangTamil =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][ textView $ 
     [ textFromHtml $ "<strike> ₹" <> getPlanPrice fares "INITIAL_BASE_FEE" <> "</stike>"
     , visibility if (getAllFareFromArray fares ["INITIAL_BASE_FEE", "FINAL_FEE"]) > 0.0 then VISIBLE else GONE
     , color Color.black600
     ] <> FontStyle.body7 TypoGraphy
   , textView
      [ text $ "₹" <> (getPlanPrice fares "FINAL_FEE") <> "/" <> case frequency of
                                                                    "PER_RIDE" -> getString RIDE
                                                                    "DAILY" -> getString DAY
                                                                    _ -> getString DAY
      , textSize if isSelectedLangTamil then FontSize.a_14 else FontSize.a_16
      , fontStyle $ FontStyle.bold LanguageStyle
      , margin $ MarginLeft 3
      , color Color.black800
      ]
   ]

  -- TODO:: will be used in future
-- planPriceView :: forall w. Array PaymentBreakUp -> String -> PrestoDOM (Effect Unit) w
-- planPriceView fares frequency =
--   linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   ][ 
--     textView $ 
--     [ textFromHtml $ "<strike> ₹" <> getPlanPrice fares "INITIAL_BASE_FEE" <> "</stike>"
--     , visibility if (getAllFareFromArray fares ["INITIAL_BASE_FEE", "FINAL_FEE"]) > 0.0 then VISIBLE else GONE
--     , color Color.black600
--     ] <> FontStyle.body7 TypoGraphy
--   , textView
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , gravity LEFT
--     , text $ "₹" <> (getPlanPrice fares "FINAL_FEE")
--     , textSize FontSize.a_32
--     , fontStyle $ FontStyle.bold LanguageStyle
--     , color Color.black800
--     ]
--   , textView
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , gravity LEFT
--     , text $ case frequency of
--               "PER_RIDE" -> getString PER_RIDE
--               "DAILY" -> getString PER_DAY
--               _ -> getString DAY
--     , textSize FontSize.a_12
--     , fontStyle $ FontStyle.bold LanguageStyle
--     , color Color.black800
--     ]
--   ]

bottomView :: forall w. (Action -> Effect Unit) -> ST.OnBoardingSubscriptionScreenState -> PrestoDOM (Effect Unit) w
bottomView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][
    linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    , margin $ MarginBottom 16
    ][]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 0 16 16 
    , orientation VERTICAL
    , gravity CENTER
    ][
      PrimaryButton.view (push <<< JoinPlanAC) (joinPlanButtonConfig state)
    ]
  ]

underlinedTextView :: String -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
underlinedTextView value push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    , onClick push $ const GoToRegisteration
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text value
          , color Color.black650
          ]
        <> FontStyle.body5 TypoGraphy
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background  Color.black650
        ]
        []
    ]


commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> Boolean-> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action txtFromHtml = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , onClick push $ const action
  , (if txtFromHtml then textFromHtml else text) text'
  ] <> theme TypoGraphy
