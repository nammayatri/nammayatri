module Components.ChooseYourRide.View where

import Common.Types.App
import Debug
import Animation (translateYAnim,translateYAnimFromTop, fadeIn)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as Animation
import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide.Controller (Action(..), Config, BookAnyProps, bookAnyProps)
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex, length, (!!), filter, nubBy, any, foldl, filter, elem, null)
import Data.Function.Uncurried (runFn1)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import JBridge (getLayoutBounds, getHeightFromPercent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<>), const, pure, unit, bind, not, show, bind, negate, (<<<), (==), (>=), (*), (+), (<=), (&&), (/), (>), (||), (-), map, (/=), (<), (<>))
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), Shadow(..), Gradient(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd, disableClickFeedback, accessibility, peakHeight, halfExpandedRatio, relativeLayout, topShift, bottomShift, alignParentBottom, imageWithFallback, shadow, clipChildren, layoutGravity, accessibilityHint, horizontalScrollView, scrollBarX, disableKeyboardAvoidance, singleLine, maxLines, textFromHtml, gradient, frameLayout, enableShift, nestedScrollView, shimmerFrameLayout)
import PrestoDOM.Properties (cornerRadii)
import Data.Tuple (Tuple(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import ConfigProvider
import PrestoDOM.Properties (sheetState)
import Data.Int (toNumber,ceil, fromString)
import MerchantConfig.Types(AppConfig(..))
import Mobility.Prelude
import Screens.Types (ZoneType(..), TipViewStage(..), FareProductType(..))
import Data.Ord(min, max)
import Resources.Constants (intMin, intMax)
import Helpers.SpecialZoneAndHotSpots as HS
import Data.Array (groupBy, head, sortBy, fromFoldable, all)
import Data.Maybe 
import Data.Ord (comparing)
import Data.Traversable (traverse)
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (compare)
import Data.Function (on)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
    [ width MATCH_PARENT
    , height $ V $ EHC.screenHeight unit
    , clipChildren false
    , gravity BOTTOM
    , orientation VERTICAL
    ]
    [ linearLayout
      [ height $ WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      ][ coordinatorLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][bottomSheetLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , accessibility DISABLE
          , enableShift false
          , peakHeight $ if null config.quoteList then 300 else getPeekHeight config 
          , sheetState COLLAPSED
          , orientation VERTICAL
          ][linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ chooseYourRideView push config 
            ]
          ]
        ]
      , if EHC.os == "IOS" then bottomLayoutView push config VISIBLE "BottomLayoutView" else linearLayout[][]
      ]
    , if EHC.os /= "IOS" then bottomLayoutView push config VISIBLE "BottomLayoutView" else linearLayout[][]
    ]
  where
    getPeekHeight :: Config -> Int
    getPeekHeight config = 
      let variantBasedList = filterVariantAndEstimate config.quoteList
          topProviderList = filter (\element -> element.providerType == ONUS) config.quoteList
          currentPeekHeight = getQuoteListViewHeight config $ length if config.showMultiProvider then variantBasedList else topProviderList 
      in if currentPeekHeight == 0 then 470 else currentPeekHeight

addTipView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
addTipView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.ivory
  , cornerRadius 12.0
  , padding $ Padding 20 12 20 12
  , margin $ MarginTop 16
  , gravity CENTER
  , clickable true
  , afterRender push $ const $ NoAction
  , onClick push $ const $ if state.tipViewProps.stage == DEFAULT then AddTip else NoAction 
  , visibility $ boolToVisibility state.enableTips
  ] $ (case state.tipViewProps.stage of 
          DEFAULT -> [defaultTipView push state]
          TIP_AMOUNT_SELECTED -> [selectTipView push state]
          RETRY_SEARCH_WITH_TIP -> [tipSelectedView push state]
          _ -> [defaultTipView push state])

bottomLayoutView :: forall w. (Action -> Effect Unit) -> Config -> Visibility -> String -> PrestoDOM (Effect Unit) w 
bottomLayoutView push config visibility' id' = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , id $ EHC.getNewIDWithTag id'
  , visibility visibility'
  , alignParentBottom "true,-1"
  , clickable true
  , afterRender push $ const $ NoAction
  , padding $ Padding 16 (if config.showPreferences then 16 else 0) 16 16
  , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5 
  ][ addTipView push config
   , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config "PrimaryButtomConfirm") 
   ]
  
defaultTipView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
defaultTipView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , onClick push $ const $ AddTip
  , gravity CENTER
  , clickable true
  ][textView $ 
    [ textFromHtml $ (getString A_TIP_HELPS_FIND_A_RIDE_QUICKER) <> " " <> "<span style='color:#0066FF'>" <> (getString ADD_NOW) <> "</span>"
    , width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.black900
    , singleLine false
    , maxLines 2
    ] <> FontStyle.body1 LanguageStyle
  ]

tipSelectedView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tipSelectedView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  , clickable true
  , onClick push $ const $ ChangeTip
  ][ textView $ 
    [ text $ "₹" <> (show $ state.tipForDriver) <> " " <> getString TIP_ADDED
    , color Color.black900
    , width WRAP_CONTENT
    , height WRAP_CONTENT
    ] <> FontStyle.body4 LanguageStyle
  , textView $ 
    [ text $ getString CHANGE
    , width WRAP_CONTENT
    , height WRAP_CONTENT
    , color Color.blue900
    , margin $ MarginLeft 4
    ] <> FontStyle.body1 LanguageStyle
  ]

selectTipView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
selectTipView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  ][ textView $ 
    [ text $ getString A_TIP_HELPS_FIND_A_RIDE_QUICKER
    , color Color.black900
    , margin $ MarginBottom 12
    ] <> FontStyle.body1 LanguageStyle
  , tipsHorizontalView push state
  ]

tipsHorizontalView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tipsHorizontalView push state = 
  PrestoAnim.animationSet [fadeIn true] $
  horizontalScrollView
  [ height WRAP_CONTENT
  , width $ V ((EHC.screenWidth unit) - 72) 
  , scrollBarX false
  , disableKeyboardAvoidance true
  ][linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT 
    ]
    ( mapWithIndex
      ( \index item ->
          linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          ][ linearLayout
            [ width $ if index == 0 then WRAP_CONTENT else V 84
            , height $ V 36
            , background Color.white900
            , margin $ MarginLeft if index == 0 then 0 else 8
            , cornerRadius 7.0
            , gravity CENTER
            , padding $ if index == 0 then PaddingHorizontal 8 8 else Padding 0 0 0 0
            , stroke $ "1," <> (if (state.tipViewProps.activeIndex == index) then Color.blue800 else Color.grey900)
            , onClick push $ const $ TipBtnClick index (fromMaybe 0 (state.customerTipArrayWithValues !! index))
            , accessibility ENABLE
            , accessibilityHint $ "₹" <> show (fromMaybe 0 (state.customerTipArrayWithValues !! index)) <> " Tip"<> (if (state.tipViewProps.activeIndex == index) then " Selected" else " : Button")
            ][textView $ 
              [ text $ item
              , color $ Color.black800
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , lineHeight "12"
              , accessibility DISABLE
              ] <> FontStyle.body6 LanguageStyle
            ]
          ]
      ) state.customerTipArray
    )
  ]
 
bottomDummyView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
bottomDummyView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , id $ EHC.getNewIDWithTag "bottomButtonLayout"
    , padding $ Padding 16 10 16 16
    ][ linearLayout
        [ width MATCH_PARENT
        , height $ V if EHC.os == "IOS" then 70 else 24
        , visibility $ boolToVisibility $ config.bookingPreferenceEnabled
        ][]
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config "DummyPrimaryConfirm")
    ]

bookingPreferencesView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
bookingPreferencesView push config = 
  let globalConfig = getAppConfig appConfig
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ boolToVisibility config.bookingPreferenceEnabled 
  ]
  [ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ PaddingVertical 12 (if config.showPreferences then 12 else 0)
    , stroke $ if config.showPreferences then "1," <> Color.grey900 else "0," <> Color.grey900
    , cornerRadius 8.0
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_HORIZONTAL
      , onClick push $ const PreferencesDropDown
      , accessibility DISABLE
      , clickable true
      ]
      [ textView $
        [ height $ V 24
        , width WRAP_CONTENT
        , color Color.darkCharcoal
        , text $ getString BOOKING_PREFERENCE
        , accessibility DISABLE
        ] <> FontStyle.body1 LanguageStyle
      , imageView
        [ width $ V 10
        , height $ V 10
        , margin (Margin 9 5 0 0)
        , accessibility DISABLE
        , imageWithFallback $ if config.showPreferences
                    then HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_up"
                    else HU.fetchImage HU.FF_ASSET "ny_ic_chevron_down"
        ]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginLeft 20
      , orientation VERTICAL
      ]
      [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility $ boolToVisibility config.showPreferences 
        ]
        [ showMenuButtonView push (getString AUTO_ASSIGN_DRIVER) (HU.fetchImage HU.FF_ASSET "ny_ic_faster_lightning") true config globalConfig
        , showMenuButtonView push (getString CHOOSE_BETWEEN_MULTIPLE_DRIVERS) (HU.fetchImage HU.FF_ASSET "ny_ic_info") false config globalConfig
        ]
      ]
    ]
  ]

showMenuButtonView :: forall w. (Action -> Effect Unit) -> String -> String -> Boolean -> Config -> AppConfig -> PrestoDOM (Effect Unit) w
showMenuButtonView push menuText menuImage autoAssign componentConfig appConfig =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  , padding $ PaddingVertical 10 10
  , onClick push $ const $ RadioButtonClick autoAssign
  ]
  [ let isAutoAssign = componentConfig.flowWithoutOffers && autoAssign || not componentConfig.flowWithoutOffers && not autoAssign
    in
    linearLayout
    [ height $ V 20
    , width $ V 20
    , stroke if isAutoAssign then "2," <> appConfig.primaryBackground else "2," <> Color.black600
    , cornerRadius 10.0
    , gravity CENTER
    
    ]
    [ linearLayout
      [ width $ V 10
      , height $ V 10
      , cornerRadius 5.0
      , background $ appConfig.primaryBackground
      , visibility $ boolToVisibility isAutoAssign
      ]
      []
    ]
  , textView $
    [ text menuText
    , width MATCH_PARENT
    , gravity CENTER
    , color appConfig.estimateAndQuoteConfig.textColor
    , height WRAP_CONTENT
    , margin $ MarginHorizontal 10 10
    ] <> FontStyle.paragraphText LanguageStyle
  , if autoAssign 
      then autoAssignInfoView push menuImage autoAssign appConfig
      else multipleOfferInfoView push menuImage autoAssign
  ]

autoAssignInfoView :: forall w. (Action -> Effect Unit) -> String -> Boolean -> AppConfig -> PrestoDOM (Effect Unit) w
autoAssignInfoView push menuImage autoAssign appConfig =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , background appConfig.autoSelectBackground
    , cornerRadius 14.0
    , gravity CENTER
    , padding $ Padding 10 6 10 6
    ]
    [ imageView
      [ height $ V 12
      , width $ V 8
      , margin $ MarginRight 4
      , imageWithFallback menuImage
      ]
    , textView $
      [ text $ getString FASTER
      , width WRAP_CONTENT
      , gravity CENTER
      , color Color.white900
      , height WRAP_CONTENT
      ] <> FontStyle.body15 LanguageStyle
    ]

multipleOfferInfoView :: forall w. (Action -> Effect Unit) -> String -> Boolean -> PrestoDOM (Effect Unit) w
multipleOfferInfoView push menuImage autoAssign =
  imageView
    [ height $ V 16
    , width $ V 16
    , imageWithFallback menuImage
    , margin $ MarginHorizontal 5 5
    , onClick push $ const $ OnIconClick autoAssign
    ]

chooseYourRideView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
chooseYourRideView push config =
  let estimateConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      anims = if EHC.os == "IOS"
              then [fadeIn true]
              else [translateYAnimFromTop $ Animation.chooseRideAnimConfig]
      tagConfig = HS.specialZoneTagConfig config.zoneType
      showTag = any (_ == config.zoneType) [SPECIAL_PICKUP, METRO]
  in
  PrestoAnim.animationSet anims $
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , onAnimationEnd push (const NoAction)
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity RIGHT
      , margin $ MarginRight 15
      , visibility $ boolToVisibility $ isJust config.nearByDrivers && estimateConfig.showNearByDrivers 
      , disableClickFeedback true
      ][ textView
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , gravity RIGHT
         , stroke $ "1," <> Color.grey900
         , text $ show (fromMaybe 0 config.nearByDrivers) <> " " <> (getString CABS_AVAILABLE)
         , padding (Padding 10 5 10 5)
         , color Color.blue900
         , background Color.white900
         , cornerRadius 8.0
         , fontStyle $ FontStyle.medium LanguageStyle
         ]
       ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , background tagConfig.backgroundColor
      , cornerRadii $ Corners 24.0 true true false false
      ][linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER
        , background tagConfig.backgroundColor
        , padding (PaddingVertical 4 4)
        , cornerRadii $ Corners 24.0 true true false false
        , visibility $ boolToVisibility showTag
        , clickable $ isJust tagConfig.infoPopUpConfig
        , onClick push $ const $ SpecialZoneInfoTag
        ] [ imageView
            [ width (V 20)
            , height (V 20)
            , margin (MarginRight 6)
            , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET tagConfig.icon
            ]
          , textView
            [ width if EHC.os == "IOS" && config.zoneType == AUTO_BLOCKED then (V 230) else WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , textSize FontSize.a_14
            , text tagConfig.text
            , color Color.white900
            ]
          , imageView
            [ width (V 18)
            , height (V 18)
            , visibility $ boolToVisibility $ isJust tagConfig.infoPopUpConfig
            , margin (MarginLeft 6)
            , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_white_info"
            ]
          ] 
      , linearLayout
        [ orientation VERTICAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , clickable true
        , padding $ PaddingTop if EHC.os == "IOS" then 13 else 7
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        , cornerRadii $ Corners 24.0 true true false false
        ][ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , id $ EHC.getNewIDWithTag "rideEstimateHeaderLayout"
            ][linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER_HORIZONTAL
                ][linearLayout
                  [background Color.transparentGrey
                  , height $ V 4
                  , width $ V 34
                  , margin (MarginVertical 4 4)
                  , cornerRadius if EHC.os == "IOS" then 2.0 else 4.0
                  ][]
                ] 
              , textView (
                [ text 
                    if length config.quoteList > 1 
                    then (getString CHOOSE_YOUR_RIDE)
                    else (getString CONFIRM_YOUR_RIDE)
                , color Color.black800
                , gravity CENTER_HORIZONTAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                ] <> FontStyle.h1 TypoGraphy)
                , estimatedTimeAndDistanceView push config
                , textView $
                  [ textFromHtml $ getString TOLL_CHARGES_WILL_BE_EXTRA
                  , color Color.black650
                  , gravity CENTER_HORIZONTAL
                  , height WRAP_CONTENT
                  , gravity CENTER_HORIZONTAL
                  , width MATCH_PARENT
                  , visibility $ boolToVisibility $ config.showTollExtraCharges && maybe false (\item -> not $ item.serviceTierName == Just "Auto" || (item.vehicleVariant == "BOOK_ANY"  && all (_ ==  "Auto") item.selectedServices )) (config.quoteList !! config.activeIndex)
                  ] <> FontStyle.paragraphText TypoGraphy
                , linearLayout
                  [ height $ V 1
                  , width MATCH_PARENT
                  , margin $ MarginTop 12
                  , background Color.grey900
                  ][]
                ]
          , textView $
            [ text $ getString SHOWING_FARE_FROM_MULTI_PROVIDER
            , color Color.black700
            , height WRAP_CONTENT
            , gravity CENTER
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 0
            , padding $ Padding 12 12 12 12
            , background Color.blue600
            , cornerRadius 8.0
            , visibility $ boolToVisibility config.showMultiProvider
            ] <> FontStyle.paragraphText TypoGraphy
          , quoteListView push config 
          ]  
       ]
  ]

estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
estimatedTimeAndDistanceView push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginTop 4
    ]
    [ textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDistance
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.5
        , background Color.black600
        , margin (Margin 6 2 6 0)
        ]
        []
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDuration
        , color Color.black650
        ]
        <> FontStyle.paragraphText TypoGraphy
    ]

quoteListView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
quoteListView push config =
  let variantBasedList = filterVariantAndEstimate config.quoteList
      topProviderList = filter (\element -> element.providerType == ONUS) config.quoteList
      viewHeight = getScrollViewHeight config $ length if config.showMultiProvider then variantBasedList else topProviderList
  in 
  frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push (const NoAction)
    , margin $ MarginBottom if EHC.os == "IOS" then 44 else 0
    ]
    [scrollView
      [ nestedScrollView $ length config.quoteList > 3
      , width MATCH_PARENT
      , height $ V if null config.quoteList then 200 else viewHeight
      ]
      [  if null (config.quoteList) then 
            shimmerFrameLayout 
            [ width MATCH_PARENT
            , height $ V 200
            , orientation VERTICAL
            , background Color.transparent
            ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , padding $ PaddingHorizontal 16 16
                , orientation VERTICAL
                ] (mapWithIndex (\index _ -> 
                      shimmerItemView (index == 0)) [1,2,3] )]
            else 
              Keyed.linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , padding $ PaddingVertical 16 10
              , margin $ MarginHorizontal 16 16
              , orientation VERTICAL
              ][  
                (if config.showMultiProvider  then 
                    Tuple "MultiProvider" $ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    ] $ mapWithIndex
                        ( \index item -> do
                            let estimates = if item.vehicleVariant == "BOOK_ANY" then filter (\quote -> elem (fromMaybe "" quote.serviceTierName) item.selectedServices) variantBasedList else []
                                services = if item.vehicleVariant == "BOOK_ANY" then HU.getAllServices FunctionCall else []
                                bookAnyConfig = getBookAnyProps item estimates
                                price = getMinMaxPrice bookAnyConfig item estimates
                                capacity = getMinMaxCapacity bookAnyConfig item estimates
                            ChooseVehicle.view (push <<< ChooseVehicleAC) (item{selectedEstimateHeight = config.selectedEstimateHeight, price = price, showInfo = true, capacity = capacity, singleVehicle = (length variantBasedList == 1), currentEstimateHeight = config.currentEstimateHeight, services = services})
                        ) variantBasedList
                  else 
                    Tuple "TopProvider" $ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    ] $ mapWithIndex
                        ( \index item -> do
                            let estimates = if item.vehicleVariant == "BOOK_ANY" then filter (\quote -> elem (fromMaybe "" quote.serviceTierName) item.selectedServices) topProviderList else []
                                services = if item.vehicleVariant == "BOOK_ANY" then HU.getAllServices FunctionCall else []
                                bookAnyConfig = getBookAnyProps item estimates
                                price = getMinMaxPrice bookAnyConfig item estimates
                                capacity = getMinMaxCapacity bookAnyConfig item estimates
                            ChooseVehicle.view (push <<< ChooseVehicleAC) (item{selectedEstimateHeight = config.selectedEstimateHeight, price = price, showInfo = true, capacity = capacity, singleVehicle = (length topProviderList == 1), currentEstimateHeight = config.currentEstimateHeight, services = services})
                        ) topProviderList)
           , if EHC.os /= "IOS" then bottomLayoutViewKeyed push config "BottomLayoutView" else Tuple "EmptyLL" $ linearLayout[][]-- TODO:: Temporary fix, should make scrollable list better
          ]
      ]
    -- , linearLayout -- TODO:: Temporary removing gradient for estimates
    --   [ height $ WRAP_CONTENT
    --   , width $ MATCH_PARENT
    --   , gravity TOP_VERTICAL
    --   , accessibility DISABLE
    --   ][linearLayout
    --     [ height $ V 20
    --     , width MATCH_PARENT
    --     , accessibility DISABLE
    --     , gradient $ if EHC.os == "IOS" then (Linear 90.0 [Color.transparent, Color.transparentMid , Color.white900]) else (Linear 180.0 [Color.white900, Color.transparentMid, Color.transparent])
    --     ][]  
    --   ]
    
    --  , linearLayout
    --   [ height $ MATCH_PARENT
    --   , width $ MATCH_PARENT
    --   , gravity BOTTOM
    --   , clickable false
    --   , accessibility DISABLE
    --   ][linearLayout
    --     [ height $ V 20
    --     , width MATCH_PARENT
    --     , accessibility DISABLE
    --     , gradient $ if EHC.os == "IOS" then (Linear 90.0 [Color.white900, Color.transparentMid , Color.transparent]) else Linear 180.0 [Color.transparent, Color.transparentMid, Color.white900] -- $ if EHC.os == "IOS" then (Linear 90.0 [Color.white900, Color.transparentMid , Color.transparent])
    --     ][]  
    --   ]
    ]
  where
    bottomLayoutViewKeyed push config id' = 
      Tuple "EXTRA" $ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , visibility INVISIBLE
      , orientation VERTICAL
      , background Color.white900
      , id $ EHC.getNewIDWithTag id'
      , alignParentBottom "true,-1"
      , clickable true
      , afterRender push $ const $ NoAction
      , padding $ Padding 16 (if config.showPreferences then 16 else 0) 16 16
      , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5 
      ][ addTipView push config
      , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config "KeyedButtonPrimary") 
      ]

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

getBookAnyProps :: ChooseVehicle.Config -> Array ChooseVehicle.Config -> BookAnyProps
getBookAnyProps quote estimates = foldl (\acc item -> getMinMax acc item) bookAnyProps estimates
  where 
    getMinMax :: BookAnyProps -> ChooseVehicle.Config -> BookAnyProps
    getMinMax bookAnyProps item = 
      let minPrice = bookAnyProps.minPrice `min` (fromMaybe intMax item.minPrice)
          maxPrice = bookAnyProps.maxPrice `max` (fromMaybe intMin item.maxPrice)
          minCapacity = bookAnyProps.minCapacity `min` (fromMaybe intMax (fromString item.capacity))
          maxCapacity = bookAnyProps.maxCapacity `max` (fromMaybe intMin (fromString item.capacity))
      in bookAnyProps{minPrice = minPrice, maxPrice = maxPrice, minCapacity = minCapacity, maxCapacity = maxCapacity}

getMinMaxPrice :: BookAnyProps -> ChooseVehicle.Config -> Array ChooseVehicle.Config -> String
getMinMaxPrice bookAnyProps quote estimates =
  let currency = getCurrency appConfig
  in case (length estimates), quote.vehicleVariant == "BOOK_ANY" of 
      0, true -> "-"
      1, true -> (fromMaybe ChooseVehicle.config (estimates !! 0)).price
      _, true -> case bookAnyProps.minPrice <= 0, bookAnyProps.maxPrice <= 0 of  
              false,false -> if bookAnyProps.minPrice == bookAnyProps.maxPrice then quote.price
                                else (currency <> (show bookAnyProps.minPrice) <> " - " <> currency <> (show bookAnyProps.maxPrice))
              _,_ -> quote.price
      _ , false -> quote.price
      _,_ -> "-"

getMinMaxCapacity :: BookAnyProps -> ChooseVehicle.Config -> Array ChooseVehicle.Config -> String
getMinMaxCapacity bookAnyProps quote estimates =
  case (length estimates), quote.vehicleVariant == "BOOK_ANY" of 
    0, true -> "-"
    _, true -> if bookAnyProps.minCapacity == bookAnyProps.maxCapacity then (show bookAnyProps.minCapacity)
               else (show bookAnyProps.minCapacity) <> " - " <> (show bookAnyProps.maxCapacity)
    _ , false -> quote.capacity
    _,_ -> "-"

getQuoteListViewHeight :: Config -> Int -> Int
getQuoteListViewHeight config len =
  let quoteHeight = HU.getDefaultPixelSize $ config.selectedEstimateHeight
      height = if quoteHeight == 0 then 84 else quoteHeight
      rideHeaderLayout = HU.getDefaultPixelSize (runFn1 getLayoutBounds $ EHC.getNewIDWithTag "rideEstimateHeaderLayout").height
      rideHeaderHeight = if rideHeaderLayout == 0 then 81 else rideHeaderLayout
  in (if len >= 4 then (if EHC.os == "IOS" then 3 else 5) * height else 3 * height) + rideHeaderHeight + 24

getScrollViewHeight :: Config -> Int -> Int
getScrollViewHeight config len = 
   let quoteHeight = HU.getDefaultPixelSize $ config.selectedEstimateHeight
       height = if quoteHeight == 0 then 84 else quoteHeight
       rideHeaderLayout = HU.getDefaultPixelSize (runFn1 getLayoutBounds $ EHC.getNewIDWithTag "rideEstimateHeaderLayout").height
       rideHeaderHeight = if rideHeaderLayout == 0 then 81 else rideHeaderLayout
  in (if len >= 4 then (if EHC.os == "IOS" then ((getHeightFromPercent 73) - rideHeaderHeight) else ((length (filterVariantAndEstimate config.quoteList)) + 2)* height) else (len+3) * height) --((getHeightFromPercent (if EHC.os == "IOS" then 73 else 85)) - rideHeaderHeight)

primaryButtonRequestRideConfig :: Config -> String -> PrimaryButton.Config
primaryButtonRequestRideConfig config id' = PrimaryButton.config
  { textConfig
    { text = title
    , color = Color.yellow900
    , accessibilityHint = "Confirm And Book Button"
    }
  , id = id'
  , background = Color.black900
  , margin = Margin 0 16 0 15
  , enableRipple = not disableButton
  , alpha = if disableButton then 0.5 else 1.0
  , isClickable = not disableButton
  , rippleColor = Color.rippleShade
  }
  where 
    selectedItem = case config.quoteList !! config.activeIndex of
              Just selectedItem -> selectedItem
              Nothing -> ChooseVehicle.config
    disableButton = (selectedItem.selectedServices == []) && selectedItem.vehicleVariant == "BOOK_ANY"
    name = fromMaybe "" selectedItem.serviceTierName
    additionalString = if config.fareProductType == RENTAL then "Rental" else ""
    title = if selectedItem.vehicleVariant == "BOOK_ANY" then getString $ BOOK_ANY else getString $ BOOK ( additionalString <> " " <> name )

filterVariantAndEstimate :: Array ChooseVehicle.Config -> Array ChooseVehicle.Config -- showing unique quotes based on variant and arrange price range (In case of multiple provider)
filterVariantAndEstimate configArray = fromMaybe [] $ do
  let grouped = map fromFoldable $ groupBy ((==) `on` _.vehicleVariant) (sortBy (comparing _.vehicleVariant) configArray)
  traverse mergeGroup grouped 

  where 
    mergeGroup :: Array ChooseVehicle.Config -> Maybe ChooseVehicle.Config
    mergeGroup group = do
      let currencySymbol = getCurrency appConfig 
      first <- head group
      minPriceItem <- minimumBy (compare `on` _.minPrice) group
      maxPriceItem <- maximumBy (compare `on` _.maxPrice) group
      minBPItem <- minimumBy (compare `on` _.basePrice) group
      maxBPItem <- maximumBy (compare `on` _.basePrice) group
      case minPriceItem.minPrice, maxPriceItem.maxPrice of 
        Just minP, Just maxP -> pure $ first { showInfo = false, price = if minP == maxP then currencySymbol <> show minP else currencySymbol <> show minP <> " - " <> currencySymbol <> show maxP }
        _ , _ -> pure $ first { showInfo = false, price = if minBPItem.basePrice == maxBPItem.basePrice then  currencySymbol <> show minBPItem.basePrice else  currencySymbol <> show minBPItem.basePrice <> " - " <> currencySymbol <>  show maxBPItem.basePrice }