module Components.ChooseYourRide.View where

import Common.Types.App
import Debug
import Animation (translateYAnim,translateYAnimFromTop, fadeIn)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as Animation
import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide.Controller (Action(..), Config, BookAnyProps, bookAnyProps, getBookAnyProps, getMinMaxPrice, getMinMaxCapacity)
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
import Prelude (Unit, ($), (<>), const, pure, unit, bind, not, show, bind, negate, (<<<), (==), (>=), (*), (+), (<=), (&&), (/), (>), (||), (-), map, (/=), (<), (<>), otherwise)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), Shadow(..), Gradient(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd, disableClickFeedback, accessibility, peakHeight, halfExpandedRatio, relativeLayout, topShift, bottomShift, alignParentBottom, imageWithFallback, shadow, clipChildren, layoutGravity, accessibilityHint, horizontalScrollView, scrollBarX, disableKeyboardAvoidance, singleLine, maxLines, textFromHtml, gradient, frameLayout, enableShift, nestedScrollView, shimmerFrameLayout, alpha)
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
import Data.String as DS
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (compare)
import Data.Function (on)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import RemoteConfig as RC
import Storage as ST
import Components.ChooseVehicle.Controller as CCC

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
      in (if currentPeekHeight == 0 then 470 else currentPeekHeight) + (if config.enableTips then 36 else 0) + (if EHC.os /= "IOS" && config.fareProductType == DELIVERY then 100 + (if config.tipViewProps.stage == TIP_AMOUNT_SELECTED then 40 else 0) else 0)

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
  , afterRender push $ const $ NoAction config
  , padding $ Padding 16 (if config.showPreferences then 16 else 0) 16 16
  , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5
  ][
    deliveryPaymentAtReceivingEndLayout push config
   , addTipView push config
   , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config "PrimaryButtomConfirm")
   ]
   
addTipView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
addTipView push state =
  Keyed.linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.ivory
  , cornerRadius 12.0
  , padding $ Padding 20 12 20 12
  , margin $ MarginTop 16
  , gravity CENTER
  , clickable $ not isOneWaySpecialZoneRide
  , alpha $ if (not isOneWaySpecialZoneRide) then 1.0 else 0.5
  , afterRender push $ const $ NoAction state
  , onClick push $ const $ if state.tipViewProps.stage == DEFAULT && (not isOneWaySpecialZoneRide) then AddTip state.tipViewProps else NoAction state
  , visibility $ boolToVisibility state.enableTips
  ] $ (case state.tipViewProps.stage of
          DEFAULT -> [defaultTipView push state]
          TIP_AMOUNT_SELECTED -> [selectTipView push state]
          RETRY_SEARCH_WITH_TIP -> [tipSelectedView push state]
          _ -> [defaultTipView push state])
  where
    isOneWaySpecialZoneRide = selectedEstimate.searchResultType == ChooseVehicle.QUOTES ChooseVehicle.OneWaySpecialZoneAPIDetails

    selectedEstimate = case state.quoteList !! state.activeIndex of
                        Just selectedItem -> selectedItem
                        Nothing -> ChooseVehicle.config
    defaultTipView push state =
      Tuple "default" $ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , onClick push $ const $ AddTip state.tipViewProps
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
    tipSelectedView push state =
      Tuple "tipSelectedView" $ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , clickable true
      , onClick push $ const $ ChangeTip state.tipViewProps
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
    selectTipView push state =
      Tuple "selectTipView" $ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , orientation VERTICAL
      ][ textView $
        [ text state.tipViewProps.primaryText
        , color Color.black900
        , gravity CENTER
        , margin $ MarginBottom 6
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
          ][
            relativeLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
              [ width $ if index == 0 then WRAP_CONTENT else V 84
              , height $ V 36
              , background Color.white900
              , margin $ if index == 0 then Margin 0 12 0 0 else Margin 8 12 0 0
              , cornerRadius 7.0
              , gravity CENTER
              , padding $ if index == 0 then PaddingHorizontal 10 10 else Padding 0 0 0 0
              , stroke $ "1," <> (if (state.tipViewProps.activeIndex == index) then Color.green900 else Color.grey900)
              , onClick push $ const $ TipBtnClick index (fromMaybe 0 (state.customerTipArrayWithValues !! index)) state.customerTipArrayWithValues
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
              ],
              linearLayout
              [  height WRAP_CONTENT
              , width $ if index == 0 then WRAP_CONTENT else WRAP_CONTENT
              , margin $ if index == 0 then Margin 0 0 0 0 else Margin 18 0 0 0
              , gravity CENTER
              , gradient (Linear 90.0 [Color.green900, Color.darkGreen])
              , cornerRadius 40.0
              , visibility $ boolToVisibility $ state.tipViewProps.suggestedActiveIndex == Just index
              ][ textView $ [
                text "Suggested"
              , color Color.white900
              , padding $ Padding 6 2 6 2
              ] <> FontStyle.tags TypoGraphy
              ]
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
  let
    estimateConfig = (getAppConfig appConfig).estimateAndQuoteConfig
    anims = if EHC.os == "IOS" then [fadeIn true] else [translateYAnimFromTop $ Animation.chooseRideAnimConfig]
    tagConfig = HS.specialZoneTagConfig config.zoneType
    showTag = any (_ == config.zoneType) [SPECIAL_PICKUP, METRO]
    nearByDrivers = fromMaybe 0 config.nearByDrivers

  in
  PrestoAnim.animationSet anims $
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , onAnimationEnd push (const $ NoAction config)
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity RIGHT
      , margin $ Margin 0 0 15 8
      , visibility $ boolToVisibility $ isJust config.nearByDrivers && estimateConfig.showNearByDrivers
      , disableClickFeedback true
      ][ textView
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , gravity RIGHT
         , stroke $ "1," <> Color.grey900
        , text $ show nearByDrivers <> " " <> if nearByDrivers > 1 then (getString DRIVERS_AVAILABLE) else getString DRIVER_AVAILABLE
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
            , accessibility DISABLE
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
                [ text headerText
                , color Color.black800
                , gravity CENTER_HORIZONTAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                , accessibility ENABLE
                ] <> FontStyle.h1 TypoGraphy)
                , estimatedTimeAndDistanceView push config
                , extraChargesView
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
  where
    selectedVehicle = fromMaybe ChooseVehicle.config $ config.quoteList !! config.activeIndex
    headerText = if config.fareProductType == DELIVERY then getString CONFIRM_YOUR_DELIVERY else if length config.quoteList > 1 then getString CHOOSE_YOUR_RIDE else getString CONFIRM_YOUR_RIDE
    extraChargesView =
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_HORIZONTAL
      , margin $ MarginTop 4
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , padding $ Padding 8 8 8 8
        , background Color.grey700
        , orientation HORIZONTAL
        , visibility $ boolToVisibility $ selectedVehicle.hasTollCharges || selectedVehicle.hasParkingCharges
        , cornerRadius 16.0
        , gravity CENTER_VERTICAL
        ][
          imageView[
            height $ V 16
          , width $ V 16
          , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET if selectedVehicle.hasTollCharges then "ny_ic_black_toll" else "ny_ic_parking_logo_grey"
          , margin $ MarginRight 4
          ]
        , textView $ [
            textFromHtml case selectedVehicle.hasTollCharges, selectedVehicle.hasParkingCharges of
                  true, true -> getString APP_TOLL_PARKING_CHARGES
                  true, false -> getString APP_TOLL_CHARGES
                  false, true -> getString APP_PARKING_CHARGES
                  _, _ -> ""
            , color Color.black800
            , gravity CENTER_HORIZONTAL
            , height WRAP_CONTENT
            , gravity CENTER_HORIZONTAL
            , width MATCH_PARENT
            ] <> FontStyle.body1 TypoGraphy
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
        , accessibilityHint $  "Estimated distance is : " <> config.rideDistance
        ]
        <> FontStyle.paragraphText TypoGraphy
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.5
        , background Color.black700
        , margin (Margin 6 2 6 0)
        ]
        []
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.rideDuration
        , color Color.black650
        , accessibilityHint $ "Estimated time is : " <> config.rideDuration
          ]
          <> FontStyle.paragraphText TypoGraphy
      ]

quoteListView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
quoteListView push config =
  let variantBasedList = filterVariantAndEstimate config.quoteList
      topProviderList = filter (\element -> element.providerType == ONUS) config.quoteList
      viewHeight = getScrollViewHeight config $ length if config.showMultiProvider then variantBasedList else topProviderList
      selectedEstimatedConfig = fromMaybe ChooseVehicle.config $ config.quoteList !! config.activeIndex
  in
  frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push (const NoAction config)
    , margin $ MarginBottom if EHC.os == "IOS" then 44 else 0
    ]
    [scrollView
      [ nestedScrollView $ if selectedEstimatedConfig.vehicleVariant == "BOOK_ANY" then length config.quoteList > 4 else length config.quoteList > 5
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
                            let userCity = DS.toLower $ ST.getValueToLocalStore ST.CUSTOMER_LOCATION
                                estimates = if item.vehicleVariant == "BOOK_ANY" then filter (\estimate -> elem (fromMaybe "" estimate.serviceTierName) item.selectedServices) variantBasedList else []
                                services = if item.vehicleVariant == "BOOK_ANY" then RC.getBookAnyServices userCity else []
                                bookAnyConfig = getBookAnyProps estimates
                                price = getMinMaxPrice bookAnyConfig item estimates
                                capacity = getMinMaxCapacity bookAnyConfig item estimates
                            ChooseVehicle.view (push <<< ChooseVehicleAC config.tipViewProps) (item{selectedEstimateHeight = config.selectedEstimateHeight, price = price, showInfo = true, capacity = capacity, singleVehicle = (length variantBasedList == 1), currentEstimateHeight = config.currentEstimateHeight, services = services})
                        ) variantBasedList
                  else
                    Tuple "TopProvider" $ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    ] $ mapWithIndex
                        ( \index item -> do
                            let userCity = DS.toLower $ ST.getValueToLocalStore ST.CUSTOMER_LOCATION
                                estimates = if item.vehicleVariant == "BOOK_ANY" then filter (\estimate -> elem (fromMaybe "" estimate.serviceTierName) item.selectedServices) topProviderList else []
                                services = if item.vehicleVariant == "BOOK_ANY" then RC.getBookAnyServices userCity else []
                                bookAnyConfig = getBookAnyProps estimates
                                price = getMinMaxPrice bookAnyConfig item estimates
                                capacity = getMinMaxCapacity bookAnyConfig item estimates
                            ChooseVehicle.view (push <<< ChooseVehicleAC config.tipViewProps) (item{selectedEstimateHeight = config.selectedEstimateHeight, price = price, showInfo = true, capacity = capacity, singleVehicle = (length topProviderList == 1), currentEstimateHeight = config.currentEstimateHeight, activeIndex = config.activeIndex, services = services})
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
      , afterRender push $ const $ NoAction config
      , padding $ Padding 16 (if config.showPreferences then 16 else 0) 16 16
      , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5
      ][
      deliveryPaymentAtReceivingEndLayout push config
      , addTipView push config
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

getQuoteListViewHeight :: Config -> Int -> Int
getQuoteListViewHeight config len =
  let quoteHeight = HU.getDefaultPixelSize $ config.selectedEstimateHeight
      height = if quoteHeight == 0 then 72 else quoteHeight
      rideHeaderLayout = HU.getDefaultPixelSize (runFn1 getLayoutBounds $ EHC.getNewIDWithTag "rideEstimateHeaderLayout").height
      rideHeaderHeight = if rideHeaderLayout == 0 then 81 else rideHeaderLayout
      _ = spy "getQuoteListViewHeight" (((if len >= 4 then (if EHC.os == "IOS" then 3 else 5) * height else 3 * height) + rideHeaderHeight + 24) + (if len > 0 then len-1 else len) * 8)
  in ((if len >= 4 then (if EHC.os == "IOS" then 3 else 5) * height else 3 * height) + rideHeaderHeight + 24) + (if len > 0 then len-1 else len) * 8

getScrollViewHeight :: Config -> Int -> Int
getScrollViewHeight config len =
  let quoteHeight = HU.getDefaultPixelSize $ config.selectedEstimateHeight
      height = if quoteHeight == 0 then 84 else quoteHeight
      rideHeaderLayout = HU.getDefaultPixelSize (runFn1 getLayoutBounds $ EHC.getNewIDWithTag "rideEstimateHeaderLayout").height
      rideHeaderHeight = if rideHeaderLayout == 0 then 81 else rideHeaderLayout
      screenHeightPercent = if EHC.os == "IOS" then 73 else 85
      maxScrollViewHeight = getHeightFromPercent screenHeightPercent
      variantListLength = length $ filterVariantAndEstimate config.quoteList
      remainingAvailableScrollSpace = maxScrollViewHeight - rideHeaderHeight
      calculatedHeight
        | len >= 4 && EHC.os == "IOS" = remainingAvailableScrollSpace
        | len >= 4 = min (HU.getDefaultPixelSize remainingAvailableScrollSpace) ((variantListLength + 2)* height)
        | EHC.os == "IOS" = ((len) * height) + (HU.getDefaultPixelSize (runFn1 getLayoutBounds $ EHC.getNewIDWithTag "deliveryPaymentAtReceivingEndLayout").height)
        | otherwise = ((len+3) * height) + (HU.getDefaultPixelSize (runFn1 getLayoutBounds $ EHC.getNewIDWithTag "deliveryPaymentAtReceivingEndLayout").height)
  in calculatedHeight

primaryButtonRequestRideConfig :: Config -> String -> PrimaryButton.Config
primaryButtonRequestRideConfig config id' = PrimaryButton.config
  { textConfig
    { text = title
    , color = Color.yellow900
    , accessibilityHint = "Confirm Button to Proceed With " <> name
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
    currencySymbol = getCurrency appConfig
    selectedItem = case config.quoteList !! config.activeIndex of
              Just selectedItem -> selectedItem
              Nothing -> ChooseVehicle.config
    name = fromMaybe "" selectedItem.serviceTierName
    disableButton = ((selectedItem.selectedServices == []) && selectedItem.vehicleVariant == "BOOK_ANY") || (config.fareProductType == RENTAL && name == "")
    additionalString = case config.fareProductType of
                            RENTAL -> "Rental"
                            INTER_CITY -> "Intercity"
                            _ -> ""
    tip = fromMaybe 0 (config.tipViewProps.customerTipArrayWithValues !! config.tipViewProps.activeIndex)
    bookAnyProps = getBookAnyProps (filter (\estimate -> elem (fromMaybe "" estimate.serviceTierName) selectedItem.selectedServices) config.quoteList)
    maximumPrice = if selectedItem.vehicleVariant == "BOOK_ANY" then Just bookAnyProps.maxPrice else selectedItem.maxPrice
    minimumPrice = if selectedItem.vehicleVariant == "BOOK_ANY" then bookAnyProps.minPrice else selectedItem.basePrice
    priceRange = if isJust maximumPrice && maximumPrice /= Just minimumPrice  then currencySymbol <> show (minimumPrice + tip)  <> " - " <> currencySymbol <> show ((fromMaybe minimumPrice maximumPrice) + tip) else currencySymbol <> show (minimumPrice + tip)
    title = case config.fareProductType of
              RENTAL -> if selectedItem.vehicleVariant == "BOOK_ANY" then getString $ BOOK_ANY else getString $ BOOK ( "Rental" <> " " <> name )
              INTER_CITY -> if selectedItem.vehicleVariant == "BOOK_ANY" then getString $ BOOK_ANY else getString $ BOOK ( "Intercity" <> " " <> name )
              AMBULANCE -> getString $ BOOK ( fromMaybe "" selectedItem.serviceTierName )
              _ -> getString $ BOOK_FOR_ priceRange 
    

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

deliveryPaymentAtReceivingEndLayout :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
deliveryPaymentAtReceivingEndLayout push config =
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , padding $ Padding 8 12 8 12
      , background Color.blue600
      , cornerRadius 12.0
      , margin $ MarginTop 16
      , gravity CENTER_VERTICAL
      , visibility $ boolToVisibility $ config.fareProductType == DELIVERY
      , id $ EHC.getNewIDWithTag "deliveryPaymentAtReceivingEndLayout"
      ]
      [ imageView[
            height $ V 20
          , width $ V 20
          , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_wallet_outline"
          , margin $ MarginRight 12
          ]
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $
          [ text $ getString PAYMENT_AT_RECEIVING_END
          , color Color.black800
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginBottom 2
          ] <> FontStyle.tags LanguageStyle
        , textView $
          [ text $ getString PAYMENT_AT_RECEIVING_END_DESC
          , color Color.black700
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          ] <> FontStyle.body3 LanguageStyle
      ]
      ]
