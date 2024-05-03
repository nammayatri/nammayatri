module Components.ChooseYourRide.View where

import Common.Types.App
import Debug
import Animation (translateYAnim,translateYAnimFromTop, fadeIn)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as Animation
import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide.Controller (Action(..), Config)
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex, length, (!!), any)
import Data.Function.Uncurried (runFn1)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import JBridge (getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<>), const, pure, unit, not, show, (<<<), (==), (>=), (*), (+), (<=), (&&), (/), (>), (||), (-), (/=))
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), Shadow(..), Gradient(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd, disableClickFeedback, accessibility, peakHeight, halfExpandedRatio, relativeLayout, topShift, bottomShift, alignParentBottom, imageWithFallback, shadow, clipChildren, layoutGravity, accessibilityHint, horizontalScrollView, scrollBarX, disableKeyboardAvoidance, singleLine, maxLines, textFromHtml, gradient, frameLayout)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import ConfigProvider
import PrestoDOM.Properties (sheetState)
import Data.Int (toNumber,ceil)
import MerchantConfig.Types(AppConfig(..))
import Mobility.Prelude
import Screens.Types (ZoneType(..), TipViewStage(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  let isSingleEstimate = (length config.quoteList) == 1 && (fromMaybe ChooseVehicle.config (config.quoteList !! 0)).vehicleVariant == "AUTO_RICKSHAW" && config.enableSingleEstimate
  in
  linearLayout
    [ width MATCH_PARENT
    , height $ V $ EHC.screenHeight unit
    , clipChildren false
    , gravity BOTTOM
    , orientation VERTICAL
    ]
    [ 
      -- linearLayout
      -- [ height WRAP_CONTENT
      -- , width MATCH_PARENT
      -- , orientation VERTICAL
      -- , alignParentBottom "true,-1"
      -- ]
      -- [ 
        -- coordinatorLayout
        -- [ height WRAP_CONTENT
        -- , width MATCH_PARENT
        -- ]
        -- [
          --  bottomSheetLayout
          -- [ height WRAP_CONTENT
          -- , width MATCH_PARENT
          -- , background Color.transparent
          -- , accessibility DISABLE
          -- , peakHeight $ getPeekHeight config isSingleEstimate
          -- , topShift 0.0
          -- , sheetState COLLAPSED
          -- , bottomShift 1.0
          -- , orientation VERTICAL
          -- ]
          -- [ 
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ chooseYourRideView push config isSingleEstimate
            -- , bottomDummyView push config
            ]
          -- ]
        -- ]
      -- ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , alignParentBottom "true,-1"
      , clickable true
      -- , margin $ MarginTop 8
      , padding $ Padding 16 (if config.showPreferences then 16 else 0) 16 16
      , shadow $ Shadow 0.1 0.1 7.0 24.0 Color.greyBackDarkColor 0.5 
      ]
      [ addTipView push config
      , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config) ]
    ]
  where
    getPeekHeight :: Config -> Boolean -> Int
    getPeekHeight config isSingleEstimate = 
      let
        headerLayout = runFn1 getLayoutBounds $ EHC.getNewIDWithTag "rideEstimateHeaderLayout"
        bottomButtonLayout = runFn1 getLayoutBounds $ EHC.getNewIDWithTag "bottomButtonLayout"
        len = length config.quoteList
        quoteHeight = HU.getDefaultPixelSize $ config.selectedEstimateHeight
        estimateItemHeight = if quoteHeight == 0 then (if isSingleEstimate then 48 else 84) else quoteHeight
        quoteViewVisibleHeight = if len > 2 then (3 * estimateItemHeight) else (len * estimateItemHeight) + (estimateItemHeight / 2)
        
        pixels = runFn1 HU.getPixels FunctionCall
        density = (runFn1 HU.getDeviceDefaultDensity FunctionCall) / defaultDensity

        currentPeekHeight = headerLayout.height + quoteViewVisibleHeight + bottomButtonLayout.height
        requiredPeekHeight = if EHC.os == "IOS" 
                             then ceil ((toNumber (currentPeekHeight+ 270)) / pixels) 
                             else ceil ((toNumber currentPeekHeight / pixels) * density) 
      in 
        if requiredPeekHeight == 0 then 470 else requiredPeekHeight

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
  , onClick push $ const $ if state.tipViewProps.stage == DEFAULT then AddTip else NoAction 
  , visibility $ boolToVisibility state.enableTips
  ] $ (case state.tipViewProps.stage of 
          DEFAULT -> [defaultTipView push state]
          TIP_AMOUNT_SELECTED -> [selectTipView push state]
          RETRY_SEARCH_WITH_TIP -> [tipSelectedView push state]
          _ -> [defaultTipView push state])
  
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
            , padding $ if index == 0 then PaddingHorizontal 10 10 else Padding 0 0 0 0
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
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonRequestRideConfig config)
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

chooseYourRideView :: forall w. (Action -> Effect Unit) -> Config -> Boolean -> PrestoDOM (Effect Unit) w
chooseYourRideView push config isSingleEstimate =
  let estimateConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      anims = if EHC.os == "IOS"
              then [fadeIn true]
              else [translateYAnimFromTop $ Animation.chooseRideAnimConfig]
      tagConfig = HU.specialZoneTagConfig config.zoneType
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
            , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET tagConfig.icon
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
            , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_white_info"
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
            ][ textView (
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
                  [ text $ getString TOLL_CHARGES_WILL_BE_EXTRA
                  , color Color.black650
                  , gravity CENTER_HORIZONTAL
                  , height WRAP_CONTENT
                  , gravity CENTER_HORIZONTAL
                  , width MATCH_PARENT
                  , visibility $ boolToVisibility config.showTollExtraCharges
                  ] <> FontStyle.paragraphText TypoGraphy
                , linearLayout
                  [ height $ V 1
                  , width MATCH_PARENT
                  , margin $ MarginTop 12
                  , background Color.grey900
                  ][]
                ]
          , quoteListView push config isSingleEstimate
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

quoteListView :: forall w. (Action -> Effect Unit) -> Config -> Boolean -> PrestoDOM (Effect Unit) w
quoteListView push config isSingleEstimate =
  frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push (const NoAction)
    ]
    [scrollView
      [ height $ getQuoteListViewHeight config isSingleEstimate
      , width MATCH_PARENT
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ PaddingVertical 16 10
          , margin $ MarginHorizontal 16 16
          , orientation VERTICAL
          ]( mapWithIndex
              ( \index item -> 
                  ChooseVehicle.view (push <<< ChooseVehicleAC) (item{isSingleEstimate = isSingleEstimate})
              ) config.quoteList
          )]
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

getQuoteListViewHeight :: Config -> Boolean -> Length
getQuoteListViewHeight config isSingleEstimate =
    let len = length config.quoteList
        quoteHeight = HU.getDefaultPixelSize $ config.selectedEstimateHeight
        height = if quoteHeight == 0 then (if isSingleEstimate then 48 else 84) else quoteHeight
    in V $ (if len >= 4 then 3 * height else len * height) + if len == 1 then 16 else 5

primaryButtonRequestRideConfig :: Config -> PrimaryButton.Config
primaryButtonRequestRideConfig config = PrimaryButton.config
  { textConfig
    { text = getString $ BOOK name
    , color = Color.yellow900
    , accessibilityHint = "Confirm And Book Button"
    }
  , id = "ConfirmAndBookButton"
  , background = Color.black900
  , margin = Margin 0 16 0 15
  , enableRipple = true
  , rippleColor = Color.rippleShade
  }
  where 
    name = case config.quoteList !! config.activeIndex of
              Just selectedItem -> fromMaybe "" selectedItem.serviceTierName
              Nothing -> ""

