module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchResultType(..), FareProductType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||), negate, (*), (/), (>), (-), (/=), (<), discard, void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Shadow(..), Accessiblity(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, layoutGravity, singleLine, ellipsize, frameLayout, onAnimationEnd, shimmerFrameLayout, alpha, shadow, pivotY, accessibility, clipChildren, maxLines, accessibilityHint, accessibility, Accessiblity(..), accessibilityFocusable)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Debug
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility)
import ConfigProvider
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
import Mobility.Prelude (boolToInvisibility)
import Data.Maybe (isJust, Maybe (..), fromMaybe, maybe)
import Engineering.Helpers.Utils as EHU
import JBridge as JB
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Data.Array (length, mapWithIndex, findIndex, elem, length)
import Engineering.Helpers.Commons(os)
import Common.Animation.Config (estimateExpandingAnimationConfig)
import Data.Array as DA
import Data.String as DS


view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  let
    isActiveIndex = config.index == config.activeIndex
    stroke' = if isActiveIndex && (not config.showEditButton) && (not config.singleVehicle) then "2," <> Color.blue800 else "1," <> Color.white900
    background' = if isActiveIndex && (not config.showEditButton) && (not config.singleVehicle) then Color.blue600 else Color.white900
    padding' = Padding 8 12 8 12
    isBookAny = config.vehicleVariant == "BOOK_ANY" && config.activeIndex == config.index
    selectedEstimateHeight = if config.selectedEstimateHeight == 0 then 72 else config.selectedEstimateHeight
    currentEstimateHeight = if config.currentEstimateHeight < 80 then 176 else config.currentEstimateHeight
    margin' = MarginTop $ if config.index == 0 then 0 else 8
    selectedVehicle = maybe (getVehicleName config) (\name -> name) config.serviceTierName
    priceRange = DS.split(DS.Pattern ("-")) config.price
    fromPrice = fromMaybe "" (priceRange DA.!! 0)
    toPrice = fromMaybe "" (priceRange DA.!! 1)
    accessibilityText = selectedVehicle <> (if isActiveIndex then " selected : " else " Un Selected : ") <> fromPrice <> (if toPrice /= "" then " to " <> toPrice else "") <> " with capacity of " <> config.capacity
    blackListedSearchResultType = config.searchResultType `DA.elem` [ESTIMATES, QUOTES RENTAL,QUOTES INTER_CITY]
    isFindingQuotes = (JB.getKeyInSharedPrefKeys "LOCAL_STAGE") == "FindingQuotes"
  in
    frameLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , onClick push $ const $ OnSelect config
      , clickable config.isEnabled
      , margin margin'
      ][PrestoAnim.animationSet
        ([ Anim.fadeInWithDuration 100 isActiveIndex
        , Anim.fadeOutWithDuration 100 $ not isActiveIndex
        ] <> (if os == "IOS" then [] else [Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) isBookAny]))
        $ linearLayout
          ([ width MATCH_PARENT
          , height $ if os == "IOS" then (if isBookAny then V currentEstimateHeight else V selectedEstimateHeight) else MATCH_PARENT
          , background background'
          , cornerRadius 6.0
          , stroke stroke'
          , gravity RIGHT
          , afterRender push $ const $ NoAction config
          , accessibility DISABLE
          , visibility $ boolToVisibility $ not isFindingQuotes
          ] <> if os == "IOS" then [] else [pivotY 1.0])[]
        , linearLayout
          [ width MATCH_PARENT
          , height $ if os == "IOS" then (if isBookAny then V currentEstimateHeight else V selectedEstimateHeight) else MATCH_PARENT
          , cornerRadius 12.0
          , stroke $ "1," <> Color.grey900
          , afterRender push $ const $ NoAction config
          , accessibility DISABLE
          , visibility $ boolToVisibility $ isFindingQuotes
          ][]
        , linearLayout
            [ width MATCH_PARENT
          , height WRAP_CONTENT
          , cornerRadius 6.0
          , id $ EHC.getNewIDWithTag $ config.id <> show config.index
          , margin $ config.layoutMargin
          , padding padding'
          , orientation VERTICAL
          , afterRender push $ const $ NoAction config
          ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , afterRender push $ const $ NoAction config
              ][ linearLayout
                 [ height $ V 48
                 , width $ V 60
                 ][ imageView
                   [ imageWithFallback config.vehicleImage
                   , height $ V if config.vehicleVariant == "AUTO_RICKSHAW" then 45 else 48
                   , width $ V 60
                   ]
                 ]
               , linearLayout
                 [ height WRAP_CONTENT
                 , weight 1.0
                 , orientation VERTICAL
                 , gravity CENTER_VERTICAL
                 , padding $ PaddingLeft 8
                 , accessibility ENABLE
                 , accessibilityHint accessibilityText
                 ][ linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , gravity CENTER_VERTICAL
                    , accessibility DISABLE_DESCENDANT
                    ][ vehicleDetailsView push config ]
                  , linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , padding $ PaddingTop 5
                    , gravity CENTER_VERTICAL
                    , accessibility DISABLE_DESCENDANT
                    ][ capacityView push config
                    ]
                  ]
              , priceDetailsView push config
            ]
          , if isBookAny then bookAnyView push config else dummyView config
          ]
      , linearLayout
        [ height $ V selectedEstimateHeight
        , width $ MATCH_PARENT
        , gravity RIGHT
        , visibility $ boolToVisibility $ (config.vehicleVariant /= "BOOK_ANY") && blackListedSearchResultType
        , accessibility DISABLE
        ][linearLayout
          [ height $ V selectedEstimateHeight
          , width $ V ((EHC.screenWidth unit) * 3/10)
          , clickable true
          , accessibility DISABLE
          , onClick push $ const $ case (config.showInfo && isActiveIndex) || isFindingQuotes of
                                    false -> OnSelect config
                                    true  -> if config.showInfo && blackListedSearchResultType then ShowRateCard config else NoAction config                        
          ][]
       ]
    ]

bookAnyView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
bookAnyView push state = 
  let isBookAny = state.vehicleVariant == "BOOK_ANY" && state.activeIndex == state.index 
  in 
  PrestoAnim.animationSet
  ([ Anim.fadeInWithDuration 200 isBookAny ] <>
  (if os == "IOS" then []
  else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.6) isBookAny ]))
  $ linearLayout
  ([ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , cornerRadius 6.0
  , afterRender push $ const $ NoAction state
  , margin $ MarginTop 16
  , padding $ Padding 4 4 4 4
  ] <> if os == "IOS" then [] else [pivotY 0.6])[ variantsView push state ]

variantsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
variantsView push state =
  let
    bookAnyVariants = fromMaybe [] $ EHU.splitArrayByLengths state.services [2,3]
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , clipChildren false
      , orientation VERTICAL
      , afterRender push $ const $ NoAction state
      ]
      ( mapWithIndex
          ( \listIndex listItem ->
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , clipChildren false
                , gravity CENTER_HORIZONTAL
                ]
                ( mapWithIndex
                    ( \itemIndex item ->
                        let
                          index = case findIndex (\x -> x == item) state.services of
                            Just index -> index
                            Nothing -> 0
                          isActiveIndex = elem item state.selectedServices
                          itemCount = if (length state.services) == 4 then 2 else 3
                          isInActive = not $ elem item state.availableServices 
                          widthFactor = if itemCount == 3 && item == "Non-AC Mini" then 20 else if itemCount == 2 && item == "Non-AC Mini" then 38 else if itemCount == 2 then 36 else 28
                          shadowOpacity = if isInActive then 0.0 else 1.0
                        in
                          linearLayout
                            [ height $ V 32
                            , width $ V ((EHC.screenWidth unit / itemCount) - widthFactor)
                            , clipChildren false
                            , margin $ Margin 4 4 4 4
                            , cornerRadius 6.0
                            , shadow $ Shadow 0.0 1.0 4.0 0.0 Color.black12 shadowOpacity
                            , stroke $ "1," <> (if isActiveIndex then Color.blue800 else Color.grey900)
                            , clickable $ true
                            , alpha if isInActive then 0.5 else 1.0
                            , background $ if isInActive then Color.grey900 else if isActiveIndex then Color.blue600 else Color.white900
                            , gravity CENTER
                            , onClick (\action -> if isInActive then do 
                                                     void $ pure $ EHU.showToast "Not available at this moment"
                                                     pure unit
                                                  else push action ) $ const $ ServicesOnClick state item
                            , accessibility if isInActive then DISABLE else ENABLE
                            , accessibilityHint $ "Inside Book Any : " <> item <> if isActiveIndex && (not isInActive) then " Checkbox : selected " else " Checkbox : Un Selected"
                            ][ linearLayout
                              [ height MATCH_PARENT
                              , width MATCH_PARENT
                              , gravity CENTER
                              ][ imageView
                                 [ width $ V 14
                                 , height $ V 14
                                 , margin $ MarginRight 2
                                 , gravity CENTER
                                 , visibility $ boolToVisibility $ isActiveIndex && (not isInActive)
                                 , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_check_blue"
                                 ] 
                              , textView $ 
                                 [ text item
                                 , height MATCH_PARENT
                                 , width WRAP_CONTENT
                                 , gravity CENTER
                                 , singleLine true
                                 , maxLines 1
                                 , padding $ PaddingBottom $ if isActiveIndex && (not isInActive) then 2 else 0
                                 , color $ if isActiveIndex && (not isInActive) then Color.blue800 else Color.black800
                                 , accessibility if isInActive then DISABLE else ENABLE
                                 ] <> FontStyle.tags LanguageStyle
                              ]
                          ]
                    )
                    listItem
                )
          )
          bookAnyVariants
      )

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  let selectedVehicle = maybe (getVehicleName config) (\name -> getCustomNameForServiceTier config.vehicleVariant name) config.serviceTierName
  in
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , accessibility DISABLE
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , singleLine true
          , ellipsize true
          , text selectedVehicle
          , color Color.black800
          ]
        <> FontStyle.body7 TypoGraphy
    , linearLayout[
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 12.0
      , margin $ MarginLeft 4
      , padding $ Padding 8 5 8 5
      , onClick push $ const $ OnEditClick
      , visibility $ boolToVisibility $ config.showEditButton
      , background Color.blue600
      , gravity CENTER_VERTICAL
      ][
        imageView [
          width $ V 10
        , height $ V 10
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_edit"
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text config.editBtnText
        , color Color.blue800
        , margin $ MarginLeft 4
        ] <> FontStyle.tags TypoGraphy
      ]
    ]
  where
    getCustomNameForServiceTier :: String -> String -> String
    getCustomNameForServiceTier vehicleVariant name = 
      case vehicleVariant of
        "DELIVERY_BIKE" -> "2 Wheeler"
        _ -> name

getVehicleName :: Config -> String
getVehicleName config = 
  case config.vehicleVariant of
    "AUTO_RICKSHAW" -> "Auto Rickshaw"
    "TAXI" -> "Non-AC Mini"
    "TAXI_PLUS" -> "AC Mini"
    "SEDAN" -> "Sedan"
    "SUV" -> "XL Cab"
    "HATCHBACK" -> "AC Mini"
    "BIKE" -> "Bike Taxi"
    "BOOK_ANY" -> "Book Any"
    "SUV_PLUS" -> "XL Plus"
    "DELIVERY_BIKE" -> "2 Wheeler"
    "AMBULANCE_TAXI" -> "Non-AC" <> "\x00B7" <> "O̶₂"
    "AMBULANCE_TAXI_OXY" -> "Non-AC" <> "\x00B7" <> "O₂"
    "AMBULANCE_AC" -> "AC" <> "\x00B7" <> "O̶₂"
    "AMBULANCE_AC_OXY" -> "AC" <> "\x00B7" <> "O₂"
    "AMBULANCE_VENTILATOR" -> "Ventilator"
    "EV_AUTO_RICKSHAW" -> "EV Auto Rickshaw"
    "HERITAGE_CAB" -> "Heritage Cab"
    _ -> "Non-AC Mini"

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config =
  let isActiveIndex = config.index == config.activeIndex
      infoIcon ="ny_ic_info_blue_lg"
      enableRateCard = (JB.getKeyInSharedPrefKeys "LOCAL_STAGE") == "FindingQuotes" || (config.showInfo && (isActiveIndex || config.singleVehicle) && config.vehicleVariant /= "BOOK_ANY" && config.searchResultType /= QUOTES OneWaySpecialZoneAPIDetails)
      isBookAny = config.vehicleVariant == "BOOK_ANY"
  in
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , padding $ PaddingLeft 8
    , clickable isActiveIndex
    , afterRender push (const $ NoAction config)
    , onClick push $ case  enableRateCard of
                          false -> const $ NoAction config
                          true  -> const $ ShowRateCard config
    , accessibility DISABLE
    ][linearLayout
      ([ height MATCH_PARENT
      , width $ if (isBookAny && os == "IOS") then V (((EHC.screenWidth unit) * 33) / 100) else WRAP_CONTENT
      , orientation VERTICAL
      ] <> if isBookAny then [gravity RIGHT] else [])
      [ linearLayout
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         , accessibility DISABLE
         ][ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text config.price
            , color Color.black800
            , accessibility DISABLE
            ] <> FontStyle.body7 TypoGraphy
          , linearLayout
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , gravity CENTER_VERTICAL
            ] $ if enableRateCard then 
                   [imageView
                    [ imageWithFallback $ fetchImage FF_COMMON_ASSET infoIcon
                    , width $ V 15
                    , height $ V 15
                    , gravity CENTER_VERTICAL
                    , margin $ MarginLeft 4
                    ] 
                   ]
                else []
         ]
       , linearLayout
         [ height $ V 20
         , width WRAP_CONTENT
         , cornerRadius 10.0
         , background Color.green900
         , margin $ MarginTop 4
         , gravity CENTER_VERTICAL
         , padding $ PaddingHorizontal 6 6
         , visibility $ boolToVisibility isBookAny
         , accessibility DISABLE
         ][ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_faster_lightning"
            , width $ V 12
            , height $ V 12
            , gravity CENTER_VERTICAL
            , margin $ MarginRight 4
            ]
          , textView $ 
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , text "Fastest"
            , gravity CENTER_VERTICAL
            , color Color.white900
            , padding $ PaddingBottom $ if os == "IOS" then 0 else 5
            ] <> FontStyle.body15 TypoGraphy
         ]
      ]
  ]

shimmerView :: forall w. Config -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
    [ width $ V 100
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.transparent
    , cornerRadius 6.0
    ] 
    [ 
      linearLayout
    [ height MATCH_PARENT
    , width $ V 100
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    , gravity CENTER_VERTICAL
    , cornerRadius 6.0
    , background Color.greyDark
    ]
    [ textView
        [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , color Color.black800
          ]
      , imageView
        [ width $ V 15
        , height $ V 15
        , gravity CENTER_VERTICAL
        , margin $ MarginLeft 4
        ]
    ]
    ]

capacityView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
capacityView push config = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ][ vehicleInfoView "ny_ic_user_filled" config.capacity config.vehicleVariant
     , descriptionView config.serviceTierShortDesc config.vehicleVariant config.airConditioned
     ]

vehicleInfoView :: forall w. String -> String -> String -> PrestoDOM (Effect Unit) w
vehicleInfoView imageName description vehicleVariant = do
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , visibility $ boolToVisibility $ not (EHU.isAmbulance vehicleVariant)
    ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET imageName
        , width $ V 14
        , height $ V 14
        ]
      , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text description
            , color Color.black700
            ]
          <> FontStyle.tags TypoGraphy
    ]

descriptionView :: forall w. Maybe String -> String -> Maybe Boolean -> PrestoDOM (Effect Unit) w
descriptionView description vehicleVariant airConditioned = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , visibility $ boolToVisibility $ isJust description
    ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_circle_grey"
        , width $ V 3
        , height $ V 3
        , margin $ Margin 2 2 0 0
        ]
     , imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_ac"
        , width $ V 14
        , height $ V 14
        , visibility $ boolToVisibility $ airConditioned == Just true
        , margin $ MarginLeft 2
        ]   
     ,  textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ fromMaybe "" description
          , color Color.black700
          , margin $ Margin 2 0 0 0 
          ]
        <> FontStyle.tags TypoGraphy
    ]

dummyView :: forall w. Config -> PrestoDOM ( Effect Unit) w
dummyView state = 
  linearLayout
  [height $ V 0
  , width $ V 0
  ][]
