module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||), negate, (*), (/), (>), (-), (/=), discard, void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, layoutGravity, singleLine, ellipsize, frameLayout, onAnimationEnd, shimmerFrameLayout, alpha)
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
import Data.Maybe (isJust, Maybe (..), fromMaybe)
import Engineering.Helpers.Utils as EHU
import JBridge as JB
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Data.Array (length, mapWithIndex, findIndex, elem)
import Engineering.Helpers.Commons(os)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  let
    isActiveIndex = config.index == config.activeIndex
    stroke' = if isActiveIndex && (not config.showEditButton) && (not config.singleVehicle) then "2," <> Color.blue800 else "1," <> Color.white900
    background' = if isActiveIndex && (not config.showEditButton) && (not config.singleVehicle) then Color.blue600 else Color.white900
    padding' = Padding 16 16 16 16
    isBookAny = config.vehicleVariant == "BOOK_ANY" && config.activeIndex == config.index
    selectedEstimateHeight = if config.selectedEstimateHeight == 0 then 80 else config.selectedEstimateHeight
    currentEstimateHeight = if config.currentEstimateHeight == 0 then 184 else config.currentEstimateHeight
  in
    frameLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , onClick push $ const $ OnSelect config
      , clickable config.isEnabled
      ][  
       PrestoAnim.animationSet
            [ Anim.fadeInWithDuration 100 isActiveIndex
            , Anim.fadeOutWithDuration 100 $ not isActiveIndex
            ]
            $ 
            linearLayout
                [ width MATCH_PARENT
                , height $ if os == "IOS" then (if isBookAny then V currentEstimateHeight else V selectedEstimateHeight) else MATCH_PARENT
                , background background'
                , cornerRadius 6.0
                , stroke stroke'
                , gravity RIGHT
                , afterRender push $ const $ NoAction config
                ][]
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , cornerRadius 6.0
          , id $ EHC.getNewIDWithTag config.id
          , margin $ config.layoutMargin
          , padding padding'
          , orientation VERTICAL
          , afterRender push $ const $ NoAction config
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , afterRender push $ const $ NoAction config
              ]
              [ linearLayout
                  [ height $ V 48
                  , width $ V 60
                  ]
                  [ imageView
                    [ imageWithFallback config.vehicleImage
                    , height $ V if config.vehicleVariant == "AUTO_RICKSHAW" then 45 else 48
                    , width $ V 60
                    ]
                  ]
              , linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , weight 1.0
                  ]
                  [ linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      ]
                      [ linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , orientation VERTICAL
                          , gravity CENTER_VERTICAL
                          , padding $ PaddingLeft 8
                          ]
                          [ linearLayout
                              [ width MATCH_PARENT
                              , height WRAP_CONTENT
                              , gravity CENTER_VERTICAL
                              ]
                              [ vehicleDetailsView push config
                              , linearLayout [ weight 1.0 ] []
                              , linearLayout
                                  [ width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , orientation VERTICAL
                                  , afterRender push (const $ NoAction config)
                                  ][ priceDetailsView push config ]
                              ]
                          , linearLayout
                              [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , padding $ PaddingTop 5
                              , gravity CENTER_VERTICAL
                              ]
                              [ capacityView push config
                              , descriptionView config.serviceTierShortDesc config.vehicleVariant config.airConditioned
                              ]
                          ]
                      ]
                  ]
              ]
            , if isBookAny then bookAnyView push config else dummyView config
          ]
      , linearLayout
        [ height $ V selectedEstimateHeight
        , width $ MATCH_PARENT
        , gravity RIGHT
        , visibility $ boolToVisibility $ config.vehicleVariant /= "BOOK_ANY"
        ][linearLayout
          [ height $ V selectedEstimateHeight
          , width $ V ((EHC.screenWidth unit) * 3/10)
          , clickable true
          , onClick push $ const $ case config.showInfo && isActiveIndex of
                                    false -> OnSelect config
                                    true  -> if config.showInfo then ShowRateCard config else NoAction config
          ][]
       ]
    ]

bookAnyView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
bookAnyView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , cornerRadius 6.0
  , afterRender push $ const $ NoAction state
  , margin $ MarginTop 16
  , padding $ Padding 8 8 8 8
  ][ variantsView push state ]

variantsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
variantsView push state =
  let
    tipValuesArr = EHU.splitIntoEqualParts 3 state.services
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      ( mapWithIndex
          ( \listIndex listItem ->
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , gravity CENTER_HORIZONTAL
                ]
                ( mapWithIndex
                    ( \itemIndex item ->
                        let
                          index = case findIndex (\x -> x == item) state.services of
                            Just index -> index
                            Nothing -> 0
                          isActiveIndex = elem item state.selectedServices
                          isInActive = not $ elem item state.availableServices
                        in
                          linearLayout
                            [ height $ V 32
                            , width $ V ((EHC.screenWidth unit / 3) - 31)
                            , margin $ itemMargin itemIndex listIndex listItem
                            , cornerRadius 4.0
                            , stroke $ (if isInActive then "0," else "1,") <> (if isActiveIndex then Color.blue800 else Color.blue600)
                            , clickable $ true
                            , alpha if isInActive then 0.5 else 1.0
                            , background $ if isInActive then Color.grey900 else if isActiveIndex then Color.blue600 else Color.white900
                            , gravity CENTER
                            , onClick (\action -> if isInActive then do 
                                                     void $ pure $ JB.toast "Not available at this moment"
                                                     pure unit
                                                  else push action ) $ const $ ServicesOnClick state item
                            ]
                            [ textView
                                $ [ text $ item
                                  , height MATCH_PARENT
                                  , width MATCH_PARENT
                                  , color $ if isActiveIndex && (not isInActive) then Color.blue800 else Color.black800
                                  , gravity CENTER
                                  ]
                                <> FontStyle.tags LanguageStyle
                            ]
                    )
                    listItem
                )
          )
          tipValuesArr
      )
  where
  itemMargin :: Int -> Int -> Array String -> Margin
  itemMargin index listIndex listItem = Margin 0 (if listIndex > 0 then 8 else 0) (if (index + 1) == (length listItem) then 0 else 8) 0

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , singleLine true
          , ellipsize true
          , text $ case config.serviceTierName of
                     Just name -> name
                     Nothing -> getVehicleName config
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
    getVehicleName :: Config -> String
    getVehicleName config = 
      case (getMerchant FunctionCall) of
        YATRISATHI -> case config.vehicleVariant of
                        "TAXI" -> "Non AC Taxi"
                        "SUV"  -> "AC SUV"
                        _      -> "AC Cab"
        _          -> case config.vehicleVariant of
                        "AUTO_RICKSHAW" -> "Auto Rickshaw"
                        "TAXI" -> "Non-AC Taxi"
                        "TAXI_PLUS" -> "AC Taxi"
                        "SEDAN" -> "Sedan"
                        "SUV" -> "SUV"
                        "HATCHBACK" -> "Hatchback"
                        _ -> "Non-AC Taxi"

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config =
  let isActiveIndex = config.index == config.activeIndex
      infoIcon ="ny_ic_info_blue_lg"
      enableRateCard = config.showInfo && (isActiveIndex || config.singleVehicle) && config.vehicleVariant /= "BOOK_ANY"
  in
  linearLayout
    [ height MATCH_PARENT
    , width $  WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    , gravity CENTER_VERTICAL
    , clickable isActiveIndex
    , onClick push $ case enableRateCard of
                          false -> const $ NoAction config
                          true  -> const $ ShowRateCard config
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.price
          , color Color.black800
          ]
        <> FontStyle.body7 TypoGraphy
      , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET infoIcon
        , width $ V 15
        , height $ V 15
        , gravity CENTER_VERTICAL
        , margin $ MarginLeft 4
        , visibility $ boolToVisibility enableRateCard
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
    ][ vehicleInfoView "ic_user_filled" config.capacity]

vehicleInfoView :: forall w. String -> String -> PrestoDOM (Effect Unit) w
vehicleInfoView imageName description = do
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
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
