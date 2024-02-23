module Components.ChooseVehicle.View where

import Common.Types.App
import ConfigProvider
import Debug

import Common.Styles.Colors as Color
import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Types (STR(..))
import Language.Strings (getString)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
import Mobility.Prelude (boolToInvisibility)
import Data.Maybe (isJust, Maybe (..), fromMaybe)
import Engineering.Helpers.Utils as EHU
import JBridge as JB
import Data.String as DS
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||) ,negate, (*), (/), (<), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, layoutGravity, singleLine, ellipsize, frameLayout)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  cardView push config
  where
    isActiveIndex = config.index == config.activeIndex

cardView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
cardView push config =
  let
    isActiveIndex = config.index == config.activeIndex
    stroke' = if isActiveIndex && (not config.showEditButton) then "2," <> Color.blue800 else "1," <> Color.white900
    background' = if isActiveIndex && (not config.showEditButton) then Color.blue600 else Color.white900
    padding' = Padding 16 16 16 16
    bounds = JB.getLayoutBounds $ EHC.getNewIDWithTag config.id
  in
    frameLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][ linearLayout
          [ width MATCH_PARENT
          , height $ WRAP_CONTENT
          , cornerRadius 6.0
          , id $ EHC.getNewIDWithTag config.id
          , clickable config.isEnabled
          , background background'
          , padding padding'
          , margin $ config.layoutMargin
          , stroke stroke'
          , gravity RIGHT
          , onClick push $ const $ OnSelect config
          , afterRender push (const NoAction)
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , afterRender push (const NoAction)
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
                                  , gravity RIGHT
                                  , afterRender push (const NoAction)
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
          ]
      , linearLayout
        [ height $ V bounds.height
        , width $ MATCH_PARENT
        , gravity RIGHT
        ][linearLayout
          [ height $ V bounds.height
          , width $ V ((EHC.screenWidth unit) * 3/10)
          , clickable true
          , onClick push $ const $ case isActiveIndex of
                                    false -> OnSelect config
                                    true  -> if config.showInfo then ShowRateCard config else NoAction
          ][]
       ]
    ]

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
                        "SEDAN" -> "Comfy" 
                        "SUV" -> "SUV"
                        "HATCHBACK" -> "Eco" 
                        _ -> "Non-AC Taxi"

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config =
  let isActiveIndex = config.index == config.activeIndex
      infoIcon ="ny_ic_info_blue_lg"
  in
  linearLayout
    [ height MATCH_PARENT
    , width $  WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    , gravity CENTER_VERTICAL
    , clickable isActiveIndex
    , onClick push $ case (config.showInfo && isActiveIndex) of
                          false -> const $ NoAction
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
        , visibility $ boolToVisibility $ config.showInfo && isActiveIndex
        ]
    ]

capacityView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
capacityView push config = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ][ vehicleInfoView "ic_user_filled" config.capacity]

vehicleInfoView :: forall w. String -> String -> PrestoDOM (Effect Unit) w
vehicleInfoView imageName description = 
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

