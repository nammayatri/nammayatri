module Screens.WelcomeScreen.View where

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton as PrimaryButton
import Components.SelectMenuButton as MenuButtonController
import Components.SelectMenuButton.View as MenuButton
import Data.Array as DA
import Data.Function.Uncurried (runFn2)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge (addCarousel)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (CityConfig)
import Prelude (Unit, const, map, ($), (<<<), (<>), bind, pure, unit, (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, color, cornerRadius, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.Types (CarouselModel, WelcomeScreenStage(..), WelcomeScreenState)
import Screens.Types as ST
import Screens.WelcomeScreen.ComponentConfig
import Screens.WelcomeScreen.Controller (Action(..), ScreenOutput, eval)
import Styles.Colors as Color


screen :: WelcomeScreenState -> Screen Action WelcomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "WelcomeScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "WelcomeScreen ----- state" state
          let _ = spy "WelcomeScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> WelcomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background if DA.any (_ == state.props.currentStage) [ENABLE_LOCATION, CAROUSEL, DETECT_LOCATION] then "#FFFAED" else Color.white900
        , padding $ PaddingBottom 24
        ][ headerView state push
          , relativeLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            ][ 
                linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , visibility if state.props.currentStage == DETECT_LOCATION then VISIBLE else GONE
                ][ currentLocationView state push
                 , currentLanguageView state push
                ]
              , if DA.any (_ == state.props.currentStage) [SELECT_LANG, SELECT_CITY]  then radioButtonView state push else dummyView

        -- imageView
        --     [ height $ V 50
        --     , width $ V 147
        --     , margin $ MarginTop 50
        --     , imageWithFallback "ny_namma_yatri,https://assets.juspay.in/nammayatri/images/user/ny_namma_yatri"   -- "ic_namma_yatri_logo"
        --     ]
            -- , carouselView state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            ][ PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state) ]
          ]
        ]

carouselView :: WelcomeScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
carouselView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "CarouselView"
    , gravity CENTER
    , weight 1.0
    , margin $ MarginBottom 20
    , afterRender (\action -> do
        _ <- push action
        -- _ <- runFn2 addCarousel (getCarouselViewData state) (getNewIDWithTag "CarouselView")
        pure unit
        ) (const AfterRender)
    ][]

headerView :: WelcomeScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if DA.any (_ == state.props.currentStage) [SELECT_LANG, SELECT_CITY] then VISIBLE else GONE
  ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
   , linearLayout 
     [ height $ V 1
     , width MATCH_PARENT
     , background Color.grey900
     ][]
  ]

getCarouselViewData :: WelcomeScreenState -> Array CarouselModel
getCarouselViewData state = [
      {image : "ny_ic_welcome_screen_1", title : getString DIRECT_PAYMENT_NO_COMMISSIONS, description : getString CUSTOMER_PAYS_DIRECTLY},
      {image : "ny_ic_welcome_screen_2", title : getString HUNDRED_PERCENT_FARE_GOES_TO_YOU, description : getString FARE_SHOWN_IS_FARE_YOU_GET},
      {image : "ny_ic_welcome_screen_3", title : getString BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION, description : getString OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT}
    ]

currentLocationView :: forall w. WelcomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
currentLocationView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ Padding 24 24 24 24
  , margin $ Margin 16 16 16 16
  , cornerRadius 12.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  ][  imageView
      [ height $ V 220
      , width $ V 220
      , imageWithFallback $ "ny_ic_bangalore_map," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_bangalore_map.png"
      ]
    , textView $
      [ text $ getString YOUR_DETECTED_LOCATION_IS
      , gravity CENTER
      , color Color.black700
      , margin $ MarginTop 24
      ] <> FontStyle.paragraphText TypoGraphy
    , textView $
      [ text "Bangalore"
      , gravity CENTER
      , color Color.black800
      , margin $ MarginTop 4
      ] <> FontStyle.priceFont TypoGraphy
    , textView $
      [ text "Change City"
      , gravity CENTER
      , color Color.blue800
      , margin $ MarginTop 16
      , onClick push $ const $ ChangeStage SELECT_CITY
      ] <> FontStyle.tags TypoGraphy
  ]

currentLanguageView :: forall w. WelcomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
currentLanguageView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ Padding 12 12 12 12
  , margin $ MarginHorizontal 16 16
  , cornerRadius 12.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  ][ linearLayout
      [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
      ][ textView $ [
          text $ getString LANGUAGE_DETECTED <> ": "
          , gravity CENTER
          , color Color.black800
          , margin $ MarginTop 2
        ] <> FontStyle.body3 TypoGraphy
      , textView $ [
          text "English"
          , gravity CENTER
          , color Color.black900
        ] <> FontStyle.subHeading1 TypoGraphy
      ]
    , textView $ [
      text $ getString CHANGE_LANGUAGE_STR <> "(" <> ")"
      , gravity CENTER
      , color Color.blue800
      , onClick push $ const $ ChangeStage SELECT_LANG
      , margin $ MarginTop 16
      ] <> FontStyle.body3 TypoGraphy
  ]


radioButtonView :: WelcomeScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
radioButtonView state push =
  let items = if state.props.currentStage == SELECT_LANG then state.data.config.languageList else transformCityConfig state.data.config.cityConfig
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  ][  textView $
      [ text $ getString if state.props.currentStage == SELECT_LANG then SELECT_LANGUAGE_DESC else SELECT_LOCATION_DESC
      , color Color.black800
      , margin $ Margin 16 24 16 16
      ] <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 5
      , background Color.white900
      ](DA.mapWithIndex
          (\ index language ->  
          PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha $ AnimConfig.translateYAnimMapConfig index
          ] $ MenuButton.view
              (push <<< (MenuButtonAction))
              { text: {name: language.name, value: language.value, subtitle: language.subtitle}, 
                isSelected: (state.props.selectedLanguage == language.value), 
                index : index, lineVisiblity : false, 
                selectedStrokeColor : Color.blue900, 
                selectedBackgroundColor : Color.blue600, 
                notSelectedStrokeColor : Color.grey700 }) items
      )
  ]
 

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [visibility GONE][]

transformCityConfig :: Array CityConfig -> Array MenuButtonController.Text
transformCityConfig cityConfig = map (\city -> {name: city.cityName, value: city.cityName, subtitle: ""}) cityConfig