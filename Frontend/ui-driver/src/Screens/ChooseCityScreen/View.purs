module Screens.ChooseCityScreen.View where

import Screens.ChooseCityScreen.ComponentConfig

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..), CarouselData)
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
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (CityConfig)
import Prelude (Unit, const, map, ($), (<<<), (<>), bind, pure, unit, (==))
import PrestoDOM (Accessiblity(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), accessibility, afterRender, alignParentBottom, alpha, background, color, cornerRadius, fontStyle, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.ChooseCityScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (ChooseCityScreenStage(..), ChooseCityScreenState)
import Styles.Colors as Color

screen :: ChooseCityScreenState -> Screen Action ChooseCityScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ChooseCity"
  , globalEvents:  [(\ push -> do
    _ <- pure $ spy "hello " "abc"
    _ <- JB.storeCallBackDriverLocationPermission push LocationPermissionCallBack
    pure $ pure unit)]
  , eval:
      ( \state action -> do
          let _ = spy "ChooseCityScreen ----- state" state
          let _ = spy "ChooseCityScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> ChooseCityScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        -- , background if DA.any (_ == state.props.currentStage) [ENABLE_PERMISSION, CAROUSEL, DETECT_LOCATION] then "#FFFAED" else Color.white900
        , gradient (Linear 0.0 ["#F5F8FF", "#E2EAFF"])
        , padding $ PaddingBottom 24
        ][ headerView state push
          , relativeLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            ][ enableLocationPermission state push
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , visibility if state.props.currentStage == DETECT_LOCATION then VISIBLE else GONE
                ][ currentLocationView state push
                 , currentLanguageView state push
                ]
              -- , linearLayout
              --   [ height MATCH_PARENT
              --   , width MATCH_PARENT
              --   , orientation VERTICAL
              --   -- , background Color.black800
              --   , gravity CENTER
              --   , visibility if state.props.currentStage == CAROUSEL then VISIBLE else GONE
              --   ](if state.props.currentStage == CAROUSEL then [carouselView state push] else  [] )
              -- , if DA.any (_ == state.props.currentStage) [SELECT_LANG, SELECT_CITY]  then radioButtonView state push else dummyView
                , if state.props.currentStage == SELECT_LANG then radioButtonView state push else dummyView
                , if state.props.currentStage == SELECT_CITY then radioButtonView2 state push else dummyView

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

-- carouselView:: ChooseCityScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
-- carouselView state push = 
--   linearLayout 
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     , orientation VERTICAL
--     , id $ getNewIDWithTag "CarouselView"
--     , accessibility DISABLE
--     , gravity CENTER
--     -- , weight 1.0
--     , margin $ MarginBottom 20
--     , afterRender (\action -> do
--         _ <- push action
--         _ <- pure $ spy "testing5" "abc"
--         _ <- runFn2 JB.addCarousel state.data.carouselModal (getNewIDWithTag "CarouselView")
--         pure unit
--         ) (const AfterRender)
--     -- , background Color.blue900
--     ][]

headerView :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
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

-- getCarouselViewData :: WelcomeScreenState -> Array CarouselData
-- getCarouselViewData state = [
--       {image : "ny_ic_welcome_screen_1", title : getString DIRECT_PAYMENT_NO_COMMISSIONS, description : getString CUSTOMER_PAYS_DIRECTLY},
--       {image : "ny_ic_welcome_screen_2", title : getString HUNDRED_PERCENT_FARE_GOES_TO_YOU, description : getString FARE_SHOWN_IS_FARE_YOU_GET},
--       {image : "ny_ic_welcome_screen_3", title : getString BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION, description : getString OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT}
--     ]

currentLocationView :: forall w. ChooseCityScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
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
      , imageWithFallback $ fetchImage FF_ASSET (getLocationMapImage state.data.locationSelected)
      
      ]
    , textView $
      [ text "Can you please enter your location"
      , gravity CENTER
      , color Color.black700
      , margin $ MarginTop 24
      ] <> FontStyle.paragraphText TypoGraphy
    , textView $
      [ text state.data.locationSelected
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

currentLanguageView :: forall w. ChooseCityScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
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
          text $ getLangFromVal state.props.selectedLanguage
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


radioButtonView :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
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

radioButtonView2 :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
radioButtonView2 state push =
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
              (push <<< (MenuButtonAction2))
              { text: {name: language.name, value: language.value, subtitle: language.subtitle}, 
                isSelected: (state.data.locationSelected == language.value), 
                index : index, lineVisiblity : false, 
                selectedStrokeColor : Color.blue900, 
                selectedBackgroundColor : Color.blue600, 
                notSelectedStrokeColor : Color.grey700 }) items
      )
  ]

enableLocationPermission :: ChooseCityScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
enableLocationPermission state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ Padding 20 20 20 20
  , margin $ MarginTop 14
  , visibility if state.props.currentStage == ENABLE_PERMISSION then VISIBLE else GONE
  , margin $ Margin 16 16 16 16
  , cornerRadius 12.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  ][  textView $
      [ text $ getString ENABLE_LOCATION_PERMISSION
      , gravity CENTER
      , color Color.black800
      , margin $ MarginBottom 20
      ] <> FontStyle.body8 TypoGraphy
    , linearLayout 
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , background Color.blue600
      
      , cornerRadius 8.0
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
      ][ imageView
         [ height $ V 220
         , width $ V 220 
        --  , imageWithFallback $ "ny_driver_location_permission," <> (HU.getAssetStoreLink FunctionCall) <> "ny_driver_location_permission.png"
         , imageWithFallback $ fetchImage FF_ASSET "ny_driver_location_permission"
         ]
       ]
    , textView $
         [ text $ getString PLEASE_ENABLE_LOCATION_PERMISSION_FOR <> "Namma yatri " <> "from your device settings to start riding"
         , gravity CENTER
         , color Color.black700
         , margin $ MarginTop 4
         ] <> FontStyle.paragraphText TypoGraphy
  ]
 

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [visibility GONE][]

transformCityConfig :: Array CityConfig -> Array MenuButtonController.Text
transformCityConfig cityConfig = map (\city -> {name: city.cityName, value: city.cityName, subtitle: ""}) cityConfig