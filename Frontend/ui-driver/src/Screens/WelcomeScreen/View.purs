module Screens.WelcomeScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (<<<), map, (<>), (-), (==))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, accessibility, afterRender, background, gravity, height, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, orientation, padding, weight, width, onAnimationEnd, scrollDirection, textView, color, cornerRadius, alignParentBottom,frameLayout, onClick, currentItem, onPageSelected)
import Screens.WelcomeScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.WelcomeScreen.ScreenData
import PrestoDOM.List (ListItem, listDataV2, listItem, onClickHolder, textHolder, viewPager2, imageUrlHolder)
import Screens.Types (WelcomeScreenState)
import JBridge (addCarousel, getHeightFromPercent, getWidthFromPercent, getArray)
import Engineering.Helpers.Commons (getNewIDWithTag, safeMarginTop, safeMarginBottom, screenWidth, safeMarginBottomWithDefault, os)
import Data.Function.Uncurried (runFn2)
import Screens.WelcomeScreen.ComponentConfig
import Helpers.Utils as HU
import ConfigProvider
import MerchantConfig.Types (AppConfig)
import PrestoDOM.Animation as PrestoAnim
import Helpers.Utils as HU
import Styles.Colors as Color
import Font.Style as FontStyle
import PrestoDOM.Types.Core (toPropValue)
import Data.Maybe
import Common.Types.App as Common
import Data.Array as DA
import MerchantConfig.Utils
import Helpers.Utils (FetchImageFrom(..), fetchImage)

screen :: WelcomeScreenState -> ListItem -> Screen Action WelcomeScreenState ScreenOutput
screen initialState carouselItem =
  { initialState
  , view : view carouselItem
  , name: "WelcomeScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "WelcomeScreen ----- state" state
          let _ = spy "WelcomeScreen --------action" action
          eval state action
      )
  }


view :: forall w. ListItem -> (Action -> Effect Unit) -> WelcomeScreenState -> PrestoDOM (Effect Unit) w
view carouselItem push state =
  let config = getAppConfig appConfig
      list = carouselTextData state
  in Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility DISABLE
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background config.welcomeScreen.background
        , padding $ PaddingVertical safeMarginTop $ safeMarginBottomWithDefault 24
        ][
          linearLayout[weight 1.0, width MATCH_PARENT][]  
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              ]
              [ imageView
                  [ height $ V 50
                  , width $ V 147
                  , imageWithFallback $ HU.fetchImage HU.FF_ASSET $ if (getMerchant Common.FunctionCall) == BRIDGE then "ny_ic_bridge_logo" else "ic_namma_yatri_logo"
                  ]
              ]
            , carouselView carouselItem config state push list
            , linearLayout[weight 1.0, width MATCH_PARENT][]
            , scrollIndicator state push list
            , PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state)
        ]


carouselViewItem :: WelcomeScreenState -> forall w . PrestoDOM (Effect Unit) w
carouselViewItem state = 
  linearLayout
    [ 
      height $ V $ getHeightFromPercent 68
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    ][  imageView
        [ height $ V $ getHeightFromPercent 44
        , width $ V $ screenWidth unit
        , imageUrlHolder "image"
        ]
      , textView $
        [ textHolder "title"
        , color Color.black800
        , margin $ MarginTop if os == "IOS" then 24 else 12
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        ] <> FontStyle.h1 Common.TypoGraphy
      , textView $
        [ textHolder "subTitle"
        , color Color.black700
        , margin $ MarginTop if os == "IOS" then 16 else 8
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        ] <> FontStyle.subHeading1 Common.TypoGraphy
    ]

carouselView:: ListItem -> AppConfig -> WelcomeScreenState -> (Action -> Effect Unit) -> Array CarouselData -> forall w . PrestoDOM (Effect Unit) w
carouselView carouselItem config state push list =
  PrestoAnim.animationSet [Anim.fadeIn true] $
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background config.welcomeScreen.background
    ][ viewPager2
      $ [ listItem carouselItem
      , listDataV2 $ carouselTransformer $ list
      , height  $ V $ getHeightFromPercent 68
      , width MATCH_PARENT
      , currentItem state.data.currentActiveIndex
      , orientation HORIZONTAL
      , onPageSelected push OnPageChanged
      ]
    ]

scrollIndicator :: WelcomeScreenState ->  (Action -> Effect Unit) -> Array CarouselData ->  forall w . PrestoDOM (Effect Unit) w
scrollIndicator state push list = 
  let itemsLen = DA.length list
      indicators = getArray $ itemsLen
  in  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ] $ DA.mapWithIndex (\idx _ -> indicatorDot push (idx == state.data.currentActiveIndex) idx) $ indicators


indicatorDot :: forall w. (Action -> Effect Unit) -> Boolean -> Int -> forall w . PrestoDOM (Effect Unit) w
indicatorDot push isActive idx =
  linearLayout
  [ height $ V 10
  , width $ V 10
  , background if isActive then Color.black900 else Color.black500
  , margin $ MarginHorizontal 5 5
  , cornerRadius 5.0
  , onClick push $ const (ChangeSheet idx)
  ][]

carouselTransformer :: Array CarouselData -> Array (Record WelcomeScreenCarousel)
carouselTransformer arrData = 
  let merchant = getMerchant Common.FunctionCall
      isBridge = merchant == BRIDGE
      isIos = os == "IOS"
  in   map (\item -> {
  title : toPropValue item.title,
  subTitle : toPropValue item.description,
  image : toPropValue $ if isBridge then do
    let imageName = item.image <> "_bridge"
        imageUrl = (HU.getAssetLink Common.FunctionCall) <> imageName <> ".png"
    if isIos then "url->" <> imageUrl <> "," <> imageName else  "url->" <> imageUrl else item.image
}) arrData