module Screens.FavProviderScreen.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, weight, width,  textView, text, color, textSize, fontStyle, scrollView, fillViewport, cornerRadius, stroke, imageView, imageWithFallback, onClick)
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Prelude (Unit, const, ($), (<<<), map, (<>), (==))
import Screens.FavProviderScreen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.FavProviderScreen.ComponentConfig
import Font.Size as FontSize
import Font.Style as FontStyle
import MerchantConfig.Types as MRC
import Helpers.Utils as HU
import Resources.Constants as CONS
import Components.ProviderModel as PM
import Debug(spy)

screen :: ST.FavProviderScreenState -> Screen Action ST.FavProviderScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : "FavProviderScreen"
  , globalEvents : [] 
  , eval : \action state -> do
        let _ = spy "FavProviderScreen -----------state" state 
        let _ = spy "FavProviderScreen -----------action" action
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.FavProviderScreenState -> PrestoDOM (Effect Unit) w 
view push state = 
  Anim.screenAnimation 
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL 
      , gravity CENTER
      , onBackPressed push $ const BackPressed 
      , background Color.white900
      ][  GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey
          ][]
        , linearLayout 
          [ weight 1.0 
          , width MATCH_PARENT
          , gravity CENTER 
          , orientation VERTICAL
          , padding $ Padding 16 16 16 16
          ][  favProviderList push state ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT  
          , gravity BOTTOM
          , weight 1.0
          ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
      ]

favProviderList :: forall w. (Action -> Effect Unit) -> ST.FavProviderScreenState -> PrestoDOM (Effect Unit) w 
favProviderList push state = 
  scrollView
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , fillViewport true
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ](map (\element -> PM.view (push <<< ProviderModelAC) (providerModelConfig state element) ) state.data.currentCityConfig.iopConfig.providersList)
  ]