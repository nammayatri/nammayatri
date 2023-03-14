module Screens.SelectLanguageScreen.View where

import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons as EHC 
import Prelude (Unit, const, map, ($), (<<<),(==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, background, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, weight, width)
import Screens.SelectLanguageScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.CustomerUtils.SelectLanguageScreen.ComponentConfig

screen :: ST.SelectLanguageScreenState -> Screen Action ST.SelectLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "SelectLanguageScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.SelectLanguageScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed) 
    , background Color.white900
    , padding (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 then 24 else EHC.safeMarginBottom))
    , afterRender push (const AfterRender)
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state) 
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding (Padding 16 0 16 0)
        ][  listLanguageView state push
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT  
            , gravity BOTTOM
            , weight 1.0
            ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
          ]
      ]

listLanguageView :: forall w . ST.SelectLanguageScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
listLanguageView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ](map (\lang_data -> MenuButton.view (push <<< MenuButtonActionController) (menuButtonConfig state lang_data)) state.data.languages)
