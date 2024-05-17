module Components.BottomDrawerList.View where

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App as CT
import Components.BottomDrawerList.Controller (Action(..), Config, ListComponent)
import Data.Array as DA
import Data.String as DS
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (Unit, const, ($), (/=), (<>), (==), (>), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, gravity, height, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onClick, orientation, padding, scrollView, text, textView, visibility, weight, width, clickable, alignParentBottom)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import ConfigProvider

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state = 
  let config = getAppConfig appConfig
      filteredList = DA.filter (\item -> item.visibility) state.itemList
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  , onClick push $ const Dismiss
  , gravity BOTTOM
  , visibility $ boolToVisibility $ state.animState /= ST.HIDE
  ][ PrestoAnim.animationSet
    (if EHC.os == "IOS" then [Anim.fadeIn (state.animState == ST.SHOW), Anim.fadeOut (state.animState == ST.ANIMATING)]
      else
        [ Anim.translateYAnim AnimConfig.animConfig {fromY = 300, toY = 0, ifAnim = state.animState == ST.SHOW}
      , Anim.translateYAnim AnimConfig.animConfig {fromY = 0, toY = 300, ifAnim = state.animState == ST.ANIMATING}
      ]) $ linearLayout 
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , background Color.white900
          , clickable true
          , cornerRadii $ Corners 24.0 true true false false
          , padding $ Padding 16 16 16 $ EHC.safeMarginBottomWithDefault 24
          , onAnimationEnd push $ const OnAnimationEnd
          ][  textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , color Color.black700
              , text state.titleText
              , margin $ MarginBottom 16
              ] <> FontStyle.subHeading2 CT.TypoGraphy
            , linearLayout -- scrollView Use when it is required
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              ][ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ] $ DA.mapWithIndex (listComponent push) filteredList
              ]
          ]
  ]

listComponent :: forall w. (Action -> Effect Unit) -> Int -> ListComponent -> PrestoDOM (Effect Unit) w
listComponent push index item = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      , margin $ MarginVertical 16 16
      , visibility $ boolToVisibility $ index /= 0
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , onClick push $ const $ OnItemClick item
      ][  imageView
          [ width $ V 26
          , height $ V 26
          , imageWithFallback $ fetchImage FF_ASSET item.prefixImg
          ]
        , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ MarginHorizontal 10 10
          , gravity CENTER
          ][  textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , color Color.black800
              , text item.title
              ] <> FontStyle.subHeading2 CT.TypoGraphy
            , textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , color Color.black600
              , text item.desc
              , visibility $ boolToVisibility $ not $ DS.null item.desc
              ] <> FontStyle.tags CT.TypoGraphy
          ]
        , imageView
          [ width $ V 26
          , height $ V 26
          , imageWithFallback $ fetchImage FF_ASSET item.postFixImg
          ]
      ]
  ]