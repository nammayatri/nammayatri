module Components.StepsHeaderModal.View where

import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, (<>), bind, ($), pure, unit, show, (+), (>=), (&&), (>), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), Visibility(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, lottieAnimationView, id, afterRender, visibility, background, padding,alignParentRight, cornerRadius)
import Components.StepsHeaderModal.Controller (Action(..),Config)
import Styles.Colors as Color
import Data.Array as Array
import Data.Maybe as Maybe
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (getAssetStoreLink)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.black900
    , padding $ PaddingVertical 10 10
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginHorizontal 5 10
        , padding $ PaddingVertical 10 10
        , gravity CENTER_VERTICAL
        ][  imageView
            [ height $ V 25
            , width $ V 25
            , imageWithFallback $ "ny_ic_chevron_left_white," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_left_white.png"
            , onClick push $ const OnArrowClick
            , visibility case state.backArrowVisibility of 
                true -> VISIBLE
                false -> if (state.activeIndex > 0 && not state.driverNumberVisibility )then VISIBLE else GONE
            ]
            ,imageView
            [ 
            width $ V 25
            , height $ V 25
            , margin $ Margin 12 5 5 5
            , visibility case state.profileIconVisibility of
                true -> VISIBLE
                false -> GONE
            , imageWithFallback "ic_profile_shadow,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_profile_shadow.png"
            ]
            , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ (Maybe.fromMaybe "" state.driverMobileNumber)
            , color Color.white900
            , margin $ MarginHorizontal 5 5 
            , visibility case state.driverNumberVisibility of
                true -> VISIBLE
                false -> GONE
            ] <> FontStyle.body1 TypoGraphy
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER_HORIZONTAL
            , margin $ MarginHorizontal 5 5
            --, weight 1.0
            , visibility case state.stepsViewVisibility of
                true -> VISIBLE
                false -> GONE
            ](mapWithIndex (\index item -> 
              linearLayout
              [ height $ V 1
              , width WRAP_CONTENT
              , weight 1.0
              , background if state.activeIndex >= index then Color.white900 else Color.white900
              , margin $ MarginRight 15
              ][]) (if state.profileIconVisibility then (state.driverTextArray)  else  (state.customerTextArray)))  
          , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity RIGHT
            , text $ "Step "<> (show (state.activeIndex + 1)) <> "/"<> (show (state.numberOfSteps))
            , color Color.white900
            , margin (MarginRight 10)
            , visibility case state.stepsViewVisibility of
                true -> VISIBLE
                false -> GONE
            ] <> FontStyle.body3 TypoGraphy
          , linearLayout
           [   height WRAP_CONTENT
             , width MATCH_PARENT
             , orientation VERTICAL
             , alignParentRight "true,-1"
             , gravity RIGHT
           ] 
           [
             linearLayout
           [   height WRAP_CONTENT
             , width WRAP_CONTENT
             , orientation VERTICAL
             , alignParentRight "true,-1"
             , gravity RIGHT
             , cornerRadius 12.0
             , stroke ("1,"<>Color.white900)
             , padding $ Padding 4 2 4 4
             , margin $ MarginBottom 5
             , visibility case state.logoutVisibility of
                          true -> VISIBLE
                          false -> GONE
           ] [
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ "Logout" 
            , color Color.white900
            , margin $ MarginHorizontal 5 5
            , onClick push $ const Logout 
            -- , visibility case state.stepsViewVisibility of
            --     true -> VISIBLE
            --     false -> GONE
            ] <> FontStyle.body3 TypoGraphy
           ]
           ]

        ]
      , textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text $ Maybe.fromMaybe "" ((if state.profileIconVisibility then (state.driverTextArray)  else  (state.customerTextArray)) Array.!! state.activeIndex )
        , color Color.white900
        , margin $ Margin 15 5 0 22
        ] <> FontStyle.h1 TypoGraphy
    ]