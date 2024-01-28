{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetyEducationScreen.View where

import Animation
import Data.Maybe
import Prelude
import PrestoDOM
import Common.Types.App (LazyCheck(..))
import Data.Array
import Data.Function.Uncurried (runFn3)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude
import PrestoDOM.Animation as PrestoAnim
import RemoteConfigs as RC
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.SafetyEducationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SafetyEducationScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let
          _ = spy "SafetyEducationScreen action " action
        let
          _ = spy "SafetyEducationScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background background'
        , onBackPressed push $ const BackPressed
        , padding padding'
        ]
        [ videoView push state
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , visibility $ boolToVisibility $ isNothing state.props.educationViewIndex
            ]
            [ Header.view (push <<< SafetyHeaderAction) headerConfig
            , aboutNammaSafetyView state push
            ]
        ]
  where
  background' = Color.white900

  padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig = Header.config { title = getString LEARN_ABOUT_NAMMA_SAFETY }

toggleSwitchView :: Boolean -> Boolean -> Action -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive visibility' action push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const action
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]

shimmerView :: forall w. NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility $ boolToVisibility state.props.showShimmer
    ]
    [ sfl (V 400) 16 1 true
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        ]
        [ sfl (V 80) 130 3 (getValueToLocalStore IS_SOS_ACTIVE == "true")
        , sfl (V 80) 130 1 (getValueToLocalStore IS_SOS_ACTIVE /= "true")
        ]
    ]

sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop marginTop
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        ( map
            ( \item ->
                linearLayout
                  [ height height'
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , weight 1.0
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 4 4 4 4
                  ]
                  []
            )
            (1 .. numberOfBoxes)
        )
    ]

aboutNammaSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
aboutNammaSafetyView state push =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView
            [ text $ getString LEARN_ABOUT_SAFETY_MODE
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black700
            , background Color.blue600
            , gravity LEFT
            , padding $ Padding 12 16 12 16
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            (mapWithIndex (\index item -> cardView item index push) state.data.videoList)
        ]
    ]

cardView :: RC.SafetyVideoConfig -> Int -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
cardView cardData index push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ PaddingRight 16
    , margin $ Margin 16 16 16 0
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    , onClick push $ const $ ChangeEducationViewIndex index
    ]
    [ relativeLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginRight 14
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET cardData.coverImageUrl
            , height $ V 90
            , width $ V 100
            ]
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_play_black_white"
            , height $ V 90
            , width $ V 100
            , padding $ Padding 36 31 36 31
            ]
        ]
    , textView
        $ [ text cardData.title
          , gravity CENTER_VERTICAL
          , color Color.black800
          ]
        <> FontStyle.body6 TypoGraphy
    ]

videoView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
videoView push state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.black900
    , visibility $ boolToInvisibility $ isJust state.props.educationViewIndex
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.black900
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_close_white"
                , height $ V 24
                , width $ V 24
                , margin $ Margin 16 16 16 16
                , onClick push $ const BackPressed
                ]
            , textView
                $ [ text $ getString LEARN_ABOUT_NAMMA_SAFETY
                  , color Color.white900
                  , gravity LEFT
                  , weight 1.0
                  ]
                <> FontStyle.h3 TypoGraphy
            ]
        , PrestoAnim.animationSet [ triggerOnAnimationEnd true ]
            $ linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , background Color.black900
                , cornerRadius 8.0
                , gravity CENTER_VERTICAL
                , id $ EHC.getNewIDWithTag "SafetyYoutubeVideoView"
                , onAnimationEnd
                    ( \action -> do
                        let
                          _ = spy "onAnimationEnd" "VideoView"
                        void $ pure $ runFn3 JB.setYoutubePlayer (EHC.getYoutubeData viewConfig.videoId "PORTRAIT_VIDEO" 1500 true false) (EHC.getNewIDWithTag "SafetyYoutubeVideoView") "PAUSED"
                    )
                    (const NoAction)
                ]
                []
        ]
    , relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gradient (Linear 180.0 [ "#00D9D9D9", "#269F9F9F", Color.black ])
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.black900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , gravity CENTER_VERTICAL
            , padding $ Padding 16 16 16 16
            ]
            [ textView
                $ [ text viewConfig.title
                  , color Color.white900
                  , gravity LEFT
                  , weight 1.0
                  , margin $ MarginRight 12
                  ]
                <> FontStyle.h1 TypoGraphy
            , arrowButtonView false 20 (index > 0) push $ ChangeEducationViewIndex (index - 1)
            , arrowButtonView true 0 (index < length state.data.videoList - 1) push $ ChangeEducationViewIndex (index + 1)
            ]
        ]
    ]
  where
  index = fromMaybe (-1) state.props.educationViewIndex

  viewConfig = fromMaybe { videoId: "", title: "", coverImageUrl: "" } (state.data.videoList !! index)

arrowButtonView :: forall w. Boolean -> Int -> Boolean -> (Action -> Effect Unit) -> Action -> PrestoDOM (Effect Unit) w
arrowButtonView isDirectionRight marginRight isActive push action =
  linearLayout
    ( [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , padding $ Padding 12 12 12 12
      , background Color.white900
      , cornerRadius 21.0
      , margin $ MarginRight marginRight
      ]
        <> if isActive then
            [ onClick push $ const action ]
          else
            [ alpha 0.5 ]
    )
    [ imageView
        [ imageWithFallback
            $ fetchImage FF_ASSET
                if isDirectionRight then
                  "ny_ic_arrow_right_black"
                else
                  "ny_ic_arrow_left_black"
        , height $ V 18
        , width $ V 18
        ]
    ]

getSafePadding :: Padding
getSafePadding =
  Padding 0 EHC.safeMarginTop 0
    (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
