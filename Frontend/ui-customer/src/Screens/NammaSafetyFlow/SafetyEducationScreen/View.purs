{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetyEducationScreen.View where

import Animation (screenAnimation, triggerOnAnimationEnd)
import Data.Array as DA
import Data.Maybe as Mb
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import Prelude (Unit, const, negate, not, pure, void, ($), (&&), (+), (-), (<), (<<<), (<>), (==), (>), map)
import PrestoDOM (Gradient(..), Gravity(..), ImageUrl(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, alignParentBottom, alpha, background, color, cornerRadius, gradient, gravity, height, id, imageUrl, imageUrlWithFallback, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, orientation, padding, relativeLayout, rippleColor, scrollView, stroke, text, textFromHtml, textView, visibility, weight, width, accessibilityHint)
import Screens.Types (NammaSafetyScreenState)
import Common.Types.App (LazyCheck(..))
import Data.Function.Uncurried (runFn5)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Foreign (unsafeToForeign)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import RemoteConfig as RC
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Screens.NammaSafetyFlow.SafetyEducationScreen.Controller (Action(..), ScreenOutput, eval)
import Styles.Colors as Color
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SafetyEducationScreen"
  , globalEvents: []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background background'
        , onBackPressed push $ const BackPressed
        , padding padding'
        ]
        [ Header.view (push <<< SafetyHeaderAction) headerConfig
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ if Mb.isJust state.props.educationViewIndex then
                if state.props.showVideoView then
                  videoView push state
                else
                  descriptionView push state
              else
                aboutNammaSafetyView state push
            ]
        ]
  where
  background' = if Mb.isJust state.props.educationViewIndex && state.props.showVideoView then Color.black900 else Color.white900

  padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig =
    (Header.config Language)
      { title =
        if Mb.isJust state.props.educationViewIndex && not state.props.showVideoView then
          ""
        else
          getString LEARN_ABOUT_NAMMA_SAFETY
      , headerVisiblity = boolToVisibility $ not state.props.showVideoView
      }

aboutNammaSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
aboutNammaSafetyView state push =
  Keyed.relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility $ Mb.isNothing state.props.educationViewIndex
    ]
    [ Tuple "aboutNammaSafetyView"
        $ linearLayout
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
                (DA.mapWithIndex (\index item -> cardView item index push) state.data.videoList)
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
            [ imageUrl cardData.coverImageUrl
            , height $ V 90
            , width $ V 100
            ]
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_play_black_white"
            , height $ V 28
            , width $ V 28
            , margin $ Margin 36 31 36 31
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
  Keyed.relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.black900
    , visibility $ boolToInvisibility $ Mb.isJust state.props.educationViewIndex
    ]
    [ Tuple "aboutNammaSafetyVideoView"
        $ linearLayout
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
                    , height $ V 40
                    , width $ V 40
                    , padding $ Padding 8 8 8 8
                    , margin $ Margin 8 8 8 8
                    , onClick push $ const BackPressed
                    , rippleColor Color.rippleShade
                    , cornerRadius 20.0
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
                        ( \_ -> do
                            void $ pure $ runFn5 JB.setYoutubePlayer (EHC.getYoutubeData { videoId = viewConfig.videoId, videoType = "PORTRAIT_VIDEO", videoHeight = 1500, showFullScreen = true, showSeekBar = false, hideFullScreenButton = true }) (EHC.getNewIDWithTag "SafetyYoutubeVideoView") "PAUSED" push YoutubeVideoStatus
                        )
                        (const NoAction)
                    ]
                    []
            ]
    , Tuple "aboutNammaSafetyVideoViewBottom"
        $ relativeLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ height $ V 200
                , width MATCH_PARENT
                , alignParentBottom "true,-1"
                , gradient $ Linear gradientAngle [ Color.transparent, "#269F9F9F", Color.black ]
                ]
                []
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , alignParentBottom "true,-1"
                , gravity CENTER_VERTICAL
                , padding $ Padding 16 16 16 16
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , weight 1.0
                    ]
                    [ textView
                        $ [ text viewConfig.title
                          , color Color.white900
                          , gravity LEFT
                          , width WRAP_CONTENT
                          , margin $ MarginRight 12
                          ]
                        <> FontStyle.h1 TypoGraphy
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    ]
                    [ arrowButtonView false 20 (index > 0) push false $ ChangeEducationViewIndex (index - 1)
                    , arrowButtonView true 0 (index < DA.length state.data.videoList - 1) push false $ ChangeEducationViewIndex (index + 1)
                    ]
                ]
            ]
    ]
  where
  index = Mb.fromMaybe (-1) state.props.educationViewIndex

  gradientAngle = if EHC.os == "IOS" then 270.0 else 180.0

  viewConfig = Mb.fromMaybe { videoId: "", title: "", coverImageUrl: "", description: [] } (state.data.videoList DA.!! index)

arrowButtonView :: forall w. Boolean -> Int -> Boolean -> (Action -> Effect Unit) -> Boolean -> Action -> PrestoDOM (Effect Unit) w
arrowButtonView isDirectionRight marginRight isActive push isDarkTheme action =
  linearLayout
    ( [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , padding $ Padding 12 12 12 12
      , background background'
      , cornerRadius 21.0
      , margin $ MarginRight marginRight
      , accessibilityHint $ if isDirectionRight then "Go to next video button " else "Go to previous video button"
      ]
        <> if isActive then
            [ rippleColor Color.rippleShade, onClick push $ const action ]
          else
            [ alpha 0.5 ]
    )
    [ imageView
        [ imageWithFallback
            $ fetchImage FF_ASSET case isDirectionRight, isDarkTheme of
                true, false -> "ny_ic_arrow_right_black"
                true, true -> "ny_ic_arrow_yellow_filled_right"
                false, false -> "ny_ic_arrow_left_black"
                false, true -> "ny_ic_arrow_yellow_filled_left"
        , height $ V 18
        , width $ V 18
        ]
    ]
  where
  background' = if isDarkTheme then Color.black900 else Color.white900

getSafePadding :: Padding
getSafePadding =
  Padding 0 EHC.safeMarginTop 0
    (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)

descriptionView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
descriptionView push state =
  Keyed.relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    [ Tuple "aboutNammaSafetyDescriptionView"
        $ scrollView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , orientation VERTICAL
                ]
                [ relativeLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , gravity CENTER_VERTICAL
                    , onClick push $ const $ ShowVideoView true
                    , accessibilityHint $ "Play Video Button"
                    ]
                    [ imageView
                        [ if EHC.os == "IOS" then
                            imageUrl videoCoverImage
                          else
                            imageUrlWithFallback $ ListImageUrl videoCoverImage viewConfig.videoId
                        , height $ V 250
                        , width MATCH_PARENT
                        ]
                    , linearLayout
                        [ width MATCH_PARENT
                        , height $ V 250
                        , gravity CENTER
                        ]
                        [ imageView
                            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_play_btn"
                            , height $ V 40
                            , width $ V 40
                            ]
                        ]
                    ]
                , linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    , padding $ Padding 16 16 16 16
                    ]
                    ( map
                        ( \(RC.DescriptionComponent item) ->
                            let
                              decodedFontStyle = Mb.fromMaybe FontStyle.Tags $ FontStyle.decodeFontStyle $ unsafeToForeign item.fontStyle
                            in
                              linearLayout
                                [ height WRAP_CONTENT
                                , width MATCH_PARENT
                                , gravity CENTER_VERTICAL
                                ]
                                [ textView
                                    $ [ text "•"
                                      , visibility $ boolToVisibility item.isBullet
                                      , margin $ Margin 0 item.marginTop 8 0
                                      , width WRAP_CONTENT
                                      , height WRAP_CONTENT
                                      ]
                                    <> FontStyle.getFontStyle decodedFontStyle TypoGraphy
                                , textView
                                    $ [ textFromHtml item.text
                                      , height WRAP_CONTENT
                                      , color item.color
                                      , gravity LEFT
                                      , margin $ Margin item.marginLeft item.marginTop 0 0
                                      , padding $ PaddingVertical 3 3
                                      , weight 1.0
                                      ]
                                    <> FontStyle.getFontStyle decodedFontStyle TypoGraphy
                                ]
                        )
                        viewConfig.description
                    )
                ]
            ]
    , Tuple "aboutNammaSafetyDescriptionViewBottom"
        $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , gravity CENTER_VERTICAL
            , padding $ Padding 16 16 16 16
            ]
            [ HV.layoutWithWeight
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                ]
                [ arrowButtonView false 20 (index > 0) push true $ ChangeEducationViewIndex (index - 1)
                , arrowButtonView true 0 (index < DA.length state.data.videoList - 1) push true $ ChangeEducationViewIndex (index + 1)
                ]
            ]
    ]
  where
  index = Mb.fromMaybe (-1) state.props.educationViewIndex

  viewConfig = Mb.fromMaybe { videoId: "", title: "", coverImageUrl: "", description: [] } (state.data.videoList DA.!! index)

  videoCoverImage = EHC.getImageUrl "" viewConfig.videoId
