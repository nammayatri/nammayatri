{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.NotificationDetailModel.View where

import Animation (screenAnimationFadeInOut)
import Common.Types.App (LazyCheck(..), YoutubeData)
import Types.App (defaultGlobalState)
import Components.NotificationDetailModel.Controller (Action(..), fetchTitleAndUrl)
import Components.PopUpModal as PopUpModal
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex, (!!)) as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length, trim)
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, screenWidth, getVideoID, getYoutubeData)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), parseNumber)
import JBridge (renderBase64Image, openUrlInApp, setScaleType, setYoutubePlayer, addMediaPlayer)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, show, unit, void, discard, ($), (<<<), (<>), (==), (&&), (-), (*), (/))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageUrl, imageView, linearLayout, margin, onAnimationEnd, onClick, orientation, padding, progressBar, relativeLayout, stroke, text, textSize, textView, visibility, weight, width, scrollBarY, scrollView, lineHeight, textFromHtml, imageWithFallback, rippleColor)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (NotificationDetailModelState, YoutubeVideoStatus(..))
import Services.API (MediaType(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Styles.Types (FontStyle)
import Data.Function.Uncurried (runFn5)
import Mobility.Prelude (boolToVisibility)

view :: forall w. (Action -> Effect Unit) -> NotificationDetailModelState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , clickable true
        , onAnimationEnd
            ( \action -> do
                if state.notificationNotSeen then push IncreaseViewCount else pure unit
                _ <- push action
                void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
                  $ do
                      case state.notificationNotSeen of
                        true -> do
                          res <- Remote.messageSeenBT state.messageId
                          pure unit
                        false -> pure unit
            )
            (const AfterRender)
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , gravity CENTER_HORIZONTAL
              , orientation VERTICAL
              ]
              [ headerLayout state push
              , scrollView
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , scrollBarY false
                  ]
                  [ linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , gravity CENTER
                          , background Color.blue600
                          , cornerRadius 4.0
                          , margin $ Margin 16 16 16 16
                          ]
                          [ screenAnimationFadeInOut
                              $ linearLayout
                                  [ width MATCH_PARENT
                                  , height WRAP_CONTENT
                                  , gravity CENTER
                                  , id (getNewIDWithTag "illustrationView")
                                  , onAnimationEnd
                                      ( \action -> do
                                          let
                                            mediaType = fromMaybe Image state.mediaType

                                            id = (getNewIDWithTag "illustrationView")

                                            url = state.mediaUrl
                                          case mediaType of
                                            VideoLink -> pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig "VIDEO" (getVideoID url)) id (show PLAY) push YoutubeVideoStatus
                                            PortraitVideoLink -> pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig "PORTRAIT_VIDEO" (getVideoID url)) id (show PLAY) push YoutubeVideoStatus
                                            Image -> renderBase64Image state.mediaUrl (getNewIDWithTag "illustrationView") true "FIT_CENTER"
                                            Audio -> addMediaPlayer (getNewIDWithTag "illustrationView") state.mediaUrl false
                                            AudioLink -> addMediaPlayer (getNewIDWithTag "illustrationView") state.mediaUrl false
                                            _ -> pure unit
                                      )
                                      (const NoAction)
                                  ]
                                  [ linearLayout
                                      [ width MATCH_PARENT
                                      , height $ V 150
                                      , gravity CENTER
                                      , visibility if state.mediaType == Just Image then VISIBLE else GONE
                                      ]
                                      [ progressBar
                                          [ width WRAP_CONTENT
                                          , height WRAP_CONTENT
                                          ]
                                      ]
                                    , linearLayout
                                      [ width MATCH_PARENT
                                      , height WRAP_CONTENT
                                      , gravity CENTER
                                      , visibility if state.mediaType == Just ImageLink then VISIBLE else GONE
                                      ]
                                      [ progressBar
                                          [ width WRAP_CONTENT
                                          , height WRAP_CONTENT
                                          , padding $ PaddingVertical 5 5
                                          ]
                                        , imageView
                                          [ width MATCH_PARENT
                                          , visibility GONE
                                          , gravity CENTER
                                          , id $ getNewIDWithTag "imageWithUrl"
                                          , afterRender
                                              ( \action -> do
                                              _ <- pure $ setScaleType (getNewIDWithTag "imageWithUrl") state.mediaUrl "FIT_XY"
                                              pure unit)
                                              (const AfterRender)
                                          ]
                                      ]
                                  ]
                        ]
                      , titleAndLikeCount state push
                      , descriptionAndComment state push
                    ]
                ]
              ]
          ]
            <> if state.addCommentModelVisibility == VISIBLE then [ addCommentModel state push ] else []
        )
    where 
      getYoutubeDataConfig videoType videoId = getYoutubeData {
        videoType = videoType,
        videoId = videoId
      }
addCommentModel :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
addCommentModel state push =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility state.addCommentModelVisibility
    ]
    [ PopUpModal.view (push <<< AddCommentModelAction) (addCommentModelConfig state) ]

descriptionText :: (Action -> Effect Unit) -> NotificationDetailModelState -> forall w. PrestoDOM (Effect Unit) w
descriptionText push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ] (Array.mapWithIndex
            (\index item ->
             let
              desLength = length item
              in
               if desLength == 0
                then
                linearLayout[][]
                else
                  if charAt 0 item == Just '*' && charAt (desLength - 1) item == Just '*'
                    then
                      let
                      titleAndUrl = fetchTitleAndUrl desLength item
                      linkTitle = trim $ fromMaybe "" (titleAndUrl Array.!! 0)
                      linkUrl = trim $ fromMaybe "" (titleAndUrl Array.!! 1)
                      in
                      textView $
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , color Color.blue900
                        , textFromHtml linkTitle
                        , onClick (\action -> do
                                          _ <- openUrlInApp linkUrl
                                          pure unit
                                    ) (const $ NoAction)
                        ] <> FontStyle.paragraphText LanguageStyle
                  else
                    textView $
                      [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , textFromHtml item
                      ] <> FontStyle.paragraphText LanguageStyle
            ) state.description
      )

descriptionAndComment :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
descriptionAndComment state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 0 16 16
    ]
    [ descriptionText push state
    , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , color Color.blue800
        , margin $ MarginTop 16
        , visibility GONE -- TODO :: Change the visibility in next iteration
        , text state.actionText
        ] <> FontStyle.body15 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , background Color.blue600
      , cornerRadius 15.0
      , padding $ Padding 10 6 10 6
      , margin $ MarginTop 16
      , onClick push $ const ShareMessage
      , gravity CENTER
      , rippleColor Color.rippleShade
      , visibility $ boolToVisibility state.shareable
      ][
        imageView
        [ height $ V 16
        , width $ V 16 
        , imageUrl "ic_share_blue"
        , margin $ MarginRight 2
        ]
      , textView $
        [ height WRAP_CONTENT
        , width  WRAP_CONTENT
        , text $ getString SHARE
        , color Color.blue900
        ] <> FontStyle.body1 LanguageStyle
      ]
    , if isJust state.comment then customTextView (getString YOUR_COMMENT) FontSize.a_14 Color.black900 (MarginTop 16) $ FontStyle.medium LanguageStyle else textView []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginTop 8
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , padding $ Padding 16 16 16 16
        , onClick push $ const AddCommentClick
        , clickable if isJust state.comment then false else true
        ]
        [ customTextView (if state.comment == Nothing then (getString ADD_A_COMMENT) else fromMaybe "" state.comment) FontSize.a_14 (if state.comment == Nothing then Color.black600 else Color.black800) (Margin 0 0 0 0) $ FontStyle.medium LanguageStyle ]
    ]

headerLayout :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 5 8 5 8
        ]
        [ imageView
            [ width $ V 40
            , height $ V 40
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackArrow
            , padding $ Padding 7 7 7 7
            , rippleColor Color.rippleShade
            , cornerRadius 20.0
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString MESSAGE
              , margin $ MarginLeft 10
              , weight 1.0
              , gravity CENTER_VERTICAL
              , color Color.black900
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

titleAndLikeCount :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
titleAndLikeCount state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ Margin 16 0 16 8
    ]
    [ linearLayout
      [ width $ V ((screenWidth unit) * 65/100)
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ customTextView state.title FontSize.a_14 Color.black800 (Margin 0 0 0 0) $ FontStyle.semiBold LanguageStyle
       , customTextView state.timeLabel FontSize.a_10 Color.black500 (Margin 0 0 0 0) $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding (Padding 8 4 8 4)
      , cornerRadius 48.0
      , stroke if state.likeStatus then "1," <> Color.transparent else "1," <> Color.grey900 
      , background if state.likeStatus then Color.red100 else Color.transparent
      , onClick push (const $ LikeMessage)
      ][ imageView
         [ height $ V 16
         , width $ V 16
         , imageWithFallback $ fetchImage FF_ASSET $ if state.likeStatus then "ny_ic_heart_red" else "ny_ic_heart_outline"
         , margin $ MarginRight 4
         ]
       , textView 
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         , textSize FontSize.a_12
         , lineHeight "20"
         , color Color.black700
         , fontStyle $ FontStyle.medium LanguageStyle
         , text $ parseNumber state.likeCount
         ]
      ]
    ]

customTextView :: String -> Int -> String -> Margin -> FontStyle -> forall w. PrestoDOM (Effect Unit) w
customTextView text' size color' margin' style =
  textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , fontStyle style
    , margin margin'
    , lineHeight "20"
    , textSize size
    , color color'
    , text text'
    ]

addCommentModelConfig :: NotificationDetailModelState -> PopUpModal.Config
addCommentModelConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , margin = (MarginHorizontal 16 16)
        , buttonLayoutMargin = (Margin 0 16 16 0)
        , editTextVisibility = VISIBLE
        , dismissPopupConfig { visibility = VISIBLE, height = V 12, width = V 12, margin = (Margin 0 21 22 0), padding = (Padding 8 8 8 8) }
        , eTextConfig { editText { placeholder = (getString ENTER_YOUR_COMMENT) }, topLabel { visibility = GONE, text = (getString ENTER_YOUR_COMMENT), color = Color.black900 }, margin = (Margin 16 16 16 0) }
        , primaryText { text = (getString ADD_A_COMMENT), gravity = LEFT, margin = (Margin 16 21 0 0) }
        , secondaryText { visibility = GONE }
        , option1 { visibility = false }
        , option2
          { text = (getString POST_COMMENT)
          , background = Color.white900
          , color = Color.blue800
          , strokeColor = Color.white900
          , padding = (Padding 16 0 16 0)
          , isClickable = state.commentBtnActive
          }
        , cornerRadius = (Corners 15.0 true true true true)
        }
  in
    popUpConfig'

