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
import Common.Types.App (LazyCheck(..))
import Components.NotificationDetailModel.Controller (Action(..))
import Components.PopUpModal as PopUpModal
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (addMediaPlayer, getVideoID, setYoutubePlayer)
import JBridge (renderBase64Image)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, show, unit, ($), (<<<), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageUrl, imageView, linearLayout, margin, onAnimationEnd, onClick, orientation, padding, progressBar, relativeLayout, stroke, text, textSize, textView, visibility, weight, width, scrollBarY, scrollView, lineHeight, textFromHtml)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (NotificationDetailModelState, YoutubeData, YoutubeVideoStatus(..))
import Services.APITypes (MediaType(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Styles.Types (FontStyle)

view :: forall w. (Action -> Effect Unit) -> NotificationDetailModelState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , clickable true
        , afterRender
            ( \action -> do
                _ <- push action
                launchAff_ $ flowRunner $ runExceptT $ runBackT
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
                                            VideoLink -> pure $ setYoutubePlayer (youtubeData state) id (show PLAY)
                                            Image -> renderBase64Image state.mediaUrl (getNewIDWithTag "illustrationView")
                                            Audio -> addMediaPlayer (getNewIDWithTag "illustrationView") state.mediaUrl
                                            AudioLink -> addMediaPlayer (getNewIDWithTag "illustrationView") state.mediaUrl
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
                                  ]
                        ]
                      , titleAndTimeLabel state push
                      , descriptionAndComment state push
                    ]
                ]
              ]
          ]
            <> if state.addCommentModelVisibility == VISIBLE then [ addCommentModel state push ] else []
        )

addCommentModel :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
addCommentModel state push =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility state.addCommentModelVisibility
    ]
    [ PopUpModal.view (push <<< AddCommentModelAction) (addCommentModelConfig state) ]

descriptionAndComment :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
descriptionAndComment state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 0 16 16
    ]
    [ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , fontStyle $ FontStyle.regular LanguageStyle
        , lineHeight "20"
        , textSize FontSize.a_14
        , color Color.black700
        , textFromHtml state.description   
        ]
    , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , fontStyle $ FontStyle.bold LanguageStyle
        , textSize FontSize.a_12
        , color Color.blue800
        , margin $ MarginTop 16
        , visibility GONE -- TODO :: Change the visibility in next iteration
        , text state.actionText
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
        , padding $ Padding 5 16 5 16
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageUrl "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackArrow
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString ALERT
              , textSize FontSize.a_18
              , margin $ MarginLeft 20
              , weight 1.0
              , gravity CENTER_VERTICAL
              , color Color.black900
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

titleAndTimeLabel :: NotificationDetailModelState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
titleAndTimeLabel state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ Margin 16 0 16 8
    ]
    [ customTextView state.title FontSize.a_14 Color.black800 (Margin 0 0 0 0) $ FontStyle.semiBold LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    , customTextView state.timeLabel FontSize.a_12 Color.black800 (Margin 0 0 0 0) $ FontStyle.medium LanguageStyle 
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
        , eTextConfig { editText { placeholder = (getString ENTER_YOUR_COMMENT), fontStyle = FontStyle.medium LanguageStyle, textSize = FontSize.a_14 }, topLabel { visibility = GONE, fontStyle = FontStyle.medium LanguageStyle, text = (getString ENTER_YOUR_COMMENT), color = Color.black900 }, margin = (Margin 16 16 16 0) }
        , primaryText { text = (getString ADD_A_COMMENT), gravity = LEFT, margin = (Margin 16 21 0 0) }
        , secondaryText { visibility = GONE }
        , option1 { visibility = false }
        , option2
          { text = (getString POST_COMMENT)
          , background = Color.white900
          , color = Color.blue800
          , strokeColor = Color.white900
          , padding = (Padding 16 0 16 0)
          , fontSize = FontSize.a_16
          , fontStyle = FontStyle.medium LanguageStyle
          , isClickable = state.commentBtnActive
          }
        , cornerRadius = (Corners 15.0 true true true true)
        }
  in
    popUpConfig'

youtubeData :: NotificationDetailModelState -> YoutubeData
youtubeData state =
  { videoTitle: "title"
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: getVideoID state.mediaUrl
  }
