{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NotificationsScreen.View where

import Prelude

import Animation (fadeIn, fadeOut, screenAnimationFadeInOut)
import Common.Types.App (LazyCheck(..))
import Types.App (defaultGlobalState)
import Components.ErrorModal as ErrorModal
import Components.NotificationDetailModel as NotificationDetailModel
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array ((..), length)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alignParentBottom, background, color, gravity, height, id, imageUrl, imageView, layoutGravity, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarY, swipeRefreshLayout, text, textSize, textView, visibility, weight, width, imageWithFallback, rippleColor, cornerRadius)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.List as PrestoList
import PrestoDOM.Types.Core (toPropValue)
import Screens.NotificationsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (NotificationsScreenState, AnimationState(..), NotificationCardPropState)
import Services.API (MessageListRes(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Components.BottomNavBar.View as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Screens as ScreenNames
import Helpers.Utils(fetchImage, FetchImageFrom(..))


screen :: NotificationsScreenState -> PrestoList.ListItem -> Screen Action NotificationsScreenState ScreenOutput
screen initialState notificationListItem =
  { initialState: initialState { shimmerLoader = AnimatedIn }
  , view: view notificationListItem
  , name: "NotificationsScreen"
  , globalEvents:
      [ globalOnScroll "NotificationsScreen"
      , ( \push -> do
            void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  (MessageListRes messageListRes) <- Remote.messageListBT "8" $ show initialState.offsetValue
                  lift $ lift $ doAff do liftEffect $ push $ MessageListResAction (MessageListRes messageListRes)
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "HomeScreen state -----" state
          let
            _ = spy "HomeScreen--------action" action
          eval action state
      )
  }

view :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NotificationsScreenState -> PrestoDOM (Effect Unit) w
view notificationListItem push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , onBackPressed push $ const BackPressed
    ] $ [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ][ screenAnimationFadeInOut $
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , weight 1.0
                ][ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , weight 1.0
                    ][ headerLayout state push
                    ,  notificationListView notificationListItem push state
                    ]
                  , loadMoreView push state
                ]
          , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.ALERTS_SCREEN state.config.bottomNavConfig)
          ]
      ] <> (if (state.notifsDetailModelVisibility == VISIBLE) then [ notificationDetailModel push state ] else [])

loadMoreView :: forall w . (Action -> Effect Unit) -> NotificationsScreenState -> PrestoDOM (Effect Unit) w
loadMoreView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onClick push (const LoadMore)
    , gravity CENTER
    , alignParentBottom "true,-1"
    , padding (PaddingBottom 5)
    , visibility if (state.loaderButtonVisibility && (not state.loadMoreDisabled)) then VISIBLE else GONE
    ]
    [ linearLayout
        [height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ][]
      , textView
        ( [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString LOAD_OLDER_ALERTS)
          , padding (Padding 10 10 10 10)
          , color Color.blueTextColor
          ]
            <> FontStyle.subHeading1 TypoGraphy
        )
    ]


notificationDetailModel :: forall w. (Action -> Effect Unit) -> NotificationsScreenState -> PrestoDOM (Effect Unit) w
notificationDetailModel push state =
  PrestoAnim.animationSet
    [ fadeIn $ state.notifsDetailModelVisibility == VISIBLE
    , fadeOut $ state.notifsDetailModelVisibility == GONE
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , visibility state.notifsDetailModelVisibility
        ]
        [ NotificationDetailModel.view (push <<< NotificationDetailModelAC) state.notificationDetailModelState ]

notificationListView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NotificationsScreenState -> PrestoDOM (Effect Unit) w
notificationListView notificationListItem push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ]
    [ swipeRefreshLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , padding (PaddingBottom 20)
        , onRefresh push $ const Refresh
        , id $ getNewIDWithTag "NotificationSwipeRefresh"
        ]
        [ Keyed.relativeLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            ( [ Tuple "NOTIFICATION_LIST"
                  $ PrestoList.list
                      [ height MATCH_PARENT
                      , scrollBarY false
                      , width MATCH_PARENT
                      , onScroll "notifications" "NotificationsScreen" push $ Scroll
                      , onScrollStateChange push ScrollStateChanged
                      , visibility
                          $ case state.shimmerLoader of
                              AnimatedOut -> VISIBLE
                              _ -> GONE
                      , PrestoList.listItem notificationListItem
                      , PrestoList.listDataV2 state.prestoListArrayItems
                      ]
              , Tuple "SHIMMER_LOADER"
                  $ PrestoAnim.animationSet
                      [ PrestoAnim.Animation
                          [ PrestoAnim.duration 1000
                          , PrestoAnim.toAlpha
                              $ case state.shimmerLoader of
                                  AnimatingIn -> 1.0
                                  AnimatedIn -> 1.0
                                  AnimatingOut -> 0.0
                                  AnimatedOut -> 0.0
                          , PrestoAnim.fromAlpha
                              $ case state.shimmerLoader of
                                  AnimatingIn -> 0.0
                                  AnimatedIn -> 1.0
                                  AnimatingOut -> 1.0
                                  AnimatedOut -> 0.0
                          , PrestoAnim.tag "Shimmer"
                          ]
                          true
                      ]
                  $ PrestoList.list
                      [ height MATCH_PARENT
                      , scrollBarY false
                      , background Color.bg_grey
                      , width MATCH_PARENT
                      , onAnimationEnd push OnFadeComplete
                      , PrestoList.listItem notificationListItem
                      , PrestoList.listDataV2 $ shimmerData <$> (1 .. 5)
                      , visibility
                          $ case state.shimmerLoader of
                              AnimatedOut -> GONE
                              _ -> VISIBLE
                      ]
              , Tuple "NO_NOTIFICATION"
                  $ linearLayout
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      , background Color.white900
                      , visibility
                          $ case state.shimmerLoader of
                              AnimatedOut -> if length state.prestoListArrayItems > 0 then GONE else VISIBLE
                              _ -> GONE
                      ]
                      [ ErrorModal.view (push <<< ErrorModalActionController) (noNotificationsConfig Config)
                      ]
              ]
            )
        ]
    ]

headerLayout :: NotificationsScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
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
        , layoutGravity "center_vertical"
        , gravity CENTER_VERTICAL
        , padding $ Padding 5 8 5 8
        ]
        [ imageView
            [ width $ V 40
            , height $ V 40
            , imageWithFallback $ fetchImage FF_ASSET $ "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            , padding $ Padding 7 7 7 7
            , rippleColor Color.rippleShade
            , cornerRadius 20.0
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString ALL_MESSAGES
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

noNotificationsConfig :: LazyCheck -> ErrorModal.Config
noNotificationsConfig _ =
  let
    config = ErrorModal.config
    noNotificationsConfig' =
      config
        { imageConfig
          { imageUrl = "ny_ic_no_alerts"
          , height = V 116
          , width = V 188
          , margin = MarginBottom 16
          }
        , errorConfig
          { text = getString NO_NOTIFICATIONS_RIGHT_NOW
          , margin = MarginBottom 7
          , color = Color.black900      
          }
        , errorDescriptionConfig
          { text = getString NO_NOTIFICATIONS_RIGHT_NOW_DESC
          , color = Color.black700
          , margin = MarginHorizontal 40 40
          }
        , buttonConfig { visibility = GONE }
        }
  in
    noNotificationsConfig'

shimmerData :: Int -> NotificationCardPropState
shimmerData i =
  { mediaUrl: toPropValue ""
  , title: toPropValue ""
  , action1Text: toPropValue ""
  , action2Text: toPropValue ""
  , notificationLabel: toPropValue ""
  , timeLabel: toPropValue ""
  , description: toPropValue ""
  , cardVisibility: toPropValue "gone"
  , shimmerVisibility: toPropValue "visible"
  , notificationLabelColor: toPropValue ""
  , action1Visibility: toPropValue "visible"
  , action2Visibility: toPropValue "visible"
  , descriptionVisibility: toPropValue "visible"
  , illustrationVisibility: toPropValue "visible"
  , playBtnVisibility: toPropValue "gone"
  , notificationNotSeen: toPropValue ""
  , imageUrl: toPropValue ""
  , playButton: toPropValue "ic_play_btn"
  , previewImage: toPropValue "gone"
  , previewImageTitle: toPropValue "Preview Image"
  , imageVisibility : toPropValue "gone"
  , messageId: toPropValue ""
  , imageWithUrl : toPropValue ""
  , imageWithUrlVisibility : toPropValue ""
  , likeCount : toPropValue 0
  , viewCount : toPropValue 0
  , backgroundHolder : toPropValue ""
  , likeCountVisibility : toPropValue $ "visible"
  , shareCountVisibility : toPropValue $ "visible"
  , viewCountVisibility : toPropValue $ "visible"
  }
