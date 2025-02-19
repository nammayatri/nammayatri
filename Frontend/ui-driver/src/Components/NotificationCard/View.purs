{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.NotificationCard.View where

import Common.Types.App (LazyCheck(..))
import Components.NotificationCard.Controller (Action(..), CounterData(..), getCounters)
import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, map, ($), (<<<), (<>), (==), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, ellipsize, fontStyle, gravity, height, imageView, lineHeight, linearLayout, margin, maxLines, orientation, padding, relativeLayout, shimmerFrameLayout, stroke, textSize, textView, weight, width, imageWithFallback)
import PrestoDOM.List as PrestoList
import Screens.NotificationsScreen.Controller (Action(..)) as NotificationsScreen
import Styles.Colors as Color

view :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
view push =
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    ]
    [ shimmerView
    , notificationCardView push
    ]

notificationCardView :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
notificationCardView push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , PrestoList.visibilityHolder "cardVisibility"
    , orientation VERTICAL
    , margin $ Margin 16 20 16 4
    , cornerRadius 8.0
    , stroke $ "1," <> Color.grey900
    , background Color.white900
    , PrestoList.onClickHolder push $ NotificationsScreen.NotificationCardClick <<< IllutrationClick
    ]
    [ relativeLayout
        [ height $ V 159
        , width MATCH_PARENT
        , PrestoList.visibilityHolder "illustrationVisibility"
        ]
        [ linearLayout -- container for illustrationType
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , background Color.white900
            , PrestoList.visibilityHolder "imageVisibility"
            , cornerRadius 4.0
            ]
            [ imageView
                [ PrestoList.imageUrlHolder "imageUrl"
                , height MATCH_PARENT
                , width MATCH_PARENT
                , PrestoList.backgroundHolder "backgroundHolder"
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity CENTER
            , PrestoList.visibilityHolder "playBtnVisibility"
            ]
            [ imageView
                [ PrestoList.imageUrlHolder "playButton"
                , height $ V 40
                , width $ V 40
                ]
            ]
        , textView $
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , PrestoList.visibilityHolder "previewImage"
            , PrestoList.textHolder "previewImageTitle"
            , gravity CENTER
            , cornerRadius 4.0
            , color Color.white900
            , background Color.black9000
            ] <> FontStyle.body12 LanguageStyle
        , imageView
            [ width MATCH_PARENT
            , height $ V 159
            , PrestoList.visibilityHolder "imageWithUrlVisibility"
            , PrestoList.imageUrlHolder "imageWithUrl"
            , PrestoList.backgroundHolder "backgroundHolder"
            , gravity CENTER
            , cornerRadius 4.0
            ]
        ]
    , titleAndTimeView push
    , descriptionView push
    , descriptionWithBulletPoints push
    , actionAndCount push
    ]

titleAndTimeView :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
titleAndTimeView push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 5
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     ]
     [ textView
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , PrestoList.textHolder "title"
         , fontStyle $ FontStyle.bold LanguageStyle
         , color Color.black800
         , textSize FontSize.a_16
         ]
     , linearLayout
         [ height WRAP_CONTENT
         , weight 1.0
         ]
         []
     , linearLayout
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         , PrestoList.backgroundHolder "notificationLabelColor"
         , cornerRadius 14.0
         , padding $ Padding 10 3 10 5
         , PrestoList.visibilityHolder "notificationNotSeen"
         ]
         [ textView
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , color Color.white900
             , PrestoList.textHolder "notificationLabel"
             , fontStyle $ FontStyle.semiBold LanguageStyle
             , textSize FontSize.a_12
             ]
         ]
     ]
  , textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , PrestoList.textHolder "timeLabel"
    , fontStyle $ FontStyle.regular LanguageStyle
    , color Color.black500
    , textSize FontSize.a_10
    ]
  ]

descriptionView :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
descriptionView push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 5
    , PrestoList.visibilityHolder "descriptionVisibility"
    ]
    [ textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , PrestoList.textFromHtmlHolder "description"
        , ellipsize true
        , maxLines 2
        ] <> FontStyle.paragraphText LanguageStyle
    ]

actionAndCount :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
actionAndCount push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 5
    , gravity CENTER_VERTICAL
    ]
    [ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , color Color.blue900
        , PrestoList.textHolder "action1Text"
        , PrestoList.visibilityHolder "action1Visibility"
        , padding $ Padding 0 5 5 5
        , PrestoList.onClickHolder push $ NotificationsScreen.NotificationCardClick <<< Action1Click
        ] <> FontStyle.body6 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        [ textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.blue900
            , margin $ MarginLeft 10
            , PrestoList.textHolder "action2Text"
            , PrestoList.visibilityHolder "action2Visibility"
            , padding $ Padding 5 5 5 5
            , PrestoList.onClickHolder push $ NotificationsScreen.NotificationCardClick <<< Action2Click
            ] <> FontStyle.body6 TypoGraphy
        ]
    , counterView push
    ]

counterView :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
counterView push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    ]
    ( mapWithIndex
        ( \index (CounterData counter) ->
            linearLayout
              ( [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , cornerRadius 48.0
                , background Color.blue600
                , padding (Padding 8 4 8 4)
                , PrestoList.visibilityHolder counter.visibility
                , margin $ MarginLeft if index == 0 then 0 else 8
                ]
                  <> if counter.isShare then [ PrestoList.onClickHolder push $ NotificationsScreen.NotificationCardClick <<< ShareClick ] else []
              )
              ( [ imageView
                    [ imageWithFallback counter.icon
                    , height $ V 16
                    , width $ V 16
                    , margin $ MarginRight 4
                    ]
                ]
                  <> if not counter.isShare then
                      [ textView
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , PrestoList.textHolder counter.value
                          , color Color.black700
                          , textSize FontSize.a_12
                          , lineHeight "20"
                          , fontStyle $ FontStyle.medium LanguageStyle
                          ]
                      ]
                    else
                      []
              )
        )
        (getCounters "")
    )


descriptionWithBulletPoints :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
descriptionWithBulletPoints push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , PrestoList.visibilityHolder "bulletPointsVisibility"
    ]
    ( map
        ( \optionItem ->
            linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              ]
              [ linearLayout
                  [ width $ V 5
                  , height $ V 5
                  , background Color.black800
                  , cornerRadius 18.0
                  ]
                  []
              , textView
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , margin $ MarginLeft 5
                  , PrestoList.textHolder optionItem
                  ]
              ]
        )
        []
    )

shimmerView :: forall w. PrestoDOM (Effect Unit) w
shimmerView =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 20 16 20
    , PrestoList.visibilityHolder "shimmerVisibility"
    , orientation VERTICAL
    , margin $ Margin 16 20 16 4
    , cornerRadius 8.0
    , stroke $ "1," <> Color.grey900
    , background Color.white900
    ]
    [ shimmerFrameLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height $ V 150
            , background Color.grey900
            , cornerRadius 4.0
            ]
            []
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginTop 5
        ]
        [ sfl $ customTextView 20 50 0
        , linearLayout
            [ height $ V 20
            , weight 1.0
            ]
            []
        , sfl
            $ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , background Color.grey900
                , cornerRadius 12.0
                , padding $ Padding 6 3 6 3
                ]
                [ customTextView 20 60 0 ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginTop 10
        ]
        [ sfl $ customTextView 20 50 0
        , sfl $ customTextView 20 50 20
        , linearLayout
            [ height $ V 20
            , weight 1.0
            ]
            []
        , sfl $ customTextView 20 60 0
        ]
    ]

customTextView :: forall w. Int -> Int -> Int -> PrestoDOM (Effect Unit) w
customTextView height' width' marginLeft =
  textView
    [ width $ V width'
    , height $ V height'
    , background Color.grey900
    , margin $ MarginLeft marginLeft
    , cornerRadius 4.0
    ]

sfl :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
sfl a =
  shimmerFrameLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    ]
    [ a ]
