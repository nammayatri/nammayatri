module Components.NotificationCard.View where

import Common.Types.App (LazyCheck(..))
import Components.NotificationCard.Controller (Action(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, map, ($), (<<<), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, fontStyle, gravity, height, imageView, linearLayout, margin, orientation, padding, relativeLayout, shimmerFrameLayout, stroke, textSize, textView, weight, width, ellipsize, maxLines)
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
        , textView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , PrestoList.visibilityHolder "previewImage"
            , PrestoList.textHolder "previewImageTitle"
            , gravity CENTER
            , cornerRadius 4.0
            , color Color.white900
            , background Color.black9000
            , textSize FontSize.a_20
            ]
        ]
    , titleAndLabel push
    , descriptionView push
    , descriptionWithBulletPoints push
    , actionAndTimeLabel push
    ]

titleAndLabel :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
titleAndLabel push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 5
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

descriptionView :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
descriptionView push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 5
    , PrestoList.visibilityHolder "descriptionVisibility"
    ]
    [ textView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , PrestoList.textFromHtmlHolder "description"
        , ellipsize true
        , maxLines 2
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
    ]

actionAndTimeLabel :: forall w. (NotificationsScreen.Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
actionAndTimeLabel push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 5
    ]
    [ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , color Color.blue900
        , PrestoList.textHolder "action1Text"
        , PrestoList.visibilityHolder "action1Visibility"
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , padding $ Padding 0 5 5 5
        , textSize FontSize.a_14
        , PrestoList.onClickHolder push $ NotificationsScreen.NotificationCardClick <<< Action1Click
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        [ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.blue900
            , margin $ MarginLeft 10
            , textSize FontSize.a_14
            , PrestoList.textHolder "action2Text"
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , PrestoList.visibilityHolder "action2Visibility"
            , padding $ Padding 5 5 5 5
            , PrestoList.onClickHolder push $ NotificationsScreen.NotificationCardClick <<< Action2Click
            ]
        ]
    , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , PrestoList.textHolder "timeLabel"
        , fontStyle $ FontStyle.medium LanguageStyle
        , color Color.black800
        , textSize FontSize.a_12
        ]
    ]

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
