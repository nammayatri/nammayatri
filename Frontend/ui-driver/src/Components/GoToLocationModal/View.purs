module Components.GoToLocationModal.View where

import Prelude
import Common.Types.App (LazyCheck(..))
import Components.GoToLocationModal.Controller (GoToModalConfig, Action(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Prelude (Unit, ($), const, unit, not, (<>), (/), (-), (==))
import PrestoDOM (PrestoDOM, Orientation(..), Gravity(..), Length(..), Padding(..), Margin(..), Visibility(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, clickable, onClick, color, background, lineHeight, visibility, cornerRadius, stroke, ellipsize, maxLines, imageWithFallback, weight, relativeLayout)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> GoToModalConfig -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (Padding 16 20 16 20)
    , margin (Margin 16 16 16 0)
    , stroke ("1," <> Color.grey900)
    , cornerRadius 8.0
    , onClick push $ const (CardClicked state)
    ]
    [ relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_HORIZONTAL
        ]
        [ imageView
            [ imageWithFallback case state.tag of
                "HOME_TAG" -> "ny_ic_home,https://assets.juspay.in/nammayatri/images/user/ny_ic_home.png"
                "WORK_TAG" -> "ny_ic_work,https://assets.juspay.in/nammayatri/images/user/ny_ic_work.png"
                "OTHER_TAG" -> "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
                _ -> "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
            , height $ V 20
            , margin (Margin 0 2 12 0)
            , width $ V 20
            ]
        , savedLocationView state push
        , if state.isSelectable then radioButton state push else linearLayout [] []
        ]
    ]

savedLocationView :: forall w. GoToModalConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginLeft 25
    , gravity LEFT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ linearLayout
            [ orientation HORIZONTAL
            , height WRAP_CONTENT
            , weight 1.0
            ]
            [ textView
                [ text state.tag
                , ellipsize true
                , maxLines 2
                , lineHeight "20"
                , textSize FontSize.a_16
                , weight 1.0
                , color Color.black800
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            ]
        , linearLayout
            [ orientation HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity RIGHT
            , visibility if state.isEditEnabled then VISIBLE else GONE
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding (Padding 4 4 4 4)
                , onClick push $ const (EditLocation state)
                , clickable true
                , margin (MarginRight 12)
                ]
                [ textView
                    $ [ width WRAP_CONTENT
                      , textSize FontSize.a_14
                      , color Color.blue900
                      , fontStyle $ FontStyle.medium LanguageStyle
                      ]
                    <> case state.editAcText of
                        Just txt -> [ text txt ]
                        Nothing -> [ visibility GONE ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding (Padding 4 4 4 4)
                , clickable true
                , onClick push $ const (DeleteLocation state)
                ]
                [ textView
                    $ [ textSize FontSize.a_14
                      , color Color.blue900
                      ]
                    <> FontStyle.body1 LanguageStyle
                    <> case state.removeAcText of
                        Just txt -> [ text txt ]
                        Nothing -> [ visibility GONE ]
                ]
            ]
        ]
    , textView
        $ [ text state.address
          , maxLines 2
          , ellipsize true
          , textSize FontSize.a_12
          , margin (MarginTop 8)
          , lineHeight "16"
          , fontStyle $ FontStyle.regular LanguageStyle
          , color Color.black700
          ]
    ]

radioButton :: forall w. GoToModalConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
radioButton state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity RIGHT
    ]
    [ relativeLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginTop 8
        ]
        [ imageView
            [ height (V 21)
            , width (V 21)
            , visibility if state.isSelected then GONE else VISIBLE
            , imageWithFallback "ny_ic_radio_unselected,"
            ]
        , imageView
            [ width (V 21)
            , height (V 21)
            , imageWithFallback $ "ny_ic_radio_selected,"
            , visibility if state.isSelected then VISIBLE else GONE
            ]
        ]
    ]
