module Components.SavedLocationCard.View where

import Components.SavedLocationCard.Controller( Action(..), getCardType)
import Screens.Types (LocationListItemState, CardType(..))
import Effect (Effect)
import Prelude (Unit, ($), const, unit, not,(<>),(/),(-))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import PrestoDOM (PrestoDOM, Orientation(..), Gravity(..), Length(..), Padding(..), Margin(..), Visibility(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, clickable, onClick, color, background, lineHeight, visibility, cornerRadius, stroke, ellipsize, maxLines, imageWithFallback)
import Debug.Trace (spy)
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Engineering.Helpers.Commons(screenWidth)
import Data.Maybe(Maybe(..), fromMaybe)

view :: forall w. (Action -> Effect Unit) -> LocationListItemState -> PrestoDOM (Effect Unit) w 
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding (Padding 16 20 16 20)
  , margin (Margin 16 16 16 0)
  , stroke ("1,"<>Color.grey900)
  , cornerRadius 8.0
  , onClick push $ if (not state.isEditEnabled) then const (CardClicked state) else (const (EditLocation state))
  ][ linearLayout 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_HORIZONTAL
      ][  imageView
          [ imageWithFallback case (getCardType (fromMaybe "" state.cardType)) of
                Just card -> case card of 
                  HOME_TAG -> "ny_ic_home,https://assets.juspay.in/nammayatri/images/user/ny_ic_home.png"
                  WORK_TAG -> "ny_ic_work,https://assets.juspay.in/nammayatri/images/user/ny_ic_work.png"
                  OTHER_TAG -> "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
                Nothing   -> "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
          , height $ V 20
          , margin (Margin 0 2 12 0)
          , width $ V 20
          ]
        , savedLocationView state push 
      ]

  ]

savedLocationView :: forall w. LocationListItemState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationView state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity LEFT 
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][  linearLayout
          [ orientation HORIZONTAL
          , height WRAP_CONTENT
          , width $ V ((screenWidth unit / 2) - 28)
          , onClick push $ if (not state.isEditEnabled) then const (CardClicked state) else const (EditLocation state)
          ][  textView
              [ text case (getCardType (fromMaybe "" state.cardType)) of 
                    Just tag -> case tag of 
                      HOME_TAG -> (getString HOME)
                      WORK_TAG -> (getString WORK)
                      OTHER_TAG -> state.tagName
                    Nothing -> state.tagName
              , ellipsize true
              , maxLines 2
              , lineHeight "20"
              , textSize FontSize.a_16
              , color Color.black800
              , fontStyle $ FontStyle.semiBold LanguageStyle
              ]
            ]
        , linearLayout
        [ orientation HORIZONTAL
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT 
        , visibility if state.isEditEnabled then VISIBLE else GONE
        ][  linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding (Padding 4 4 4 4)
            , onClick push $ const (EditLocation state)
            , clickable true
            , margin (MarginRight 12) 
            ][  textView
                [ text (getString EDIT)
                , textSize FontSize.a_14
                , color Color.blue900
                , fontStyle $ FontStyle.medium LanguageStyle
                ]
              ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding (Padding 4 4 4 4)
            , clickable true
            , onClick push $ const (DeleteLocation state.tagName)
            ][  textView
                [ text (getString REMOVE)
                , textSize FontSize.a_14
                , color Color.blue900
                , fontStyle $ FontStyle.medium LanguageStyle
                ]
              ]
          ]
        ]
    , textView 
      [ text state.savedLocation
      , maxLines 2 
      , ellipsize true
      , onClick push $ if (not state.isEditEnabled) then const (CardClicked state) else const (EditLocation state)
      , textSize FontSize.a_12
      , margin (MarginTop 8)
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black700 
      ]
  ]