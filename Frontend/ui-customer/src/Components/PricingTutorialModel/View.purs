{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PricingTutorialModel.View where

import Components.PricingTutorialModel.Controller (Action(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, ($), (/=), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, textFromHtml, imageWithFallback)
import Styles.Colors as Color
import Debug (spy)
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.MobilityPrelude

view :: forall w .  (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
view push =
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , clickable true
      , background Color.white900
      , padding $ Padding 16 16 16 16
      ][  closeBtnView push
        , textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text (getString HOW_THE_PRICING_WORKS)
            , color Color.black800
            , gravity CENTER
            , margin (Margin 0 12 0 14)
            ] <> FontStyle.body8 TypoGraphy
        , listComponentView ""
      ]


closeBtnView :: forall w .  (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
closeBtnView push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity RIGHT
    ][  linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , onClick push $ const Close
        ][  imageView
            [ height $ V 25
            , width $ V 25
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
            , margin (Margin 12 12 12 12)
            ]
          ]
        ]


listComponentView :: forall w . String -> PrestoDOM (Effect Unit) w
listComponentView dummy =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (Margin 0 0 0 10)
    ](map (\item ->
        linearLayout
          [ width MATCH_PARENT
          , height if (item.image == "ic_mask_3") then (V 146) else WRAP_CONTENT
          , orientation VERTICAL
          , background Color.catskillWhite
          , cornerRadius 20.0
          , margin (Margin 0 16 0 0)
          ][linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , margin (Margin 0 0 0 0)
              -- , gravity CENTER
              ][linearLayout
                [ height WRAP_CONTENT
                , width $ V 0
                , weight 1.0
                , orientation VERTICAL
                , margin (Margin 16 20 10 9)
                ][textView (
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text item.heading
                  , color Color.black800
                  , margin (Margin 0 0 0 9)
                  ] <> FontStyle.subHeading1 TypoGraphy)
                , textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , textFromHtml item.subHeading
                  , color Color.black700
                  ]<> FontStyle.paragraphText TypoGraphy
                , textView (
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text (fromMaybeString item.note)
                  , color Color.black700
                  , lineHeight "13"
                  , margin (MarginTop 6)
                  , visibility if item.note /= Nothing then VISIBLE else GONE
                  ] <> FontStyle.tags TypoGraphy)
                ]
                , imageView
                      [height $ V 110
                      , width $ V 110
                      , imageWithFallback item.image
                      ]
                ]
            ]) (cardData ""))


cardData :: String -> Array CardData
cardData dummy =
  [
    { heading : (getString GET_ESTIMATE_FARE),
      subHeading : (getString ASK_FOR_PRICE_INFO),
      note : Nothing,
      image : fetchImage FF_ASSET "ny_ic_ask_price"
    },
    { heading : (getString SELECT_AN_OFFER_FROM_OUR_DRIVERS),
      subHeading : (getString SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO),
      note : Nothing,
      image : fetchImage FF_ASSET "ny_ic_select_offer"
    },
    { heading : (getString PAY_THE_DRIVER),
      subHeading : (getString PAY_THE_DRIVER_INFO),
      note : Just (getString PAY_THE_DRIVER_NOTE),
      image : fetchImage FF_ASSET"ny_ic_pay_driver"
    }
  ]

type CardData =
  { heading :: String
  , image :: String
  , note :: Maybe String
  , subHeading :: String
  }


