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
import Debug.Trace (spy)
import Common.Types.App
import Constant.Test as Id


view :: forall w .  (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
view push = 
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , clickable true
      , background Color.white900
      , padding $ Padding 16 16 16 16
      , Id.testId $ Id.Component Id.pricingtutorial
      ][  closeBtnView push
        , textView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , textSize FontSize.a_20
            , text (getString HOW_THE_PRICING_WORKS)
            , color Color.black800
            , fontStyle $ FontStyle.bold LanguageStyle
            , gravity CENTER
            , margin (Margin 0 12 0 14)
            ]
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
        , Id.testId $ Id.Object Id.close
        ][  imageView
            [ height $ V 25
            , width $ V 25
            , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
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
                ][textView 
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , textSize FontSize.a_16
                  , text item.heading
                  , color Color.black800
                  , fontStyle $ FontStyle.semiBold LanguageStyle 
                  , margin (Margin 0 0 0 9)
                  ]
                , textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , textFromHtml item.subHeading
                  , color Color.black700
                  ]<> FontStyle.paragraphText TypoGraphy
                , textView 
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , textSize FontSize.a_12
                  , text (fromMaybe "" item.note)
                  , color Color.black700
                  , fontStyle $ FontStyle.medium LanguageStyle
                  , lineHeight "13"
                  , margin (MarginTop 6)
                  , visibility if item.note /= Nothing then VISIBLE else GONE
                  ]
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
      image : "ny_ic_ask_price,https://assets.juspay.in/nammayatri/images/user/ny_ic_ask_price.png"
    },
    { heading : (getString SELECT_AN_OFFER_FROM_OUR_DRIVERS),
      subHeading : (getString SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO),
      note : Nothing,
      image : "ny_ic_select_offer,https://assets.juspay.in/nammayatri/images/user/ny_ic_select_offer.png"
    },
    { heading : (getString PAY_THE_DRIVER),
      subHeading : (getString PAY_THE_DRIVER_INFO),
      note : Just (getString PAY_THE_DRIVER_NOTE),
      image : "ny_ic_pay_driver,https://assets.juspay.in/nammayatri/images/user/ny_ic_pay_driver.png"
    }
  ] 

type CardData =
  { heading :: String
  , image :: String
  , note :: Maybe String
  , subHeading :: String
  }


