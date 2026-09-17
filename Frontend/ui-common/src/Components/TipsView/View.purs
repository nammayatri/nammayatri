{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.TipsView.View where

import Components.TipsView.Controller (Action(..), Config)
import Effect (Effect)
import Prelude (class Eq, Unit, show, bind, const, map, pure, unit, not, void, ($), (&&), (+), (*), (/), (/=), (<<<), (<>), (==), (||), (>), (-), mod, discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Accessiblity(..), PrestoDOM, Visibility(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), afterRender, accessibilityHint ,alignParentBottom, background, clickable, color, cornerRadius, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility, rippleColor, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenWidth)
import Data.Array (filter, head, null, (!!), mapWithIndex, slice, length, cons, findIndex)
import Engineering.Helpers.Utils(splitIntoEqualParts, getFlexBoxCompatibleVersion)
import Data.Maybe (Maybe(..), fromMaybe)
import Styles.Colors as Color
import Mobility.Prelude
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Storage (getValueToLocalStore, KeyStore(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  let enableFlexBox = not $ (os == "IOS" || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (getFlexBoxCompatibleVersion "")))
  in
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin state.tipLayoutMargin
    ]
    [ if enableFlexBox then androidTipsView push state else iOSTipsView push state
    , tipInfoView push state
    ]

tipInfoView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tipInfoView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.grey700
  , cornerRadius 4.0
  , margin $ MarginTop 12
  , padding $ Padding 20 13 20 13
  , visibility $ boolToVisibility state.showTipInfo
  ]
  [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ]
      [ imageView
          [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_wallet_filled"
          , width $ V 20
          , height $ V 20
          , margin $ MarginRight 5
          ]
      , textView
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.fareEstimateText
          , weight 1.0
          , color Color.black800
          , textSize FontSize.a_12
          ]
      , textView
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.fareEstimate
          , accessibilityHint (replaceAll (Pattern "-") (Replacement " To ") state.fareEstimate)
          , accessibility ENABLE
          , color Color.black800
          , textSize FontSize.a_14
          ]
      ]
  , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 14
      , visibility $ boolToVisibility state.enableTips
      ]
      [ imageView
          [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_stop_circle_yellow"
          , width $ V 20
          , height $ V 20
          , margin $ MarginRight 5
          ]
      , textView
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.tipSelectedText
          , weight 1.0
          , color Color.black800
          , textSize FontSize.a_12
          ]
      , textView
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.tipSelected
          , color Color.black800
          , textSize FontSize.a_14
          ]
      ]
  ]

androidTipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
androidTipsView push state =
  let tipArraySize = length state.customerTipArray
  in
    flexBoxLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 16
      , justifyContent JUSTIFY_EVENLY
      , flexDirection ROW
      , flexWrap WRAP
      , alignItems ALIGN_BASELINE
      , visibility $ boolToVisibility $ state.enableTips && state.isVisible
      ]
      ( mapWithIndex
          ( \index item ->
              linearLayout
                ([ height WRAP_CONTENT
                , width WRAP_CONTENT
                , visibility $ boolToVisibility $ (index == 0 && state.searchExpired) || index /= 0
                , cornerRadius 8.0
                , stroke $ "1," <> (if (state.activeIndex == index) then Color.blue800 else Color.grey900)
                , accessibility ENABLE
                , padding $ if index == 0 then Padding 5 10 5 10 else Padding 8 10 8 10
                , accessibilityHint $ "₹" <> show (fromMaybe 100 (state.customerTipArrayWithValues !! index)) <> " Tip" <> (if (state.activeIndex == index) then " Selected" else " : Button")
                , onClick push $ const $ TipBtnClick index (fromMaybe 100 (state.customerTipArrayWithValues !! index))
                , clickable true
                , background $ if state.activeIndex == index then Color.blue600 else Color.white900
                , gravity CENTER
                ] <> if index == tipArraySize - 1 then [] else [margin $ MarginRight 8])
                [ textView
                    $ [ text $ item
                      , color $ Color.black800
                      , singleLine true
                      , gravity CENTER
                      ]
                    <> FontStyle.body6 LanguageStyle
                ]
          )
          state.customerTipArray
      )

iOSTipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
iOSTipsView push state =
  let
    tipValuesArr = splitIntoEqualParts 4 state.customerTipArray
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 16
      , orientation VERTICAL
      , visibility $ boolToVisibility $ state.enableTips && state.isVisible
      ]
      ( mapWithIndex
          ( \listIndex listItem ->
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , gravity CENTER_HORIZONTAL
                ]
                ( mapWithIndex
                    ( \itemIndex item ->
                        let
                          index = case findIndex (\x -> x == item) state.customerTipArray of
                            Just index -> index
                            Nothing -> 0
                        in
                          linearLayout
                            [ height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , margin $ tipsMargin itemIndex listIndex listItem
                            , visibility $ boolToVisibility $ (index == 0 && state.searchExpired) || index /= 0
                            , cornerRadius 8.0
                            , stroke $ "1," <> (if (state.activeIndex == index) then Color.blue800 else Color.grey900)
                            , accessibility ENABLE
                            , padding $ if index == 0 then Padding 5 10 5 10 else Padding 8 10 8 10
                            , accessibilityHint $ "₹" <> show (fromMaybe 100 (state.customerTipArrayWithValues !! index)) <> " : Button"
                            , onClick push $ const $ TipBtnClick index (fromMaybe 100 (state.customerTipArrayWithValues !! index))
                            , clickable $ true
                            , background $ if state.activeIndex == index then Color.blue600 else Color.white900
                            , gravity CENTER
                            ]
                            [ textView
                                $ [ text $ item
                                  , color $ Color.black800
                                  , singleLine true
                                  , gravity CENTER
                                  ]
                                <> FontStyle.body6 LanguageStyle
                            ]
                    )
                    listItem
                )
          )
          tipValuesArr
      )
  where
  tipsMargin :: Int -> Int -> Array String -> Margin
  tipsMargin index listIndex listItem = Margin 0 (if listIndex > 0 then 12 else 0) (if (index + 1) == (length listItem) then 0 else 12) 0
