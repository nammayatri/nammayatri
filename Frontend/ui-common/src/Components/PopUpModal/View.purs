{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PopUpModal.View where

import Prelude (Unit, const, unit, ($), (<>), (/), (-), (+), (==), (||), (&&), (>), not, (<<<), bind, discard, show, pure)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight, accessibilityHint, accessibility, textFromHtml)
import Components.PopUpModal.Controller (Action(..), Config)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenHeight, screenWidth)
import PrestoDOM.Properties (cornerRadii)
import Common.Types.App
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os, clearTimer, countDown)
import Data.Array ((!!), mapWithIndex)
import Data.Maybe (Maybe(..),fromMaybe)
import Control.Monad.Trans.Class (lift)
import JBridge (startTimerWithTime)
import Data.String (replaceAll, Replacement(..), Pattern(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , accessibility DISABLE
    , background state.backgroundColor
    , afterRender
        ( \action -> do
            if (state.option2.enableTimer || state.option1.enableTimer) then do
              let
                timerValue' = if state.option2.enableTimer then state.option2.timerValue else state.option1.timerValue
              if os == "IOS" then
                liftEffect $ startTimerWithTime (show timerValue') "" "1" push CountDown
              else
                countDown timerValue' "" push CountDown
              pure unit
            else
              pure unit
        )
        (const NoAction)
    , onClick
        ( \action -> do
            _ <- push action
            clearTheTimer state
            pure unit
        )
        if state.backgroundClickable && state.dismissPopup then const DismissPopup else if state.backgroundClickable then const OnButton1Click else const NoAction
    , gravity state.gravity
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin state.dismissIconMargin
        , gravity RIGHT
        , visibility state.dismissIconVisibility
        ][ imageView
            [ height $ V 21
            , width $ V 21
            , imageWithFallback "ny_ic_dismiss,https://assets.juspay.in/nammayatri/images/user/ny_ic_dismiss.png" 
            ]
        ]
     , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadii state.cornerRadius
        , orientation VERTICAL
        , background Color.white900
        , margin state.margin
        , padding state.padding
        , accessibility DISABLE
        , clickable true
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility state.coverImageConfig.visibility
            , cornerRadii state.cornerRadius
            , accessibility DISABLE_DESCENDANT
            , orientation VERTICAL
            ][  textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , margin $ MarginVertical 10 10
                , color Color.black800
                ] <> (case state.topTitle of
                        Just txt -> [text txt]
                        Nothing -> [visibility GONE])
                  <> (FontStyle.h2 LanguageStyle)
              , imageView
                [ height state.coverImageConfig.height
                , width state.coverImageConfig.width
                , margin state.coverImageConfig.margin
                , padding state.coverImageConfig.padding
                , imageWithFallback state.coverImageConfig.imageUrl
                , visibility state.coverImageConfig.visibility
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView $
                [ text $ state.primaryText.text
                , accessibilityHint state.primaryText.text
                , accessibility ENABLE
                , color $ state.primaryText.color
                , margin $ state.primaryText.margin
                , gravity $ state.primaryText.gravity
                , width if state.dismissPopupConfig.visibility == VISIBLE then WRAP_CONTENT else MATCH_PARENT
                , height WRAP_CONTENT
                , visibility $ state.primaryText.visibility
                ] <> (FontStyle.getFontStyle state.primaryText.textStyle LanguageStyle)
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity RIGHT
                , visibility state.dismissPopupConfig.visibility
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , margin state.dismissPopupConfig.margin
                    , onClick push $ const OnImageClick
                    , padding state.dismissPopupConfig.padding
                    ]
                    [ imageView
                        [ width state.dismissPopupConfig.width
                        , height state.dismissPopupConfig.height
                        , imageWithFallback state.dismissPopupConfig.imageUrl
                        , visibility state.dismissPopupConfig.visibility
                        ]
                    ]
                ]
            ]
        , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , color $ state.secondaryText.color
            , gravity $ state.secondaryText.gravity
            , padding state.secondaryText.padding
            , margin $ state.secondaryText.margin
            , text $ state.secondaryText.text
            , accessibility ENABLE
            , accessibilityHint $ replaceAll (Pattern ",") (Replacement ":") state.secondaryText.text
            , visibility $ state.secondaryText.visibility
            ] <> (FontStyle.getFontStyle state.secondaryText.textStyle LanguageStyle)
        , contactView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility state.editTextVisibility
            ]
            [ PrimaryEditText.view (push <<< ETextController) (state.eTextConfig) ]
        , tipsView push state
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , orientation HORIZONTAL
            , margin state.buttonLayoutMargin
            ]
            [ linearLayout
                [ width $ if state.optionButtonOrientation == "VERTICAL" then MATCH_PARENT else if (not state.option1.visibility) || (not state.option2.visibility) then MATCH_PARENT else WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation if state.optionButtonOrientation == "VERTICAL" then VERTICAL else HORIZONTAL
                ]
                [ linearLayout
                    [ if state.option2.visibility then width state.option1.width else weight 1.0
                    , background state.option1.background
                    , height $ V 48
                    , cornerRadius 8.0
                    , visibility $ if state.option1.visibility then VISIBLE else GONE
                    , stroke $ "1," <> state.option1.strokeColor
                    , clickable state.option1.isClickable
                    , alpha $ if state.option1.isClickable then 1.0 else 0.5
                    , margin  state.option1.margin
                    , padding  state.option1.padding
                    , gravity CENTER
                    , accessibility DISABLE
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const OnButton1Click)
                    ]
                    [   
                        imageView [
                            imageWithFallback state.option1.image.imageUrl
                            , height state.option1.image.height
                            , width state.option1.image.width
                            , margin state.option1.image.margin
                            , visibility state.option1.image.visibility
                            , padding state.option1.image.padding
                        ]
                        , textView $
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , accessibility ENABLE
                        , text $ if state.option1.enableTimer && state.option1.timerValue > 0 then (state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else state.option1.text
                        , accessibilityHint $ (if state.option1.enableTimer && state.option1.timerValue > 0 then ( state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else (replaceAll (Pattern ",") (Replacement ":") state.option1.text)) <> " : Button"
                        , color $ state.option1.color
                        , gravity CENTER
                        ] <> (FontStyle.getFontStyle state.option1.textStyle LanguageStyle)
                    ]
                , linearLayout
                    [ if state.option1.visibility then width state.option2.width else weight 1.0
                    , height state.option2.height
                    , background state.option2.background
                    , cornerRadius 8.0
                    , visibility if state.option2.visibility then VISIBLE else GONE
                    , stroke ("1," <> state.option2.strokeColor)
                    , margin state.option2.margin
                    , gravity CENTER
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const OnButton2Click)
                    , padding state.option2.padding
                    , accessibility DISABLE
                    , orientation VERTICAL
                    , clickable state.option2.isClickable
                    , alpha (if state.option2.isClickable then 1.0 else 0.5)
                    ]
                    [   imageView [
                            imageWithFallback state.option2.image.imageUrl
                            , height state.option2.image.height
                            , width state.option2.image.width
                            , margin state.option2.image.margin
                            , visibility state.option2.image.visibility
                            , padding state.option2.image.padding
                        ]
                        , textView $ 
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , accessibility ENABLE
                        , text $ if state.option2.enableTimer && state.option2.timerValue > 0 then (state.option2.text <> " (" <> (show state.option2.timerValue) <> ")") else state.option2.text
                        , accessibilityHint $ (if state.option2.enableTimer && state.option2.timerValue > 0 then (state.option2.text <> " (" <> (show state.option2.timerValue) <> ")") else (replaceAll (Pattern ",") (Replacement ":") state.option2.text)) <> " : Button"
                        , color state.option2.color
                        , gravity CENTER
                        ] <> (FontStyle.getFontStyle state.option2.textStyle LanguageStyle)
                    ]
                ]
            ]
        , linearLayout [
            height state.optionWithHtml.height
            , width  state.optionWithHtml.width
            , margin state.optionWithHtml.margin
            , clickable state.optionWithHtml.isClickable
            , background state.optionWithHtml.background
            , cornerRadius state.optionWithHtml.cornerRadius
            , visibility if state.optionWithHtml.visibility then VISIBLE else GONE
            , stroke ("1," <> state.optionWithHtml.strokeColor)
            , gravity CENTER
            , alpha (if state.optionWithHtml.isClickable then 1.0 else 0.5)
            , onClick
                ( \action -> do
                    _ <- push action
                    clearTheTimer state
                    pure unit
                )
                (const OptionWithHtmlClick)
          ][
                textView $
                [ textFromHtml $ state.optionWithHtml.textOpt1.text
                , accessibilityHint state.optionWithHtml.textOpt1.text
                , accessibility ENABLE
                , color $ state.optionWithHtml.textOpt1.color
                , margin $ state.optionWithHtml.textOpt1.margin
                , gravity $ state.optionWithHtml.textOpt1.gravity
                , visibility $ state.optionWithHtml.textOpt1.visibility
                ] <> (FontStyle.getFontStyle state.optionWithHtml.textOpt1.textStyle LanguageStyle)
                , imageView [
                    imageWithFallback state.optionWithHtml.image.imageUrl
                    , height state.optionWithHtml.image.height
                    , width state.optionWithHtml.image.width
                    , margin state.optionWithHtml.image.margin
                    , visibility state.optionWithHtml.image.visibility
                    , padding state.optionWithHtml.image.padding
                ]
                , textView $
                [ textFromHtml $ state.optionWithHtml.textOpt2.text
                , accessibilityHint state.optionWithHtml.textOpt2.text
                , accessibility ENABLE
                , color $ state.optionWithHtml.textOpt2.color
                , margin $ state.optionWithHtml.textOpt2.margin
                , gravity $ state.optionWithHtml.textOpt2.gravity
                , visibility $ state.optionWithHtml.textOpt2.visibility
                ] <> (FontStyle.getFontStyle state.optionWithHtml.textOpt2.textStyle LanguageStyle)
            ]
        ]
    ]

tipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tipsView push state =
  linearLayout
    [ 
      height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility if state.customerTipAvailable then VISIBLE else GONE
    , orientation VERTICAL
    , margin state.tipLayoutMargin
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility if state.customerTipAvailable then VISIBLE else GONE
        ]( mapWithIndex
        ( \index item ->
            linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , weight 1.0
              , margin $ state.tipButton.margin
              ]
              [ textView $
                  [ text item
                  , color $ state.tipButton.color
                  , visibility if state.tipButton.visibility then VISIBLE else GONE
                  , clickable $ state.tipButton.isClickable
                  , stroke $ "1," <> (if (state.activeIndex == index) then Color.blue800 else Color.grey900)
                  , cornerRadius 8.0
                  , width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , accessibilityHint $ "₹" <> show (fromMaybe 100 (state.customerTipArrayWithValues !! index))
                  , accessibility ENABLE
                  , padding state.tipButton.padding
                  , onClick push $ const $ Tipbtnclick index (fromMaybe 100 (state.customerTipArrayWithValues !! index))
                  , background (if (state.activeIndex == index) then Color.blue600 else state.tipButton.background)
                  ] <> (FontStyle.getFontStyle state.tipButton.textStyle LanguageStyle)
              ]
        ) state.customerTipArray)
    ,   linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.grey700
        , cornerRadius 4.0
        , margin $ MarginTop 12
        , padding $ Padding 20 13 20 13
        ]
        [ 
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ]
            [   imageView
                [ imageWithFallback "ny_ic_wallet_filled,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_wallet_filled.png"
                , width $ V 20
                , height $ V 20
                , margin $ MarginRight 5
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.fareEstimateText
                , weight 1.0
                , color Color.black800
                , textSize FontSize.a_12
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.fareEstimate
                , accessibilityHint (replaceAll (Pattern "-") (Replacement " To ") state.fareEstimate)
                , accessibility ENABLE
                , color Color.black800
                , textSize FontSize.a_14
                ]
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ MarginTop 14
            ]
            [   imageView
                [ imageWithFallback "ny_ic_stop_circle_yellow,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_stop_circle_yellow.png"
                , width $ V 20
                , height $ V 20
                , margin $ MarginRight 5
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.tipSelectedText
                , weight 1.0
                , color Color.black800
                , textSize FontSize.a_12
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.tipSelected
                , color Color.black800
                , textSize FontSize.a_14
                ]
            ]
        ]
    ]

clearTheTimer :: Config -> Effect Unit
clearTheTimer config =
  if config.option1.enableTimer then do
    pure $ clearTimer config.option1.timerID
  else if config.option2.enableTimer then do
    pure $ clearTimer config.option2.timerID
  else
    pure unit

contactView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
contactView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin state.contactViewMargin
    , stroke ("1," <> Color.borderColorLight)
    , padding state.contactViewPadding
    , cornerRadius 8.0
    , visibility state.contactViewConfig.visibility
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        ]
        [ linearLayout
            [ height $ V 24
            , width $ V 24
            , background Color.yellow900
            , cornerRadius 12.0
            , gravity CENTER
            ]
            [ textView $
                [ text state.contactViewConfig.nameInitials
                , color Color.black800
                ] <> FontStyle.body3 TypoGraphy
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding state.contactViewConfig.padding
            ]
            [ textView $
                [ text state.contactViewConfig.fullName
                , color Color.black800
                ] <> FontStyle.subHeading1 TypoGraphy
            ]
        ]
    ]
