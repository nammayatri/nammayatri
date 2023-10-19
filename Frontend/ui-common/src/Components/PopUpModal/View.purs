{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PopUpModal.View where

import Prelude (Unit, const, unit, ($), (<>), (/), (-), (+), (==), (||), (&&), (>), (/=),  not, (<<<), bind, discard, show, pure, map)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight, accessibilityHint, accessibility, textFromHtml, shimmerFrameLayout, onAnimationEnd, id)
import Components.PopUpModal.Controller (Action(..), Config, CoverVideoConfig,PrimaryButtonLayout)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Common.Styles.Colors as Color 
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenHeight, screenWidth, getNewIDWithTag, getVideoID, getYoutubeData)
import PrestoDOM.Properties (cornerRadii)
import Common.Types.App
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os, clearTimer, countDown)
import Data.Array ((!!), mapWithIndex, null)
import Data.Maybe (Maybe(..),fromMaybe)
import Control.Monad.Trans.Class (lift)
import JBridge (startTimerWithTime, setYoutubePlayer, supportsInbuildYoutubePlayer)
import Animation (fadeIn) as Anim
import Data.String (replaceAll, Replacement(..), Pattern(..))
import Data.Function.Uncurried (runFn3)
import PrestoDOM.Animation as PrestoAnim
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Components.PrimaryButton as PrimaryButton

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
            if (state.primaryButtonLayout.enableButton2Timer || state.primaryButtonLayout.enableButton1Timer) then do
              let
                timerValue' = if state.primaryButtonLayout.enableButton2Timer then state.primaryButtonLayout.button2TimerValue else state.primaryButtonLayout.button1TimerValue
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
        if state.backgroundClickable && state.dismissPopup then const DismissPopup else if state.backgroundClickable then const (PrimaryButton1 PrimaryButton.OnClick) else const NoAction
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
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_dismiss" 
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
                [ width state.topTitle.width
                , height state.topTitle.height
                , margin state.topTitle.margin
                , color state.topTitle.color
                , gravity state.topTitle.gravity
                , text state.topTitle.text
                , visibility state.topTitle.visibility
                ] <> (FontStyle.h2 LanguageStyle)
              , imageView
                [ height state.coverImageConfig.height
                , width state.coverImageConfig.width
                , margin state.coverImageConfig.margin
                , padding state.coverImageConfig.padding
                , imageWithFallback state.coverImageConfig.imageUrl
                , visibility state.coverImageConfig.visibility
                ]
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility state.coverVideoConfig.visibility
            , cornerRadii state.cornerRadius
            , accessibility DISABLE_DESCENDANT
            , orientation VERTICAL
            ][  textView $
                [ width state.topTitle.width
                , height state.topTitle.height
                , margin state.topTitle.margin
                , color state.topTitle.color
                , gravity state.topTitle.gravity
                , text state.topTitle.text
                , visibility state.topTitle.visibility
                ] <> (FontStyle.h2 LanguageStyle)
              , linearLayout[
                  height $ state.coverVideoConfig.height
                , width state.coverVideoConfig.width
                , width MATCH_PARENT
                , gravity CENTER
                ][  PrestoAnim.animationSet [Anim.fadeIn (state.coverVideoConfig.visibility == VISIBLE) ] $   linearLayout
                    [ height WRAP_CONTENT
                    , width state.coverVideoConfig.width
                    , margin state.coverVideoConfig.margin
                    , padding state.coverVideoConfig.padding
                    , cornerRadius 16.0
                    , visibility state.coverVideoConfig.visibility
                    , id (getNewIDWithTag  state.coverVideoConfig.id)
                    , onAnimationEnd
                        ( \action -> do
                            let
                                mediaType = state.coverVideoConfig.mediaType
                                id = getNewIDWithTag state.coverVideoConfig.id
                                url = state.coverVideoConfig.mediaUrl
                            if (supportsInbuildYoutubePlayer unit) then 
                                case mediaType of
                                    "VideoLink" -> pure $ runFn3 setYoutubePlayer (getYoutubeData (getVideoID url) "VIDEO" 0 ) id (show PLAY)
                                    "PortraitVideoLink" -> pure $ runFn3 setYoutubePlayer (getYoutubeData (getVideoID url) "PORTRAIT_VIDEO" 0) id (show PLAY)
                                    _ -> pure unit
                                else pure unit
                        )(const NoAction)
                    ][]
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
        , linearLayout
          [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ state.secondaryText.margin
            , padding state.secondaryText.padding
            , onClick push $ const OnSecondaryTextClick
          ][ textView $
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , color $ state.secondaryText.color
             , gravity $ state.secondaryText.gravity
             , textFromHtml state.secondaryText.text
             , accessibility ENABLE
             , accessibilityHint $ replaceAll (Pattern " ,") (Replacement ":") state.secondaryText.text
             , visibility $ state.secondaryText.visibility
             ]  <> (FontStyle.getFontStyle state.secondaryText.textStyle LanguageStyle)
            , imageView [
               width state.secondaryText.suffixImage.width
               , height state.secondaryText.suffixImage.height
               , imageWithFallback state.secondaryText.suffixImage.imageUrl
               , visibility state.secondaryText.suffixImage.visibility
               , margin state.secondaryText.suffixImage.margin
             ]
          ]
        , if (null state.listViewArray) then textView[height $ V 0] else listView push state
        , contactView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility state.editTextVisibility
            ]
            [ PrimaryEditText.view (push <<< ETextController) (state.eTextConfig) ]
        , tipsView push state
        , primaryButtonsView push state.primaryButtonLayout
        ]
    ]

listView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
listView push state = 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL 
    , margin $ Margin 16 0 16 20
    ](map (\item-> 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 12 0 0 4
        , orientation HORIZONTAL
        ][  imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_circle"
            , height $ V 6 
            , alpha 0.7
            , width $ V 6
            , margin $ MarginTop 8
            ]
          , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , margin $ MarginLeft 8
            , text item 
            , accessibility ENABLE
            , accessibilityHint item
            ] <> (FontStyle.getFontStyle FontStyle.ParagraphText LanguageStyle)

        ]
        ) state.listViewArray)

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
                  , accessibilityHint $ "₹" <> show (fromMaybe 100 (state.customerTipArrayWithValues !! index)) <> " : Button"
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
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_wallet_filled"
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
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_stop_circle_yellow"
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
  if config.primaryButtonLayout.enableButton1Timer then do
    pure $ clearTimer config.primaryButtonLayout.timer1ID
  else if config.primaryButtonLayout.enableButton2Timer then do
    pure $ clearTimer config.primaryButtonLayout.timer2ID
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


-- youtubeData :: CoverVideoConfig -> String -> YoutubeData
-- youtubeData state mediaType =
--   { videoTitle: "title"
--   , setVideoTitle: false
--   , showMenuButton: false
--   , showDuration: true
--   , showSeekBar: true
--   , videoId: getVideoID state.mediaUrl
--   , videoType: mediaType
--   }

primaryButtonsView :: forall w. (Action -> Effect Unit) -> PrimaryButtonLayout -> PrestoDOM (Effect Unit) w 
primaryButtonsView push config = 
    linearLayout[
      width config.width
    , height config.height
    , orientation config.orientation
    , gravity config.gravity
    , visibility config.visibility
    , margin config.margin
    ][
        linearLayout[
          weight 1.0
        , gravity CENTER
        , margin $ case (config.button1.visibility == VISIBLE && config.button2.visibility == VISIBLE), config.orientation of 
                    true, VERTICAL ->  MarginBottom config.gap
                    true, HORIZONTAL ->  MarginRight config.gap
                    _, _ -> Margin 0 0 0 0
        ][PrimaryButton.view (push <<< PrimaryButton1)  config.button1]
      , linearLayout[
          weight 1.0
        , gravity CENTER
        ][PrimaryButton.view (push <<< PrimaryButton2)  config.button2]
    ]
