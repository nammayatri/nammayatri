{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PopUpModal.View where

import Prelude 
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight, accessibilityHint, accessibility, textFromHtml, shimmerFrameLayout, onAnimationEnd, id, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems, rippleColor, lottieAnimationView, frameLayout, maxLines, ellipsize, scrollView, singleLine)
import Components.PopUpModal.Controller (Action(..), Config, CoverMediaConfig, DropdownAction(..), DropdownConfig(..), DropdownItem(..), RouteInfo(..), dummyDeliveryPrimaryText)
import PrestoDOM.Properties (lineHeight, cornerRadii, zIndex)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Components.TipsView as TipsView
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenHeight, screenWidth, getNewIDWithTag, getVideoID, getYoutubeData)
import PrestoDOM.Properties (cornerRadii, layoutGravity)
import Common.Types.App
import Language.Strings (getString)
import Language.Types (STR(..))
import Timers
import Animation (fadeIn) as Anim
import Common.Styles.Colors as Colors
import Components.PopUpModal.Controller (Action(..), Config, CoverMediaConfig)
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Components.PrimaryEditText.View as PrimaryEditText
import Components.TipsView as TipsView
import Engineering.Helpers.Commons as EHC
import Components.DropdownTextField.View as DropdownTextField
import Control.Monad.Trans.Class (lift)
import Data.Function.Uncurried (runFn5)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.String (replaceAll, Replacement(..), Pattern(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os, getNewIDWithTag)
import Data.Array ((!!), mapWithIndex, null, length, findIndex)
import JBridge 
import Animation (fadeIn) as Anim
import Data.String (replaceAll, Replacement(..), Pattern(..))
import Data.Function.Uncurried (runFn5)
import PrestoDOM.Animation as PrestoAnim
import Engineering.Helpers.Commons (screenHeight, screenWidth, getNewIDWithTag, getVideoID, getYoutubeData)
import Engineering.Helpers.Utils (splitIntoEqualParts)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (setYoutubePlayer, supportsInbuildYoutubePlayer, addMediaPlayer)
import Mobility.Prelude (boolToVisibility, noView)
import Engineering.Helpers.Utils(splitIntoEqualParts)
import Animation as Anim
import Prelude (Unit, const, unit, ($), (<>), (/), (-), (+), (==), (||), (&&), (>), (/=), not, (<<<), bind, discard, show, pure, map, when, mod)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight, accessibilityHint, accessibility, textFromHtml, shimmerFrameLayout, onAnimationEnd, id, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems, rippleColor)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.DropDownCard as DropDownCard
import Prim.Boolean (True)
import Debug
import Constants (languageKey)
import Locale.Utils (getLanguageLocale)
import Common.Animation.Config (estimateExpandingAnimationConfig)
import Debug (spy)
import Components.PrimaryButton as PrimaryButton
import Components.SelectRouteButton as SelectRouteButton
import Components.SelectableItems as SelectableItems
import Components.SelectableItem as SelectableItem
import Components.SearchableList.View as SearchableListView
import Components.SearchableList.Controller as SearchableList
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , zIndex 2.0
    , accessibility DISABLE
    , background state.backgroundColor
    , afterRender
        ( \action -> do
            when 
              (state.option2.enableTimer || state.option1.enableTimer)
                $ do
                  let timerValue' = if state.option2.enableTimer then state.option2.timerValue else state.option1.timerValue
                  startTimer timerValue' state.timerId "1" push CountDown
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
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_dismiss" 
            ]
        ]
     , linearLayout
        [ width MATCH_PARENT
        , height state.height
        , cornerRadii state.cornerRadius
        , orientation VERTICAL
        , background state.background
        , margin state.margin
        , padding state.padding
        , accessibility DISABLE
        , clickable true
        , onClick push $ const if state.outsideClickable then OutSideClick else NoAction
        ]
        [ linearLayout 
        [  width state.popUpHeaderConfig.width
            , height state.popUpHeaderConfig.width
            , margin state.popUpHeaderConfig.margin
            , gravity state.popUpHeaderConfig.gravity
            , visibility state.popUpHeaderConfig.visibility
            , cornerRadii (Corners (parseConerRadius state.cornerRadius) true true false false)
            , padding state.popUpHeaderConfig.padding
            , background state.popUpHeaderConfig.backgroundColor
            , orientation if state.popUpHeaderConfig.orientation == "VERTICAL" then VERTICAL else HORIZONTAL  -----HORIZONTAL
        ]   
        [ 
            linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , cornerRadii (Corners (parseConerRadius state.cornerRadius) true true false false)
            , orientation VERTICAL
            , background state.popUpHeaderConfig.backgroundColor
            , weight 1.0
            , visibility $ state.popUpHeaderConfig.visibilityV2
            ]
        [textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin state.popUpHeaderConfig.headingText.margin
            , color state.popUpHeaderConfig.headingText.color
            , gravity state.popUpHeaderConfig.headingText.gravity
            , text state.popUpHeaderConfig.headingText.text
            , visibility state.popUpHeaderConfig.headingText.visibility
            ] <> (FontStyle.getFontStyle state.popUpHeaderConfig.headingText.textStyle LanguageStyle)
         , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin state.popUpHeaderConfig.subHeadingText.margin
            , color state.popUpHeaderConfig.subHeadingText.color
            , gravity state.popUpHeaderConfig.subHeadingText.gravity
            , text state.popUpHeaderConfig.subHeadingText.text
            , visibility state.popUpHeaderConfig.subHeadingText.visibility
            ] <> (FontStyle.getFontStyle state.popUpHeaderConfig.subHeadingText.textStyle LanguageStyle)
        ],
        imageView 
            [  width state.popUpHeaderConfig.imageConfig.width
                , height state.popUpHeaderConfig.imageConfig.height
                , imageWithFallback state.popUpHeaderConfig.imageConfig.imageUrl
                , margin state.popUpHeaderConfig.imageConfig.margin
                , cornerRadii (Corners (parseConerRadius state.cornerRadius) false true false false)
                , visibility state.popUpHeaderConfig.imageConfig.visibility
            ]
        ] 
        ,    linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility if state.topTextVisibility then VISIBLE else state.coverImageConfig.visibility
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
                ] <> (FontStyle.getFontStyle state.topTitle.textStyle LanguageStyle)
              , imageView
                [ height state.coverImageConfig.height
                , width state.coverImageConfig.width
                , margin state.coverImageConfig.margin
                , padding state.coverImageConfig.padding
                , imageWithFallback state.coverImageConfig.imageUrl
                , visibility state.coverImageConfig.visibility
                , onClick push $ const OnCoverImageClick
                ]
            ]
        ,   Anim.screenAnimationFadeInOut
            $ lottieAnimationView
                [ id state.coverLottieConfig.id
                , onAnimationEnd
                    ( \action -> void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = state.coverLottieConfig.lottieUrl, lottieId = state.coverLottieConfig.id, scaleType = "FIT_CENTER", repeat =  state.coverLottieConfig.repeat }
                    )
                    (const NoAction)
                , height state.coverLottieConfig.height
                , width state.coverLottieConfig.width
                , visibility state.coverLottieConfig.visibility
                , margin state.coverLottieConfig.margin
                , padding state.coverLottieConfig.padding
                ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility state.coverMediaConfig.visibility
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
                , visibility $ boolToVisibility $ state.topTitle.visibility == VISIBLE && state.onlyTopTitle == VISIBLE
                ] <> (FontStyle.h2 LanguageStyle) 
             , linearLayout
                [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , gravity CENTER
                    , margin $ state.coverMediaConfig.coverMediaText.margin
                    , padding state.coverMediaConfig.coverMediaText.padding
                    , visibility $ state.coverMediaConfig.coverMediaText.visibility
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color $ state.coverMediaConfig.coverMediaText.color
                    , gravity $ state.coverMediaConfig.coverMediaText.gravity
                    , textFromHtml state.coverMediaConfig.coverMediaText.text
                    , accessibility ENABLE
                    , accessibilityHint $ replaceAll (Pattern " ,") (Replacement ":") state.coverMediaConfig.coverMediaText.text
                    , visibility $ state.coverMediaConfig.coverMediaText.visibility
                    ]  <> (FontStyle.getFontStyle state.coverMediaConfig.coverMediaText.textStyle LanguageStyle)
                    , imageView [
                    width state.coverMediaConfig.coverMediaText.suffixImage.width
                    , height state.coverMediaConfig.coverMediaText.suffixImage.height
                    , imageWithFallback state.coverMediaConfig.coverMediaText.suffixImage.imageUrl
                    , visibility state.coverMediaConfig.coverMediaText.suffixImage.visibility
                    , margin state.coverMediaConfig.coverMediaText.suffixImage.margin
                    ]
                ]
              , linearLayout[
                 height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin state.coverMediaConfig.margin
                , background state.coverMediaConfig.background
                , stroke state.coverMediaConfig.stroke
                , visibility state.coverMediaConfig.visibility
                , cornerRadius state.coverMediaConfig.cornerRadius
                , padding state.coverMediaConfig.padding
                ][  PrestoAnim.animationSet [Anim.fadeIn (state.coverMediaConfig.visibility == VISIBLE) ] $   linearLayout
                    [ height state.coverMediaConfig.height
                    , width state.coverMediaConfig.width
                    , gravity CENTER
                    , id (getNewIDWithTag  state.coverMediaConfig.id)
                    , onAnimationEnd
                        ( \action -> do
                            let
                                mediaType = state.coverMediaConfig.mediaType
                                id = getNewIDWithTag state.coverMediaConfig.id
                                url = state.coverMediaConfig.mediaUrl
                                audioAutoPlay = state.coverMediaConfig.audioAutoPlay
                            if (supportsInbuildYoutubePlayer unit) then 
                                case mediaType of
                                    "VideoLink" -> pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig  "VIDEO" (getVideoID url)) id (show PLAY) push YoutubeVideoStatus
                                    "PortraitVideoLink" -> pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig  "PORTRAIT_VIDEO" (getVideoID url)) id (show PLAY) push YoutubeVideoStatus
                                    "Audio" -> addMediaPlayer id url audioAutoPlay
                                    "AudioLink" -> addMediaPlayer id url audioAutoPlay
                                    _ -> pure unit
                                else pure unit
                        )(const NoAction)
                    ][]
                  ]
                ]
        , linearLayout[
            height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          ] [
            PrestoAnim.animationSet [ Anim.fadeInWithDelay 10 true ] $ lottieAnimationView [ 
              id state.coverLottie.id
            , background state.coverLottie.background
            , cornerRadius state.coverLottie.cornerRadius
            , height state.coverLottie.height
            , padding state.coverLottie.padding
            , width state.coverLottie.width
            , visibility state.coverLottie.visibility
            , margin state.coverLottie.margin
            , gravity CENTER_HORIZONTAL
            , onAnimationEnd (\_-> void $ pure $ startLottieProcess state.coverLottie.config )(const NoAction)
            ]
          ]
        , maybe noView (\layout -> layout) state.completeProfileLayout
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ imageView [
               width state.primaryText.prefixImage.width
               , height state.primaryText.prefixImage.height
               , imageWithFallback state.primaryText.prefixImage.imageUrl
               , visibility state.primaryText.prefixImage.visibility
               , margin state.primaryText.prefixImage.margin
                ]
            , textView $
                ([ text $ state.primaryText.text
                , accessibilityHint state.primaryText.text
                , accessibility ENABLE
                , color $ state.primaryText.color
                , width if state.dismissPopupConfig.visibility == VISIBLE || state.headerInfo.visibility == VISIBLE then WRAP_CONTENT else MATCH_PARENT
                , height WRAP_CONTENT
                , visibility $ state.primaryText.visibility
                , margin $ state.primaryText.margin
                , gravity $ state.primaryText.gravity
                ] <> (if state.primaryText.lineheight /= Nothing then [lineHeight $ fromMaybe "" state.primaryText.lineheight] else [])) <> (FontStyle.getFontStyle state.primaryText.textStyle LanguageStyle)
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
                    [ 
                        imageView
                        [ width state.dismissPopupConfig.width
                        , height state.dismissPopupConfig.height
                        , imageWithFallback state.dismissPopupConfig.imageUrl
                        , visibility state.dismissPopupConfig.visibility
                        ]
                    ]
                ]
            , linearLayout
                [ height (V 28)
                , width MATCH_PARENT
                , gravity RIGHT
                , visibility state.headerInfo.visibility
                ]
                [ 
                    textView $
                    [ text state.headerInfo.text
                    , accessibilityHint state.headerInfo.text
                    , accessibility ENABLE
                    , color Colors.black600
                    , gravity CENTER
                    , width  WRAP_CONTENT 
                    , height WRAP_CONTENT
                    , visibility $ state.headerInfo.visibility
                    ] <> (FontStyle.body1 TypoGraphy)
                ]
            ]
        , linearLayout
          [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity state.secondaryText.gravity
            , margin $ state.secondaryText.margin
            , padding state.secondaryText.padding
            , visibility $ state.secondaryText.visibility
            , onClick push $ const OnSecondaryTextClick
            , clickable $ state.secondaryText.isClickable
          ][ imageView [
               width state.secondaryText.prefixImage.width
               , height state.secondaryText.prefixImage.height
               , imageWithFallback state.secondaryText.prefixImage.imageUrl
               , visibility state.secondaryText.prefixImage.visibility
               , margin state.secondaryText.prefixImage.margin
             ]
            , textView $
             ([ width WRAP_CONTENT
             , height WRAP_CONTENT
             , color $ state.secondaryText.color
             , gravity $ state.secondaryText.gravity
             , if state.secondaryText.useTextFromHtml then textFromHtml state.secondaryText.text else text state.secondaryText.text
             , accessibility ENABLE
             , accessibilityHint $ replaceAll (Pattern " ,") (Replacement ":") (if state.secondaryText.accessibilityHint /= ""  then state.secondaryText.accessibilityHint else state.secondaryText.text)
             , visibility $ state.secondaryText.visibility
             ] <> (if state.secondaryText.lineheight /= Nothing then [lineHeight $ fromMaybe "" state.secondaryText.lineheight] else []))  <> (FontStyle.getFontStyle state.secondaryText.textStyle LanguageStyle)
            , imageView [
               width state.secondaryText.suffixImage.width
               , height state.secondaryText.suffixImage.height
               , imageWithFallback state.secondaryText.suffixImage.imageUrl
               , visibility state.secondaryText.suffixImage.visibility
               , margin state.secondaryText.suffixImage.margin
             ]
          ]
        , policyDownloadView push state
        , deliveryView push state
        , parcelDetailsView push state
        , selectItemsView push state
        , dropdownTextFieldView push state
        , searchableListView push state
        , upiView push state
        , if state.showCallNotPickingUpConfig then customerNotPickingUpTheCall push state else textView [ visibility GONE ]
        , linearLayout[][]
        , if (null state.listViewArray) then textView[height $ V 0] else listView push state
        , contactView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility state.editTextVisibility
            ]
            [ PrimaryEditText.view (push <<< ETextController) (state.eTextConfig) ]
        , tipsView push state
        , case state.layout of
            Just layout -> layout { visibility : VISIBLE }
            Nothing -> textView [ visibility GONE]
        , 
        linearLayout[
            height state.optionsConfig.height
            , width state.optionsConfig.width
            , orientation state.optionsConfig.orientation
            , alignParentBottom state.optionsConfig.alignParentBottom
            , gravity state.optionsConfig.gravity
        ] $
        [
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , orientation HORIZONTAL
            , margin state.buttonLayoutMargin
            ]
            [   linearLayout
                [ width $ if state.optionButtonOrientation == "VERTICAL" then MATCH_PARENT else if (not state.option1.visibility) || (not state.option2.visibility) || ( state.deliveryDetailsConfig.visibility == VISIBLE || state.parcelDetailsVisibility == VISIBLE) then MATCH_PARENT else WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation if state.optionButtonOrientation == "VERTICAL" then VERTICAL else HORIZONTAL
                ]
                [ linearLayout
                    ([ if state.option2.visibility && state.deliveryDetailsConfig.visibility  == GONE && state.parcelDetailsVisibility == GONE then width state.option1.width 
                       else if state.option1.useWeight then weight 1.0 else width state.option1.width
                    , background state.option1.background
                    , height $ state.option1.height
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
                    ] <> (if state.option1.enableRipple then [rippleColor state.option1.rippleColor] else [])
                      <> (if (isJust state.option1.layoutGravity) then [layoutGravity $ fromMaybe "center" state.option1.layoutGravity] else []))
                    [   shimmerFrameLayout
                        [ width MATCH_PARENT
                        , height MATCH_PARENT
                        , visibility $  if state.option1.showShimmer then VISIBLE else GONE
                        , cornerRadius 8.0
                        ][ linearLayout
                            [ width MATCH_PARENT
                            , height MATCH_PARENT
                            , background Colors.white200
                            , cornerRadius 8.0
                            ][]
                        ]
                        , linearLayout
                        [ width $ MATCH_PARENT
                        , height $ MATCH_PARENT
                        , visibility $ if state.option1.showShimmer then GONE else VISIBLE
                        , gravity state.option1.gravity
                        ]
                        [ imageView [
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
                            , text  (if state.option1.enableTimer && state.option1.timerValue > 0 then (state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else state.option1.text)
                            , accessibilityHint $ (if state.option1.enableTimer && state.option1.timerValue > 0 then ( state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else (replaceAll (Pattern ",") (Replacement ":") state.option1.text)) <> " : Button"
                            , color $ state.option1.color
                            , gravity CENTER
                            ] <> (FontStyle.getFontStyle state.option1.textStyle LanguageStyle)
                        ]
                    ]
                , linearLayout
                    ([ if state.deliveryDetailsConfig.visibility == VISIBLE || state.parcelDetailsVisibility == VISIBLE then weight 1.0
                       else if not state.showRetry then width state.option1.width 
                       else if state.option1.visibility then width state.option2.width else weight 1.0
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
                    , gravity state.option2.gravity
                    , clickable state.option2.isClickable
                    , alpha (if state.option2.isClickable then 1.0 else 0.5)
                    ] <> (if state.option2.enableRipple then [rippleColor state.option2.rippleColor] else []))
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
    where 
    getYoutubeDataConfig videoType videoId = getYoutubeData {
        videoType = videoType,
        videoId = videoId
        }

tipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
tipsView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility $ boolToVisibility state.isTipPopup
  ][ TipsView.view (push <<< TipsViewActionController) $ tipsViewConfig state ]

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

clearTheTimer :: Config -> Effect Unit
clearTheTimer config =
  if config.option1.enableTimer then do
    pure $ clearTimerWithId config.option1.timerID
  else if config.option2.enableTimer then do
    pure $ clearTimerWithId config.option2.timerID
  else
    pure unit

contactView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
contactView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin state.contactViewMargin
    , stroke ("1," <> Colors.borderColorLight)
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
            , background Colors.yellow900
            , cornerRadius 12.0
            , gravity CENTER
            ]
            [ textView $
                [ text state.contactViewConfig.nameInitials
                , color Colors.black800
                ] <> FontStyle.body3 TypoGraphy
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding state.contactViewConfig.padding
            ]
            [ textView $
                [ text state.contactViewConfig.fullName
                , color Colors.black800
                ] <> FontStyle.subHeading1 TypoGraphy
            ]
        ]
    ]

tipsViewConfig :: Config -> TipsView.Config
tipsViewConfig state = let  
  config = TipsView.config
  tipsViewConfig' = config {
    activeIndex = state.activeIndex
  , isVisible = state.isVisible
  , customerTipArray = state.customerTipArray
  , customerTipArrayWithValues = state.customerTipArrayWithValues
  , tipLayoutMargin = (Margin 22 2 22 22)
  , fareEstimate = state.fareEstimate
  , fareEstimateText = state.fareEstimateText
  , tipSelected = state.tipSelected
  , tipSelectedText = state.tipSelectedText
  , showTipInfo = true
  , enableTips = state.isTipEnabled
  , searchExpired = true
  }
  in tipsViewConfig'


parseConerRadius :: Corners -> Number
parseConerRadius corners =
  case corners of
    Corners radius _ _ _ _ -> radius
    Corner radius -> radius

upiView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
upiView push state = 
  let imageConfig = state.upiDetailConfig.imageConfig
      hasSecondaryText = state.upiDetailConfig.accountName /= ""
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginTop 16
  , cornerRadius 12.0
  , background Colors.grey700
  , padding $ Padding 12 8 12 8
  , gravity CENTER_VERTICAL
  , visibility state.upiDetailConfig.visibility
  ][imageView
    [ imageWithFallback $ imageConfig.imageUrl
    , height $ imageConfig.height
    , width $ imageConfig.width
    , margin $ imageConfig.margin
    , padding $ imageConfig.padding
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingBottom $ if ((getLanguageLocale languageKey) == "EN_US") then 4 else 0
    ][textView $
      [ text state.upiDetailConfig.upiID
      , color Colors.black800
      ] <> FontStyle.subHeading3 TypoGraphy 
    , textView $
      [ text state.upiDetailConfig.accountName
      , color Colors.black700
      , visibility $ boolToVisibility $ hasSecondaryText
      ] <> FontStyle.body23 TypoGraphy 
    ]
  ]

deliveryView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
deliveryView push state = 
    linearLayout
    [ height WRAP_CONTENT,
      width MATCH_PARENT,
      visibility state.deliveryDetailsConfig.visibility
    ]
    [
        deliveryDetailsView push state
    ]

policyDownloadView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
policyDownloadView push state = 
    linearLayout[
        height WRAP_CONTENT,
        width MATCH_PARENT,
        gravity CENTER,
        margin $ MarginTop 27,
        visibility $ boolToVisibility $ state.showDownloadPolicy == true,
        onClick
            ( \action -> do
                void $ openUrlInApp state.certificateUrl
                pure unit
            )
            $ const NoAction
    ][
        imageView
        [ width (V 18)
        , height (V 18)
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_download_icon"
        , margin $ Margin 0 2 5 0
        ],
        textView $
        [
          textFromHtml $ "<u>Download Policy</u>"
        , color Color.white900
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.bold LanguageStyle
        ] <> FontStyle.body3 TypoGraphy
    ]
    
deliveryDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
deliveryDetailsView push state =    
    scrollView
        [ height $ V (screenHeight unit - 400)
        , width MATCH_PARENT
        ]
        [
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin state.deliveryDetailsConfig.margin
            , orientation VERTICAL
            ]
            [
                pickupAndDrop push state 
            ,   personName push state
            ,   PrimaryEditText.view (push <<< PersonMobile) state.deliveryDetailsConfig.mobileNumberDetails
            ,   PrimaryEditText.view (push <<< PersonAddress) state.deliveryDetailsConfig.addressDetails
            ,   PrimaryEditText.view (push <<< PersonInstruction) state.deliveryDetailsConfig.instructionDetails
            ]
        ]

pickupAndDrop :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
pickupAndDrop push state = 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][
        textView $ 
        [
        text $ if state.deliveryDetailsConfig.isSource then getString PICKUP else getString DROP
        , gravity LEFT
        , color Colors.black800
        ] <> FontStyle.body3 TypoGraphy,
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginTop 8
        , background Colors.blue600
        , cornerRadius 8.0
        , padding $ Padding 8 12 8 12
        ][
            textView $
              [ text state.deliveryDetailsConfig.locationTitle
              , maxLines 1
              , ellipsize true
              , gravity LEFT
              , color Colors.black900
              , margin $ MarginBottom 2
              ] <> FontStyle.tags TypoGraphy
            , textView $
              [ text $ state.deliveryDetailsConfig.locationDetails
              , color Colors.black700
              , maxLines 1
              , ellipsize true
              , gravity LEFT
              ] <> FontStyle.body3 TypoGraphy
        ]
    ]

personName :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
personName push state =
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 20
    ]
    [
      PrimaryEditText.view (push <<< PersonName) state.deliveryDetailsConfig.personNameDetails
    , linearLayout
        [
        height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , visibility $ boolToVisibility state.deliveryDetailsConfig.checkBoxDetails.visibility
        ]
        [
        checkBoxView push state
        ,   textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ state.deliveryDetailsConfig.checkBoxDetails.text
                , color Colors.black800
                , margin $ MarginLeft 4
                ] <> FontStyle.body3 TypoGraphy
        ]
    ]

checkBoxView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
checkBoxView push state =
    linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding (Padding 0 0 0 0)
    , margin (Margin 0 0 0 0)
    , onClick push (const CheckBoxClick)
    ][ frameLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT      
        ][ linearLayout
            [ height (V 16)
            , width (V 16)
            , stroke ("1," <> Colors.grey800)
            , cornerRadius 2.0
            ][
            imageView
                [ width (V 16)
                , height (V 16)
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_check_box"
                , visibility $ boolToVisibility state.deliveryDetailsConfig.checkBoxDetails.isSelected
                ]
            ]
        ]
    ]

parcelDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
parcelDetailsView push state = 
    linearLayout[
        height WRAP_CONTENT,
        width MATCH_PARENT,
        orientation VERTICAL,
        visibility state.parcelDetailsVisibility
    ][
        dropdownView (push <<< Dropdown1) state.parcelTypeConfig,
        linearLayout [
            width MATCH_PARENT,
            height (V 4)
        ][],
        dropdownView (push <<< Dropdown2) state.parcelQuantityConfig
    ]

dropdownView :: forall w. (DropdownAction -> Effect Unit) -> DropdownConfig -> PrestoDOM (Effect Unit) w
dropdownView push config = 
  linearLayout
  [ id $ EHC.getNewIDWithTag config.label
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 16
  ]
  [ textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text config.label
    , color Colors.black800
    , margin $ Margin 0 0 0 8
    ] <> FontStyle.body3 TypoGraphy
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , cornerRadius 8.0
    ]
    [ dropdownHeader config push
    , dropdownItems config push 
    ]
  , linearLayout [
        width MATCH_PARENT,
        height (V 4)
    ][]
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , visibility config.extraInput.visibility
    ][PrimaryEditText.view (push <<< ExtraInput) config.extraInput]
  ]

dropdownHeader :: forall w. DropdownConfig -> (DropdownAction -> Effect Unit) -> PrestoDOM (Effect Unit) w
dropdownHeader config push =
 PrestoAnim.animationSet
    ([ Anim.fadeInWithDuration 200 true ] <>
    (if os == "IOS" then []
    else [ Anim.listExpandingAnimation $ estimateExpandingAnimationConfig (0) (0.0) true ]))
  $ linearLayout
    [ width MATCH_PARENT
    , height $ V 54
    , padding $ Padding 16 0 16 0
    , orientation HORIZONTAL
    , onClick push $ const Toggle
    , background Colors.white900
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    , stroke $ if config.isOpen then ("1," <> Colors.blue800) else ("1," <> Colors.grey900)
    ]
    [ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ case config.selectedItem of
            Just item -> item.title
            Nothing -> config.placeholder
        , color $ case config.selectedItem of
            Just item -> Colors.black800
            Nothing -> Colors.grey900
        , weight 1.0
        ] <> FontStyle.subHeading1 TypoGraphy
    , imageView
        [ width $ V 24
        , height $ V 24
        , padding $ Padding 6 9 6 9
        , imageWithFallback $ if config.isOpen 
            then "ny_ic_chevron_up"
            else "ny_ic_chevron_down"
        , color Colors.black800
        ]
    ]

dropdownItems :: forall w. DropdownConfig -> (DropdownAction -> Effect Unit) -> PrestoDOM (Effect Unit) w
dropdownItems config push =
  scrollView
    [ width MATCH_PARENT
    , height $ V 250 
    , visibility $ boolToVisibility config.isOpen
    , background Colors.white900
    , cornerRadius 8.0
    , margin $ MarginTop 8
    , stroke ("1," <> Colors.blue800)
    ][
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        (config.items # mapWithIndex \index item ->
            linearLayout[
                width MATCH_PARENT,
                height WRAP_CONTENT,
                orientation VERTICAL
            ][
                linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , padding $ Padding 16 12 16 12
                , onClick push $ const $ (SelectItem item)
                , rippleColor Colors.grey900
                ]
                [ textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text item.title
                , color Colors.black900
                ] <> FontStyle.body6 TypoGraphy
                , if item.subtitle /= "" then
                    textView $
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , text item.subtitle
                    , color Colors.black700
                    , margin $ MarginTop 2
                    ] <> FontStyle.body3 TypoGraphy
                else
                    linearLayout[][]
                ],
                if index < (length config.items - 1) then  -- Only add separator if not last item
                    linearLayout
                    [ width MATCH_PARENT
                    , height (V 1)
                    , background Colors.grey900
                    , margin (Margin 16 0 16 0)
                    ][]
                else
                    linearLayout[][]
            ])
    ]



selectItemsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
selectItemsView push state = 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ state.selectableItemsConfig.visibility 
    ][
        scrollView
        [ height $ V (screenHeight unit - 400)
        , width MATCH_PARENT
        ]
        [
            SelectableItems.view (push <<< SelectableItemsController) state.selectableItemsConfig
        ]
    ]

dropdownTextFieldView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
dropdownTextFieldView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility state.dropdownTextFieldConfig.visibility
  ][
    DropdownTextField.view (push <<< DropdownTextFieldAction) state.dropdownTextFieldConfig
  ]

searchableListView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
searchableListView push state =
    linearLayout
    [
        height WRAP_CONTENT,
        width MATCH_PARENT
    ][
        SearchableListView.view (push <<< SearchableListAction) state.searchableListConfig 
    ]


customerNotPickingUpTheCall :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
customerNotPickingUpTheCall push state = 
  linearLayout
   [
      width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , gravity BOTTOM
   ]
   [
    callNotPickingView push state
   ]

callNotPickingView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
callNotPickingView push state = 
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity BOTTOM
    , cornerRadii $ Corners 24.0 true true false false
    , background Color.white900
    ]
    [ linearLayout
      [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ Margin 20 24 20 20
      ]
      [textView
        ([ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , text $ getString CUSTOMER_NOT_PICKING_UP_THE_CALL
        , color Color.black
        , margin $ MarginBottom 12
        ] <> FontStyle.h0 TypoGraphy
        ),
       imageView [
        imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_call_not_picked"
        , height $ V 250
        , width MATCH_PARENT
        , gravity $ CENTER
        , margin $ MarginBottom 12
        ],
        textView
        ([ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.lightYellow
        , padding $ Padding 12 12 12 12
        , gravity CENTER
        , text $ getString YOU_CAN_CANCEL_THE_RIDE_WITHOUT_AFFECTING_YOUR_RATING
        , cornerRadius 20.0
        , color Color.lightBrown
        ] <> FontStyle.body38 TypoGraphy
        )
      ]
    ]