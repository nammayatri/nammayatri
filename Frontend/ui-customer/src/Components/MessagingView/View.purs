{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.MessagingView.View where

import Common.Types.App
import Animation (translateInXBackwardAnim, translateInXForwardAnim, translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig)
import Components.MessagingView.Controller (Action(..), Config(..), ChatComponent)
import Data.Array (mapWithIndex, (!!), length, null)
import Data.Function.Uncurried (runFn1)
import Data.Int as Int
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Number (pow)
import Data.Number as Num
import Data.String as STR
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginBottom, getNewIDWithTag, screenWidth, screenHeight, os, safeMarginTop, isPreviousVersion)
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (scrollToEnd, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, show, ($), (&&), (-), (/), (<>), (==), (>), (*), (/=), (||), not, negate, (+), (<=), discard, void, (>=))
import PrestoDOM (Accessiblity(..), BottomSheetState(..), Gravity(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, background, bottomShift, clickable, color, cornerRadius, editText, ellipsize, fontStyle, gravity, halfExpandedRatio, height, hint, hintColor, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, maxLines, onAnimationEnd, onChange, onClick, onStateChanged, orientation, padding, pattern, peakHeight, relativeLayout, scrollBarX, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, topShift, visibility, weight, width, nestedScrollView, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems, updateLayout)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Events (afterRender)
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, sheetState)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Storage (KeyStore(..), getValueToLocalStore)
import Debug

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
 PrestoAnim.animationSet [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 300 ] $ 
  relativeLayout
  [ height $ MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , clickable if os == "IOS" then true else false
  , accessibility DISABLE
  ][ linearLayout
     [ height $ WRAP_CONTENT
     , width $ MATCH_PARENT
     , clickable if os == "IOS" then true else false
     , alignParentBottom "true,-1"
     , adjustViewWithKeyboard "true"
     , margin $ MarginTop $ if os == "IOS" then safeMarginTop else 0
     ][ linearLayout
        [ height $ V $ config.peekHeight + 125
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.grey700
        , clickable true
        , adjustViewWithKeyboard "false"
        , cornerRadii $ Corners 24.0 true true false false
        , stroke $ config.config.driverInfoConfig.cardStroke
        ][ chatHeaderView config push
         , chatBodyView config push 
         ]
      ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      , adjustViewWithKeyboard "true"
      ][ chatFooterView config push ]
   ]

chatHeaderView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatHeaderView config push =
  linearLayout
  [ orientation VERTICAL
  , height $ WRAP_CONTENT
  , width MATCH_PARENT
  , clickable true
  , id $ getNewIDWithTag "MessagingHeaderView"
  ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , margin $ MarginTop 12
    , padding $ PaddingHorizontal 8 16
    ][ linearLayout
        [ height $ V 40
        , width $ V 40
        , gravity CENTER
        , onClick push $ const BackPressed
        ][ imageView
          [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
          , height $ V 24
          , accessibilityHint "Back : Button"
          , accessibility ENABLE
          , width $ V 24
          ]
        ]
      , headerNameView config push
      , headerActionView config push
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , margin $ MarginTop 12
      , background Color.grey900
      ][]
  ]

headerNameView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerNameView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width (V (((screenWidth unit)/10)* 6 ))
  , orientation VERTICAL
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    ][ textView $
      [ text $ if (getValueToLocalStore LANGUAGE_KEY) == "HI_IN" then config.userConfig.userName <> " " else getString CHAT_WITH <> " "
      , color Color.black800
      , ellipsize true
      , singleLine true
      , margin $ MarginBottom 8
      ] <> if (getValueToLocalStore LANGUAGE_KEY) == "HI_IN" then FontStyle.body15 TypoGraphy else FontStyle.tags TypoGraphy
    , textView $
      [ text $ if (getValueToLocalStore LANGUAGE_KEY) == "HI_IN" then getString CHAT_WITH else config.userConfig.userName
      , color Color.black800
      , ellipsize true
      , singleLine true
      , accessibilityHint $ "Driver Name : " <> config.userConfig.userName
      , accessibility ENABLE
      , margin $ MarginBottom 8
      ] <> if (getValueToLocalStore LANGUAGE_KEY) == "HI_IN" then FontStyle.tags TypoGraphy else FontStyle.body15 TypoGraphy
    ]
   , horizontalScrollView
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , scrollBarX false
     , updateLayout true
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , cornerRadius 4.0
            , margin $ MarginRight 6
            , stroke $ "1,"<> Color.black900
            , background Color.yellow900
            , padding $ Padding 4 2 4 2
            ][ textView $ 
              [ text config.vehicleNo
              , color Color.black800
              , accessibilityHint $ "Vehicle Number : " <> config.vehicleNo
              , accessibility ENABLE
              ] <> FontStyle.tags TypoGraphy
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , background Color.white900
            , cornerRadius 4.0
            , margin $ MarginRight 6
            , padding $ Padding 4 2 4 2
            ][ textView $
              [ text $ "OTP "
              , color Color.black700
              , accessibility DISABLE
              ] <> FontStyle.tags TypoGraphy
            , textView $
              [ text $ config.otp
              , color Color.black700
              , accessibility DISABLE
              ] <> FontStyle.body9 TypoGraphy
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , background Color.white900
            , cornerRadius 4.0
            , margin $ MarginRight 6
            , padding $ Padding 4 2 4 2
            ][ textView $
              [ text $ "Fare: " <> config.config.currency <> config.fareAmount
              , color Color.black700
              , accessibility DISABLE
              ] <> FontStyle.tags TypoGraphy
           ]
        ]
     ]
  ]

headerActionView ::forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerActionView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][ linearLayout
     [ height $ V 40
     , width $ V 40
     , gravity CENTER
     , cornerRadius if os == "IOS" then 20.0 else 32.0
     , clickable true
     , background $ Color.green200
     , onClick push $ const $ Call
     ][ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_call"
        , height $ V 16
        , width $ V 16
        , accessibilityHint "Call : Button"
        , accessibility ENABLE
        ]
     ]
  ]

chatBodyView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatBodyView config push =
  if (length config.messages) == 0 && not config.showAutoGeneratedText then
    emptyChatView config push
  else
    chatView config push

chatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatView config push =
  linearLayout
  [ weight 1.0
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginBottom $ getChatFooterHeight config
  ][scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , id $ getNewIDWithTag "ChatScrollView"
    , scrollBarY false
    , updateLayout true
    ]
    [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ PaddingHorizontal 16 16
          , visibility if config.showAutoGeneratedText then VISIBLE else GONE
          ][ chatComponent config push {message : config.autoGeneratedText, sentBy : "Driver", timeStamp : config.rideConfirmedAt , type : "" , delay : 0} false "DRIVER" 1 ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding (PaddingHorizontal 16 16)
          ](mapWithIndex (\index item -> chatComponent config push item (index == (length config.messages - 1)) (config.userConfig.appType) index) (config.messages))
      ]
    ]
  ]

chatFooterView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatFooterView config push =
  linearLayout
  [ height $ WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , gravity CENTER_VERTICAL
  , alignParentBottom "true,-1"
  , id $ getNewIDWithTag "MessagingFooterView"
  , afterRender push $ const $ NoAction
  , padding $ Padding 16 16 16 16
  ][ suggestionsView config push
   , linearLayout
      [ height $ V 48
      , width MATCH_PARENT
      , padding (PaddingHorizontal 16 8)
      , cornerRadius 24.0
      , gravity CENTER_VERTICAL
      , background Color.blue600
      , orientation HORIZONTAL
      , afterRender push $ const $ NoAction
      ][ editText $
         [ weight 1.0
         , height $ V 48
         , id (getNewIDWithTag "ChatInputEditText")
         , background Color.blue600
         , cornerRadius 24.0
         , hint $ config.hint <> " " <> fromMaybe "" ((STR.split (STR.Pattern " ") config.userConfig.userName) !! 0) <> "..."
         , singleLine true
         , hintColor Color.black700
         , ellipsize true
         , onChange push $ TextChanged
         , pattern "[^\n]*,255"
         ] <> FontStyle.body1 LanguageStyle
       , linearLayout
         [ height $ V 36
         , width $ V 36
         , gravity CENTER
         , onClick push $ const $ SendMessage
         , accessibilityHint "Send Message : Button"
         , accessibility ENABLE
         ][ imageView
            [ imageWithFallback $ if config.sendMessageActive then fetchImage FF_COMMON_ASSET "ic_send_blue" else fetchImage FF_COMMON_ASSET "ic_send"
            , height $ V 20 
            , width $ V 20 
            ] 
         ]
      ]
  ]

emptyChatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptyChatView config push =
  linearLayout
  [ height $ V 150
  , background Color.transparent
  , width MATCH_PARENT
  , accessibility DISABLE
  ][]

suggestionsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionsView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , afterRender push $ const $ NoAction
  , visibility if (length config.suggestionsList == 0 ) || not config.canSendSuggestion then GONE else VISIBLE
  , background Color.white900
  , margin $ MarginBottom 16
  ][ textView $
     [ text $ getString QUICK <> "\n" <> getString REPLIES
     , color Color.black700
     , margin $ MarginRight 12
     ] <> FontStyle.captions TypoGraphy
   , horizontalScrollView
     [ height WRAP_CONTENT
     , weight 1.0
     , scrollBarX false
     , updateLayout true
     ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity LEFT
        ] (mapWithIndex (\index item -> quickMessageView config item index push)(config.suggestionsList))
      ]
  ]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ]

quickMessageView :: forall w. Config -> String -> Int -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
quickMessageView config message idx push =
  linearLayout
  [ height WRAP_CONTENT
  , width $ WRAP_CONTENT
  , onClick push $ const $ SendSuggestion message
  , background Color.blue600
  , cornerRadius 12.0
  , margin $ MarginLeft if idx == 0 then 0 else 8
  , padding $ Padding 16 6 16 6
  ][ textView $
     [ text $ getMessageFromKey message config.languageKey
     , color Color.black900
     ] <> FontStyle.tags TypoGraphy
  ]
chatComponent :: forall w. Config -> (Action -> Effect Unit) -> ChatComponent -> Boolean -> String -> Int -> PrestoDOM (Effect Unit) w
chatComponent state push config isLastItem userType index =
  PrestoAnim.animationSet
    [ if state.userConfig.appType == config.sentBy then
         translateInXForwardAnim $ if isLastItem then true else false
      else
          translateInXBackwardAnim $ if isLastItem then true else false
    ]
  $ linearLayout
  [height WRAP_CONTENT
  , width MATCH_PARENT
  , alpha 1.0
  , margin (getChatConfig state config.sentBy isLastItem index).margin
  , gravity (getChatConfig state config.sentBy isLastItem index).gravity
  , orientation VERTICAL
  , onAnimationEnd (\action ->
      if isLastItem then do
        _ <- scrollToEnd (getNewIDWithTag "ChatScrollView") true
        pure unit
      else
        pure unit) (const NoAction)
  ][ ((if (os == "IOS" || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion ""))) then linearLayout else flexBoxLayout) $ 
     [ height MATCH_PARENT
     , width $ WRAP_CONTENT
     , background (getChatConfig state config.sentBy isLastItem index).background
     , cornerRadii (getChatConfig state config.sentBy isLastItem index).cornerRadii
     ] <> if (os == "IOS" || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion ""))) then [margin (MarginBottom 4), padding (Padding 12 12 12 12)] else [justifyContent JUSTIFY_END, flexDirection ROW, flexWrap WRAP, alignItems ALIGN_BASELINE, orientation VERTICAL, padding (Padding 10 6 10 6)])
     [ textView
        [ text $ getMessageFromKey config.message state.languageKey
        , textSize FontSize.a_14
        , singleLine false
        , lineHeight "18"
        , margin $ MarginTop $ if state.languageKey == "KN_IN" then 6 else 0
        , color (getChatConfig state config.sentBy isLastItem index).textColor
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
      , textView
        [ text config.timeStamp
        , textSize FontSize.a_10
        , height $ MATCH_PARENT
        , visibility if (os == "IOS" || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion ""))) then GONE else VISIBLE
        , color (getChatConfig state config.sentBy isLastItem index).timeStampColor
        , margin $ MarginLeft 6
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
      ]
    , textView
      [ text config.timeStamp
      , textSize FontSize.a_10
      , height $ MATCH_PARENT
      , visibility if (os == "IOS" || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion ""))) then VISIBLE else GONE
      , color Color.black800
      , fontStyle $ FontStyle.regular LanguageStyle
    ]
  ]

getChatConfig :: Config -> String -> Boolean -> Int -> {margin :: Margin, gravity :: Gravity, background :: String, cornerRadii :: Corners, textColor :: String, timeStampColor :: String}
getChatConfig state sentBy isLastItem index =
  if state.userConfig.appType == sentBy then
    {
      margin : (Margin ((screenWidth unit)/5) 8 0 if isLastItem then 8 else 0),
      gravity : RIGHT,
      background : Color.blue800,
      cornerRadii : (Corners 12.0 true true false true),
      textColor :  Color.white900,
      timeStampColor : Color.white900
    }
  else
    { margin : (Margin 0 8 ((screenWidth unit)/5) if isLastItem then 8 else 0),
      gravity :  LEFT,
      background : Color.white900,
      cornerRadii : (Corners 12.0 true true true false ),
      textColor :   Color.black800,
      timeStampColor : Color.black800
    }

getPreviousVersion :: String -> String 
getPreviousVersion _ = 
  if os == "IOS" then 
    case getMerchant FunctionCall of 
      NAMMAYATRI -> "1.3.6"
      YATRISATHI -> "1.0.5"
      YATRI -> "2.1.0"
      _ -> "0.0.0"
    else do 
      case getMerchant FunctionCall of 
        NAMMAYATRI -> "1.3.9"
        YATRISATHI -> "0.1.5"
        YATRI -> "2.2.1"
        _ -> "0.0.0"

getChatFooterHeight :: Config -> Int
getChatFooterHeight config = 
  let height = (runFn1 getLayoutBounds $ getNewIDWithTag "MessagingFooterView").height
  in if height == 0 || height == 80 then  (if null config.suggestionsList || not config.canSendSuggestion then 80 else 125) else height