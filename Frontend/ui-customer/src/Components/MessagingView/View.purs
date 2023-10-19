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
import Debug

import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXForwardAnim, translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig)
import Components.MessagingView.Controller (Action(..), Config(..), ChatComponent)
import Data.Array (mapWithIndex, (!!), length, null)
import Data.Function.Uncurried (runFn1)
import Data.Int as Int
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Number (pow)
import Data.Number as Num
import Data.String (split, Pattern(..), length) as STR
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth, os)
import Engineering.Helpers.Commons (screenHeight, safeMarginTop)
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getCommonAssetStoreLink)
import JBridge (getHeightFromPercent, getLayoutBounds, getSuggestionfromKey, scrollToEnd)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, show, ($), (&&), (-), (/), (<>), (==), (>), (*), (/=), (||), not, negate, (+), (<=))
import PrestoDOM (Accessiblity(..), BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, ellipsize, fontStyle, gravity, height, hint, hintColor, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, maxLines, onAnimationEnd, onChange, onClick, onStateChanged, orientation, padding, pattern, peakHeight, relativeLayout, scrollBarX, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, topShift, visibility, weight, width, onSlide)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Events (afterRender)
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth, sheetState)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (SheetState(..))
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  PrestoAnim.animationSet [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 300 ] $ 
  relativeLayout
  [ height $ MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , accessibility DISABLE
  ][ linearLayout
     [ height $ WRAP_CONTENT
     , width $ MATCH_PARENT
     , alignParentBottom "true,-1"
     , adjustViewWithKeyboard "true"
     ][ coordinatorLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][ bottomSheetLayout
           [ height $ WRAP_CONTENT
           , width MATCH_PARENT
           , accessibility DISABLE
           , sheetState $ if config.sheetState == STATE_EXPANDED then EXPANDED else COLLAPSED
           , cornerRadii $ Corners 24.0 true true false false
           , onStateChanged push $ ScrollStateChanged
           , onSlide (\action -> do
                        case action of
                          OnSlide val -> do 
                            let position = truncate 1 val
                            if config.chatSheetSlide /= position then push $ OnSlide position else pure unit
                          _ -> pure unit
                        ) (OnSlide)
           , peakHeight $ config.peekHeight
           , visibility VISIBLE
           , background Color.grey700
           , topShift 0.0
           ][linearLayout
             [ height $ V $ config.peekHeight + if (config.chatSheetSlide <= 0.9 && not config.isExpanding) || config.isKeyboardOpen then 0 else 200
             , width MATCH_PARENT
             , orientation VERTICAL
             , cornerRadii $ Corners 24.0 true true false false
             , stroke $ config.config.driverInfoConfig.cardStroke
             ][ chatHeaderView config push
             ]
          ]
        ]
     ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      ][ chatBodyView config push 
       , chatFooterView config push
       ]
   ]

chatHeaderView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatHeaderView config push =
  linearLayout
  [ orientation VERTICAL
  , height $ WRAP_CONTENT
  , width MATCH_PARENT
  , id $ getNewIDWithTag "MessagingHeaderView"
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ][ linearLayout
        [ height $ V 4
        , width $ V 34
        , cornerRadius 4.0
        , margin $ MarginVertical 8 8
        , background "#888888"
        ][]
      ]
   , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , padding (PaddingHorizontal 8 16)
    ][ linearLayout
        [ height $ V 40
        , width $ V 40
        , gravity CENTER
        , onClick push (const BackPressed)
        ][ imageView
          [ imageWithFallback $ "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
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
  , width (V (((screenWidth unit)/100)* 60 ))
  , orientation VERTICAL
  ][textView $
    [ text $ "Chat with " <> config.userConfig.userName
    , color Color.black800
    , ellipsize true
    , singleLine true
    , accessibilityHint $ "Driver Name : " <> config.userConfig.userName
    , accessibility ENABLE
    , margin $ MarginBottom 8
    ] <> FontStyle.body6 TypoGraphy
   , horizontalScrollView
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , scrollBarX false
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
              ] <> FontStyle.body17 TypoGraphy
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , background Color.white900
            , cornerRadius 4.0
            , gravity CENTER_VERTICAL
            , margin $ MarginRight 6
            , padding $ Padding 4 2 4 2
            ][ textView $
              [ text $ if config.driverRating == "0.0" then (getString NEW_) else config.driverRating
              , color Color.black700
              , margin $ MarginRight 4
              , accessibility DISABLE
              ] <> FontStyle.body16 TypoGraphy
             , imageView
              [ imageWithFallback $ "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_active.png"
              , height $ V 9
              , width $ V 9
              , gravity CENTER_VERTICAL
              , accessibility DISABLE
              ]
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
              ] <> FontStyle.body16 TypoGraphy
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
     , background "#2053BB6F"
     , onClick push $ const $ Call
     ][ imageView
        [ imageWithFallback $ "ny_ic_call," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_call.png"
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
  [ height $ V $ getChatHeight config -- config.peekHeight + 200 - if null config.suggestionsList || not config.canSendSuggestion then 80 else 125   |    --if config.bottomSheetState == "4" then V (getHeightFromPercent 43 - ((spy "PRAVEEN" (runFn1 getLayoutBounds $ getNewIDWithTag "MessagingHeaderView").height) + if null config.suggestionsList || not config.canSendSuggestion then 80 else 125)) else if config.bottomSheetState == "3" then V (getHeightFromPercent 75 - ((runFn1 getLayoutBounds $ getNewIDWithTag "MessagingHeaderView").height + if null config.suggestionsList || not config.canSendSuggestion then 80 else 125)) else V (getHeightFromPercent 43 - ((spy "PRAVEEN" (runFn1 getLayoutBounds $ getNewIDWithTag "MessagingHeaderView").height) + if null config.suggestionsList || not config.canSendSuggestion then 80 else 125))
  , width MATCH_PARENT
  , adjustViewWithKeyboard "true"
  , orientation VERTICAL
  ] ([ scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , id (getNewIDWithTag "ChatScrollView")
      , adjustViewWithKeyboard "true"
      , scrollBarY false
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
    ] )

chatFooterView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatFooterView config push =
  linearLayout
  [ height $ WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , gravity CENTER_VERTICAL
  , adjustViewWithKeyboard "true"
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
            [ imageWithFallback $ if config.sendMessageActive then "ic_send_blue," <> (getCommonAssetStoreLink FunctionCall) <> "ic_send_blue.png" else "ic_send," <> (getCommonAssetStoreLink FunctionCall) <> "ic_send.png"
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
     [ text $ "Quick\nReplies"
     , color Color.black700
     , margin $ MarginRight 12
     ] <> FontStyle.captions TypoGraphy
   , horizontalScrollView
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , scrollBarX false
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
  , alpha if state.spanParent then 0.0 else 1.0
  , margin (getChatConfig state config.sentBy isLastItem index).margin
  , gravity (getChatConfig state config.sentBy isLastItem index).gravity
  , orientation VERTICAL
  , onAnimationEnd (\action ->
      if isLastItem || state.spanParent then do
        _ <- scrollToEnd (getNewIDWithTag "ChatScrollView") true
        pure unit
      else
        pure unit) (const NoAction)
  ][ linearLayout
     [ padding (Padding 12 8 12 8)
     , margin (MarginBottom 4)
     , height WRAP_CONTENT
     , width $ if (os == "IOS" && (STR.length config.message) > (if state.languageKey == "HI_IN" then 50 else 30) ) then MATCH_PARENT else WRAP_CONTENT
     , background (getChatConfig state config.sentBy isLastItem index).background
     , cornerRadii (getChatConfig state config.sentBy isLastItem index).cornerRadii
     , gravity (getChatConfig state config.sentBy isLastItem index).gravity
     ][ textView
        [ text if state.spanParent then config.message else getMessageFromKey config.message state.languageKey
        , textSize FontSize.a_14
        , singleLine false
        , lineHeight "18"
        , margin $ MarginTop $ if state.languageKey == "KN_IN" then 6 else 0
        , color (getChatConfig state config.sentBy isLastItem index).textColor
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
      ]
     , textView
       [ text config.timeStamp
       , textSize FontSize.a_10
       , visibility if STR.length config.timeStamp > 0 then VISIBLE else GONE
       , color Color.black800
       , fontStyle $ FontStyle.regular LanguageStyle
       ]
  ]

getChatConfig :: Config -> String -> Boolean -> Int -> {margin :: Margin, gravity :: Gravity, background :: String, cornerRadii :: Corners, textColor :: String}
getChatConfig state sentBy isLastItem index =
  if state.userConfig.appType == sentBy then
    {
      margin : (Margin ((screenWidth unit)/4) (if index == 0 then 0 else 16) 0 if isLastItem then 12 else 0),
      gravity : RIGHT,
      background : Color.blue800,
      cornerRadii : (Corners 12.0 true true false true),
      textColor :  Color.white900
    }
  else
    { margin : (Margin 0 (if index == 0 then 0 else 16) ((screenWidth unit)/4) if isLastItem then 12 else 0),
      gravity :  LEFT,
      background : Color.white900,
      cornerRadii : (Corners 12.0 true true true false ),
      textColor :   Color.black800
    }

truncate :: Int -> Number -> Number
truncate precision num = (Num.round (num * (10.0 `pow` (Int.toNumber  precision)))) / (10.0 `pow` (Int.toNumber precision))

getChatHeight :: Config -> Int
getChatHeight config = (config.peekHeight - 88 - if null config.suggestionsList || not config.canSendSuggestion then 80 else 125) + if (config.chatSheetState == STATE_EXPANDED && not config.isKeyboardOpen) then 200 else  (if (config.chatSheetSlide <= 0.9 && not config.isExpanding) || config.isKeyboardOpen then 0 else fromMaybe 0 (Int.fromNumber(config.chatSheetSlide * (if (config.chatSheetSlide <= 0.9 && not config.isExpanding && config.chatSheetState == STATE_COLLAPSED) || config.isKeyboardOpen then 0.0 else 200.0))))
