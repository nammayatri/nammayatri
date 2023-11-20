{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.ChatView.View where
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), scrollBarY, alignParentBottom, background, color, textFromHtml, cornerRadius, fontStyle, gravity, height, id, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, editText, onChange, hint, scrollView, onAnimationEnd, pattern, ellipsize, clickable, singleLine, maxLines, hintColor, imageWithFallback, adjustViewWithKeyboard, accessibilityHint, accessibility, disableKeyboardAvoidance, relativeLayout)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth, os, safeMarginBottom)
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import PrestoDOM.Animation as PrestoAnim
import Prelude (Unit, bind, const, pure, unit, show, discard, ($), (&&), (-), (/), (<>), (==), (>), (*), (/=), (||), not, ($), negate, (+))
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth)
import Font.Size as FontSize
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Data.Array (mapWithIndex , (!!), length, null)
import Data.String (split, Pattern(..), length, null, null) as STR
import Data.Maybe (fromMaybe, Maybe(..), maybe)
import JBridge (renderBase64Image, scrollToEnd, addMediaFile, getSuggestionfromKey, getWidthFromPercent, getLayoutBounds)
import Components.ChatView.Controller (Action(..), Config(..), ChatComponentConfig)
import Common.Types.App
import Common.Styles.Colors as Color
import PrestoDOM.Elements.Elements (progressBar)
import PrestoDOM.Events (afterRender)
import Engineering.Helpers.Commons (screenHeight, safeMarginTop, convertUTCtoISC)
import Engineering.Helpers.Suggestions(getMessageFromKey)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Data.Function.Uncurried (runFn1)
import Mobility.Prelude (boolToVisibility)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , background config.white900
  , accessibility DISABLE
  ]
  [ chatHeaderView config push
  , chatBodyView config push
  , chatFooterView config push
  ]

chatHeaderView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatHeaderView config push =
  linearLayout
  [ orientation VERTICAL
  , height $ V 80
  , width MATCH_PARENT
  , margin $ MarginTop $ if os == "IOS" then safeMarginTop else 0
  , visibility $ boolToVisibility $ config.showHeader 
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , orientation HORIZONTAL
      , padding (PaddingHorizontal 8 16)
      , margin (Margin 0 16 0 16)
      ][ linearLayout
         [ height $ V 40
         , width $ V 40
         , gravity CENTER
         , onClick push (const BackPressed)
         ][ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
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
      , background config.grey900
      ][]
  ]

headerNameView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerNameView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width (V (((screenWidth unit)/100)* (getConfig config.userConfig.appType).margin ))
  , orientation VERTICAL
  ][textView (
    [ text config.userConfig.userName
    , color config.black800
    , ellipsize true
    , singleLine true
    , accessibilityHint $ "Driver Name : " <> config.userConfig.userName
    , accessibility ENABLE
    ] <> FontStyle.subHeading1 TypoGraphy)
   ,textView (
    [ text config.vehicleNo
    , visibility (getConfig config.userConfig.appType).customerVisibility
    , color config.black700
    , accessibilityHint $ "Vehicle Number : " <> config.vehicleNo
    , accessibility ENABLE
    , ellipsize true
    , singleLine true
    ] <> FontStyle.body3 TypoGraphy)
  ]

headerActionView ::forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerActionView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][ linearLayout
     [ height $ V 40
     , width $ V 68
     , visibility (getConfig config.userConfig.appType).customerVisibility
     , gravity CENTER
     , cornerRadius if os == "IOS" then 20.0 else 32.0
     , clickable true
     , background config.green200
     , onClick push (const Call)
     ][ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_call"
        , height $ V 18
        , accessibilityHint "Call : Button"
        , accessibility ENABLE
        , width $ V 18
        ]
     ]
  , linearLayout
    [ height $ V 40
    , width $ V 60
    , visibility (getConfig config.userConfig.appType).driverVisibility
    , gravity CENTER
    , alpha if config.enableCall then 1.0 else 0.5
    , clickable (config.enableCall)
    , background config.grey700
    , stroke $ "1,"<> config.grey900
    , cornerRadius 32.0
    , margin $ MarginRight 8
    , onClick push (const $ Call)
    ][ imageView
       [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_phone"
       , height $ V 20
       , accessibilityHint "Call : Button"
       , accessibility ENABLE
       , width $ V 20
       ]
    ]
  , linearLayout
    [ height $ V 40
    , width $ V 94
    , visibility (getConfig config.userConfig.appType).driverVisibility
    , gravity CENTER
    , orientation HORIZONTAL
    , background config.blue600
    , stroke $ "1,"<> config.blue900
    , cornerRadius 32.0
    , onClick push (const $ Navigate)
    ][ imageView
       [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_navigation_blue"
       , height $ V 20
       , width $ V 20
       , margin $ MarginRight 8
       ]
     , textView (
       [ text config.mapsText
       , color config.blue900
       ] <> FontStyle.subHeading2 TypoGraphy)
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
  [ height if config.spanParent then MATCH_PARENT else WRAP_CONTENT
  , width MATCH_PARENT
  , weight 1.0
  , orientation VERTICAL
  , margin config.chatMargin
  ] ([ scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , id (getNewIDWithTag "ChatScrollView")
      , scrollBarY false
      , disableKeyboardAvoidance true
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , adjustViewWithKeyboard "true"
        ][linearLayout
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , orientation VERTICAL
         , padding $ PaddingHorizontal 16 16
         ](mapWithIndex (\index item -> let lastConfig = (config.messages !! (index+1))
                                            isLastMessage = if (config.messagesSize /= "-1") then (show index == config.messagesSize ) else index == (length config.messages - 1)
                                        in chatComponentView config push item lastConfig isLastMessage config.userConfig.appType) config.messages)
         , if not (null config.chatSuggestionsList) && config.spanParent then suggestionsView config push else dummyTextView
        ]
      ]
    ] )

chatFooterView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatFooterView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background config.white900
  , visibility $ boolToVisibility $ config.showTextEdit 
  , margin $ MarginBottom $ if os == "IOS" then 16 else 0
  , adjustViewWithKeyboard "true"
  ][ suggestionsView config push
   , linearLayout
     [ width (V (screenWidth unit))
     , height $ V 1
     , background config.grey900
     , margin $ MarginTop 16
     ][]
    , linearLayout
      [ height $ V 48
      , width MATCH_PARENT
      , padding (PaddingHorizontal 16 8)
      , margin (Margin 16 16 16 16)
      , cornerRadius 24.0
      , gravity CENTER_VERTICAL
      , background config.grey800
      , orientation HORIZONTAL
      ][ editText $
         [ weight 1.0
         , height $ V 48
         , id (getNewIDWithTag "ChatInputEditText")
         , background config.grey800
         , cornerRadius 24.0
         , hint $ config.hint <> " " <> fromMaybe "" ((STR.split (STR.Pattern " ") config.userConfig.userName) !! 0) <> "..."
         , singleLine true
         , hintColor config.black700
         , ellipsize true
         , onChange push $ TextChanged
         , pattern "[^\n]*,255"
         ] <> FontStyle.body1 LanguageStyle
       , linearLayout
         [ height $ V 36
         , width $ V 36
         , gravity CENTER
         , onClick push (const (SendMessage))
         , accessibilityHint "Send Message : Button"
         , accessibility ENABLE
         ][ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET  $ if config.sendMessageActive then "ic_send_blue" else "ic_send"
            , height $ V 20 
            , width $ V 20 
            ] 
         ]
      ]
  ]

emptyChatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptyChatView config push =
  linearLayout
  [ height MATCH_PARENT
  , weight 1.0
  , width MATCH_PARENT
  , accessibility DISABLE
  ]
  [ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , accessibility DISABLE
     , background config.white900
     ]([ textView $
       [ text $ if config.userConfig.appType == "Customer" && null config.chatSuggestionsList && null config.messages then config.emptyChatHeader else config.suggestionHeader
       , color config.black700
       , accessibility ENABLE
       , width MATCH_PARENT
       , margin (Margin 16 16 16 20)
       , maxLines 2
       , ellipsize true
       , gravity CENTER
       , fontStyle $ FontStyle.medium LanguageStyle
       ]
     ] <> if (not $ null config.chatSuggestionsList) && config.spanParent then [suggestionsView config push] else [])
  ]

suggestionsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionsView config push =
    PrestoAnim.animationSet [ fadeInWithDelay config.suggestionDelay if config.spanParent then true else false ]
    $ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity RIGHT
    , margin if config.spanParent then (Margin 0 21 16 20) else (Margin 0 (if os == "IOS" then 8 else 0) 16 0)
    , onAnimationEnd push (const EnableSuggestions)
    , alpha if config.spanParent then 0.0 else 1.0
    , visibility $ boolToVisibility $ not ((null config.chatSuggestionsList) && not config.canSendSuggestion || (not config.spanParent && not config.enableSuggestions))
    ][ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , stroke ("1,"<> config.grey900)
      , cornerRadius 8.0
      , orientation VERTICAL
      ] (mapWithIndex (\index item -> quickMessageView config item (index == (length config.chatSuggestionsList)-1) push) (config.chatSuggestionsList))
    ]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ]

quickMessageView :: forall w. Config -> String -> Boolean -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
quickMessageView config message isLastItem push =
  let value = getMessageFromKey message config.languageKey
  in
  linearLayout
  [ height WRAP_CONTENT
  , width (V (((screenWidth unit)/100)* 80))
  , gravity LEFT
  , visibility $ boolToVisibility $ not $ STR.null message
  , orientation VERTICAL
  , onClick push (if config.enableSuggestionClick then const (SendSuggestion message) else (const NoAction))
  ][ textView $
     [ text $ value
     , color config.blue800
     , padding (Padding 12 16 12 16)
     ] <> FontStyle.body1 TypoGraphy
   , linearLayout
     [ width MATCH_PARENT
     , height $ V 1
     , visibility if (isLastItem) then GONE else VISIBLE
     , background config.grey900
     ][]
  ]
chatComponentView :: forall w. Config -> (Action -> Effect Unit) -> ChatComponentConfig -> Maybe ChatComponentConfig -> Boolean -> String -> PrestoDOM (Effect Unit) w
chatComponentView state push config nextConfig isLastItem userType =
  let chatConfig = getChatConfig state config.sentBy isLastItem (hasTimeStamp config nextConfig)
      value = getMessageFromKey config.message state.languageKey
  in 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility $ boolToVisibility $ not $ config.type == "Text" && (STR.null config.message)
  ][
  PrestoAnim.animationSet
    [ if state.userConfig.appType == config.sentBy then
        if (state.spanParent) then
          translateInXForwardFadeAnimWithDelay config.delay true
        else
          (translateInXForwardAnim $ if isLastItem then true else false)
      else
        if (state.spanParent) then
          translateInXBackwardFadeAnimWithDelay config.delay true
        else
          (translateInXBackwardAnim $ if isLastItem then true else false)
    ]
  $ linearLayout
  [height WRAP_CONTENT
  , width MATCH_PARENT
  , alpha if state.spanParent then 0.0 else 1.0
  , margin chatConfig.margin
  , gravity chatConfig.gravity
  , orientation VERTICAL
  , visibility $ boolToVisibility $ not $ (not state.spanParent && STR.null value)
  , onAnimationEnd (\action ->
      if isLastItem || state.spanParent then do
        _ <- scrollToEnd (getNewIDWithTag "ChatScrollView") true
        pure unit
      else
        pure unit) (const NoAction)
  ][ linearLayout
     [ padding (Padding 12 12 12 12)
     , height WRAP_CONTENT
     , width $ if (os == "IOS" && (STR.length config.message) > (if state.languageKey == "HI_IN" then 50 else 30) && config.type /= "Image" && config.type /= "Audio") then MATCH_PARENT else WRAP_CONTENT
     , background chatConfig.background
     , cornerRadii chatConfig.cornerRadii
     , gravity chatConfig.gravity
     ][ case config.type of
        "Image" -> PrestoAnim.animationSet[translateInXForwardAnim true] $ linearLayout
                 [ cornerRadius 12.0
                 , background Color.white900
                 , width $ V 180
                 , id $ getNewIDWithTag config.message
                 , onAnimationEnd (\action -> do
                                renderBase64Image config.message (getNewIDWithTag config.message) true "FIT_CENTER"
                 ) (const NoAction)
                 , onClick push (const $ OnImageClick config.message)
                 , height $ V 180
                 , gravity CENTER
                 ][ progressBar
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    ]
                  ]
        "Audio" -> PrestoAnim.animationSet[translateInXForwardAnim true] $ linearLayout
                   [ width MATCH_PARENT
                   , height MATCH_PARENT
                   , orientation HORIZONTAL
                   , onAnimationEnd (\action -> do
                                    _ <- addMediaFile (getNewIDWithTag "ChatAudioPlayer") config.message (getNewIDWithTag "ChatAudioPlayerLoader") "ny_ic_play_no_background" "ny_ic_pause_no_background" (getNewIDWithTag "ChatAudioPlayerTimer")
                                    pure unit
                                  ) (const NoAction)
                   ][ imageView
                    [ width $ V 48
                    , height $ V 48
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_add_audio"
                    , margin (MarginRight 8)
                    ]
                    , linearLayout
                    [ width $ V 32
                    , height $ V 32
                    , id (getNewIDWithTag "ChatAudioPlayerLoader")
                    , margin (MarginRight 8)
                    ] []
                    , linearLayout
                    [ orientation VERTICAL
                    ][ linearLayout
                     [ id (getNewIDWithTag "ChatAudioPlayer")
                     , height $ V 32
                     , width $ V (getWidthFromPercent 40)
                     ] []
                     , linearLayout
                     [ id (getNewIDWithTag "ChatAudioPlayerTimer")
                     , color Color.white900
                     , width WRAP_CONTENT
                     , height $ V 16
                     ] []
                    ]
                   ]
        _ -> textView $ 
          [ textFromHtml $ if state.spanParent then config.message else value
          , singleLine false
          , margin $ MarginTop $ if state.languageKey == "KN_IN" then 6 else 0
          , color chatConfig.textColor
          ] <> FontStyle.body1 TypoGraphy
      ] 
      , textView $ 
       [ text $ convertUTCtoISC config.timeStamp "hh:mm A"
       , visibility $ boolToVisibility $ hasTimeStamp config nextConfig 
       , color state.black800
       , fontStyle $ FontStyle.regular LanguageStyle
       ] <> FontStyle.captions TypoGraphy
  ]
      , linearLayout
      [
        width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER
      , visibility $ boolToVisibility $  hasDateFooter config nextConfig
      ][
        linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.grey900
          , gravity CENTER
          , weight 1.0
          ][]
      , textView
       [ text $ maybe "" (\nextConfig' -> convertUTCtoISC nextConfig'.timeStamp "ddd, DD/MM/YYYY") nextConfig
       , textSize FontSize.a_10
       , color state.black800
       , fontStyle $ FontStyle.regular LanguageStyle
       , gravity CENTER
       ]
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        , gravity CENTER
        , weight 1.0
        ][]
      ]
  ]

getConfig :: String -> {margin :: Int, customerVisibility :: Visibility, driverVisibility :: Visibility}
getConfig appType =
  if appType == "Customer" then
    {
      margin : 60,
      customerVisibility : VISIBLE,
      driverVisibility : GONE
    }
  else
    {
      margin : 30,
      customerVisibility : GONE,
      driverVisibility : VISIBLE
    }

getChatConfig :: Config -> String -> Boolean -> Boolean -> {margin :: Margin, gravity :: Gravity, background :: String, cornerRadii :: Corners, textColor :: String}
getChatConfig state sentBy isLastItem hasTimeStamp =
  if state.userConfig.appType == sentBy then
    {
      margin : Margin ((screenWidth unit)/4) 24 0 if state.spanParent then 0 else if(os == "IOS" && isLastItem && hasTimeStamp) then 12 else 0,
      gravity : RIGHT,
      background : state.blue800,
      cornerRadii : (Corners 16.0 true true false true),
      textColor :  state.white900
    }
  else
    { margin : Margin 0 24 ((screenWidth unit)/4) if state.spanParent then 0 else if(os == "IOS" && isLastItem && hasTimeStamp) then 12 else 0,
      gravity :  LEFT,
      background : state.grey900,
      cornerRadii : (Corners 16.0 true true true false ),
      textColor :   state.black800
    }

hasTimeStamp :: ChatComponentConfig -> Maybe ChatComponentConfig -> Boolean
hasTimeStamp msgConfig nxtMsgConfig = 
    maybe (not STR.null msgConfig.timeStamp) (\nxtMsg -> checkNeedTime nxtMsg) nxtMsgConfig
    where 
      checkNeedTime nxtMsg = not (STR.null msgConfig.timeStamp) &&
                             not (STR.null nxtMsg.timeStamp) &&
                             convertUTCtoISC msgConfig.timeStamp "hh:mm A" == convertUTCtoISC nxtMsg.timeStamp "hh:mm A" &&
                             msgConfig.sentBy == nxtMsg.sentBy

hasDateFooter :: ChatComponentConfig -> Maybe ChatComponentConfig -> Boolean
hasDateFooter msgConfig nxtMsgConfig = 
  maybe false (\nxtMsg -> checkNeedTime nxtMsg) nxtMsgConfig
    where 
      checkNeedTime nxtMsg = not (STR.null msgConfig.timeStamp) &&
                             not (STR.null nxtMsg.timeStamp) &&
                             (convertUTCtoISC msgConfig.timeStamp "DD:MM:YYYY") /= (convertUTCtoISC nxtMsg.timeStamp "DD:MM:YYYY") &&
                             msgConfig.sentBy == nxtMsg.sentBy
