module Components.MessagingView.Common.View where

import ConfigProvider
import Constants.Configs
import Data.Maybe
import Mobility.Prelude
import Prelude
import PrestoDOM.Core
import Services.API
import Services.Backend
import SuggestionUtils
import Timers
import Common.Types.App (ChatComponent)
import Accessor (_lat, _lon, _selectedQuotes, _fareProductType)
import Animation (fadeIn, fadeOut, translateYAnimFromTop, scaleAnim, translateYAnimFromTopWithAlpha, translateInXAnim, translateOutXAnim, translateInXForwardAnim, translateOutXBackwardAnimY, translateInXSidebarAnim, screenAnimation, fadeInWithDuration, fadeOutWithDuration, scaleYAnimWithDelay, shimmerAnimation)
import Animation as Anim
import Animation.Config (AnimConfig, animConfig)
import Animation.Config (Direction(..), translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, messageInAnimConfig, messageOutAnimConfig)
import Common.Resources.Constants (zoomLevel)
import Common.Types.App (LazyCheck(..), YoutubeData, CarouselData)
import Components.Banner.Controller as BannerConfig
import Components.Banner.View as Banner
import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide as ChooseYourRide
import Components.CommonComponentConfig as CommonComponentConfig
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.ErrorModal as ErrorModal
import Components.FavouriteLocationModel as FavouriteLocationModel
import Components.LocationListItem.View as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.MenuButton as MenuButton
import Components.MessagingView as MessagingView
import Components.PopUpModal as PopUpModal
import Components.PricingTutorialModel as PricingTutorialModel
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListModel.View as QuoteListModel
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideCompletedCard as RideCompletedCard
import Components.SaveFavouriteCard as SaveFavouriteCard
import Components.SearchLocationModel as SearchLocationModel
import Components.SelectListModal as CancelRidePopUp
import Components.SettingSideBar as SettingSideBar
import Components.SourceToDestination as SourceToDestination
import Constants (defaultDensity)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, take, (!!), head, filter, cons, null, tail)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1)
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Int (ceil, floor, fromNumber, fromString, toNumber)
import Data.Lens ((^.))
import Data.Map as Map
import Data.Number as NUM
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, liftFlow, os, safeMarginBottom, safeMarginTop, screenHeight, isPreviousVersion, screenWidth, camelCaseToSentenceCase, truncate, getExpiryTime, getDeviceHeight, getScreenPpi)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import Engineering.Helpers.Utils (showAndHideLoader)
import Engineering.Helpers.Utils (showAndHideLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer, didDriverMessage, getPixels, getDefaultPixels, getDeviceDefaultDensity)
import JBridge (addMarker, animateCamera, clearChatMessages, drawRoute, enableMyLocation, firebaseLogEvent, generateSessionId, getArray, getCurrentPosition, getExtendedPath, getHeightFromPercent, getLayoutBounds, initialWebViewSetUp, isCoordOnPath, isInternetAvailable, isMockLocation, lottieAnimationConfig, removeAllPolylines, removeMarker, requestKeyboardShow, scrollOnResume, showMap, startChatListenerService, startLottieProcess, stopChatListenerService, storeCallBackMessageUpdated, storeCallBackOpenChatScreen, storeKeyBoardCallback, toast, updateRoute, addCarousel, updateRouteConfig, addCarouselWithVideoExists, storeCallBackLocateOnMap, storeOnResumeCallback, setMapPadding)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.Types (MarginConfig, ShadowConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, bind, const, discard, map, negate, not, pure, show, unit, void, when, ($), (&&), (*), (+), (-), (/), (/=), (<), (<<<), (<=), (<>), (==), (>), (||), (<$>), identity)
import Presto.Core.Types.API (ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (Accessiblity(..), BottomSheetState(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Shadow(..), Visibility(..), accessibility, accessibilityFocusable, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, autoCorrectionType, background, clickable, clipChildren, clipToPadding, color, cornerRadius, disableClickFeedback, ellipsize, enableShift, focusable, fontStyle, frameLayout, gradient, gravity, halfExpandedRatio, height, horizontalScrollView, id, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onAnimationEnd, onBackPressed, onClick, onSlide, onStateChanged, orientation, padding, peakHeight, relativeLayout, rippleColor, rotation, scaleType, scrollBarX, scrollView, shadow, shimmerFrameLayout, singleLine, stroke, text, textFromHtml, textSize, textView, url, visibility, webView, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Properties (cornerRadii, sheetState, alpha, nestedScrollView)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (CallType(..), HomeScreenState, LocationListItemState, PopupType(..), SearchLocationModelType(..), SearchResultType(..), Stage(..), ZoneType(..), SheetState(..), Trip(..), SuggestionsMap(..), Suggestions(..), City(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalStore, updateLocalStage, getValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Locale.Utils
import Components.MessagingView.Common.Types



messageNotificationView :: forall w action. (action -> Effect Unit) -> (MessageNotificationView action) -> PrestoDOM ( Effect Unit) w
messageNotificationView push state =
  let enableChatWidget = not (os == "ANDROID" || state.enableChatWidget)
  in
  (if state.showChatNotification then 
    if os == "IOS" then PrestoAnim.animationSet[] else PrestoAnim.animationSet [fadeInWithDuration 1000 true]
  else if state.isNotificationExpanded then 
    if os == "IOS" then PrestoAnim.animationSet[] else PrestoAnim.animationSet [fadeOutWithDuration 1000 true]
  else  PrestoAnim.animationSet []) $ 
  linearLayout
  [ height $ if enableChatWidget then V 1 else V 130
  , width $ if enableChatWidget then V 1 else MATCH_PARENT
  , margin $ MarginBottom if enableChatWidget then 0 else 8
  , padding $ Padding 12 12 12 12
  , background Color.black900
  , orientation VERTICAL
  , clickable true
  , accessibility $ if state.isNotificationExpanded && os /= "IOS" then ENABLE else if not state.isNotificationExpanded then DISABLE_DESCENDANT else DISABLE
  , accessibilityHint $ "Quick Chat : Widget"
  , onAnimationEnd push $ const state.messageViewAnimationEnd
  , visibility $ if ((not state.rideStarted) && state.currentSearchResultType /= QUOTES && state.config.feature.enableChat) && state.config.feature.enableSuggestions 
                  then VISIBLE 
                  else if state.rideStarted && os == "IOS" 
                    then INVISIBLE 
                    else GONE
  , cornerRadius 20.0
  ][linearLayout 
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , clickable true
    , accessibility DISABLE
    ][ messagePromtView push state
     , chatNotificationMessageView push state
     , linearLayout
       [ height $ WRAP_CONTENT
       , width $ MATCH_PARENT
       , gravity RIGHT
       , clickable true
       , accessibility DISABLE
       ][ linearLayout
         [ height $ WRAP_CONTENT
         , width $ WRAP_CONTENT
         , cornerRadius 20.0
         , clickable true
         , accessibility ENABLE
         , accessibilityHint $ "Close : Button : Select to close chat widget"
         , background Color.manatee200
         , padding $ Padding 10 10 10 10
         , onClick push $ const state.removeNotificationAction
         ][imageView
           [ height $ V 16
           , width $ V 16
           , accessibility DISABLE
           , imageWithFallback $ fetchImage FF_ASSET "ny_ic_cross_white"
           ]
         ]
       ]  
    ]
    , separatorView state
    , if (state.lastMessage.sentBy == "Driver" || not (didDriverMessage FunctionCall)) then quickRepliesView push state else dummyView state
    , if ((not $ DS.null state.lastSentMessage.sentBy) && (not $ DS.null state.lastReceivedMessage.sentBy)) then messageView push state state.lastSentMessage else dummyView state
  ]

messagePromtView :: forall w action . (action -> Effect Unit) -> (MessageNotificationView action) -> PrestoDOM ( Effect Unit) w
messagePromtView push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ V (screenWidth unit - 100)
  , orientation VERTICAL
  , clickable true
  , accessibility ENABLE
  , accessibilityHint $ "Want to message your driver?"
  , onClick push $ const $ state.messageReceiverAction
  , visibility $ boolToVisibility $ null state.messages 
  ][ textView $ 
      [ text $ getString MESSAGE_YOUR_DRIVER
      , accessibility ENABLE
      , color Color.white900
      , accessibility DISABLE
      , ellipsize true
      , singleLine true
      ] <> FontStyle.body6 TypoGraphy
   , textView $ 
      [ text $ getString CHECK_IN_WITH_YOUR_DRIVER 
      , color Color.white900
      , accessibility DISABLE
      , ellipsize true
      , singleLine true
      ] <> FontStyle.captions TypoGraphy
  ]

chatNotificationMessageView :: forall w action. (action -> Effect Unit) -> MessageNotificationView action -> PrestoDOM ( Effect Unit) w
chatNotificationMessageView push state = if ((not $ didDriverMessage FunctionCall) && (not $ DS.null state.lastSentMessage.sentBy)) then messageView push state state.lastSentMessage
                                            else if (not $ DS.null state.lastReceivedMessage.sentBy) then messageView push state state.lastReceivedMessage
                                            else dummyView state


messageView :: forall w action. (action -> Effect Unit) -> MessageNotificationView action  -> ChatComponent -> PrestoDOM ( Effect Unit) w
messageView push state message=
  let value = getMessageFromKey message.message $ getLanguageLocale languageKey
  in
  linearLayout
  [ width $ V (screenWidth unit - 140)
  , height $ WRAP_CONTENT
  , clickable true
  , onClick push $ const $ state.messageReceiverAction
  , accessibility ENABLE
  , accessibilityHint $ (if message.sentBy == "Customer" then "You Sent : " else "Message From Driver : ") <> getMessageFromKey message.message "EN_US"
  ][ imageView
     [ height $ V 32
     , width $ V 32
     , imageWithFallback $ if message.sentBy == "Driver" then fetchImage FF_ASSET "ny_ic_driver_message" else fetchImage FF_ASSET "ny_ic_customer_message"
     , margin $ Margin 0 8 8 0
     , accessibility DISABLE
     ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , accessibility DISABLE_DESCENDANT
      , gravity LEFT
      ][ textView $ 
        [ width $ WRAP_CONTENT
        , height $ WRAP_CONTENT
        , text $ (if message.sentBy == "Driver" then getString MESSAGE_FROM_DRIVER else getString YOU_SENT) <> ":"
        , color Color.black700
        , maxLines 1
        , ellipsize true
        , margin $ if os == "IOS" then MarginBottom 2 else MarginBottom 0
        ] <> FontStyle.captions TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadii $ Corners 12.0 true true true false
        , background if message.sentBy == "Driver" then Color.manatee200 else Color.blue200
        , margin $ MarginTop 4
        , padding $ Padding 8 4 8 4
        , visibility $ boolToVisibility $ not $ DS.null value
        , gravity CENTER
        ][ textView $ 
          [ height WRAP_CONTENT
          , text $ value
          , color Color.white900
          , maxLines 1
          , ellipsize true
          ] <> FontStyle.body9 TypoGraphy
        ]
      ]
    ]


dummyView :: forall w action. MessageNotificationView action -> PrestoDOM ( Effect Unit) w
dummyView state = 
  linearLayout
  [height $ V 0
  , width $ V 0
  ][]

separatorView :: forall w action. MessageNotificationView action -> PrestoDOM ( Effect Unit) w
separatorView state = 
  linearLayout
  [ height WRAP_CONTENT
  , width  MATCH_PARENT
  , margin $ MarginVertical 8 8 
  ](map (\_ -> linearLayout
  [ height $ V 1
  , width $ V 8
  , margin $ MarginRight 4
  , background Color.manatee200
  ][]) (getArray 100))


quickRepliesView :: forall w action. (action -> Effect Unit) -> MessageNotificationView action -> PrestoDOM ( Effect Unit) w
quickRepliesView push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , accessibility DISABLE
  ][ textView $
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , text $ getString QUICK <> " " <> getString CHATS <> ":"
     , color Color.black700
     , accessibility ENABLE
     , accessibilityHint $ "Quick Chats"
     , margin $ MarginBottom 4
     ] <> FontStyle.captions TypoGraphy
   , relativeLayout
     [ height $ WRAP_CONTENT
     , width $ MATCH_PARENT
     , clickable true
     , accessibility DISABLE
     ][horizontalScrollView
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , scrollBarX false
      , accessibility DISABLE
      , clickable true
      ][linearLayout
        [ height $ WRAP_CONTENT
        , width $ MATCH_PARENT
        , clickable true
        , accessibility DISABLE
        ][linearLayout
          [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , cornerRadius 13.0
          , background Color.white900
          , padding $ Padding 16 6 16 6
          , margin $ MarginRight 12
          , clickable true
          , accessibility ENABLE
          , accessibilityHint $ "Custom Message : Button : Select to input custom message"
          , onClick push $ const $ state.messageReceiverAction
          ][ imageView
            [ height $ V 16
            , width $ V 16
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_message_black"
            ]
          ]
        , linearLayout
          [ height $ WRAP_CONTENT
          , width $ MATCH_PARENT
          , clickable true
          , id $ getNewIDWithTag "QuickReplyItems"
          , accessibility DISABLE
          ](mapWithIndex (\index item -> 
            quickReplyItem push state item index
          ) (state.chatSuggestions))
        ]
      ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , gravity RIGHT
      , accessibility DISABLE
      ][linearLayout
        [ height $ V $ replyItemHeight FunctionCall
        , width $ V 40
        , accessibility DISABLE
        , gradient $ if os == "IOS" then (Linear 180.0 ["#2C2F3A","#282C2F3A",Color.transparent]) else (Linear 90.0 [Color.transparent, "#282C2F3A", "#2C2F3A"])
        ][]  
      ]
    ]
  ]
  where replyItemHeight :: LazyCheck -> Int
        replyItemHeight dummy = do
          let layout = runFn1 getLayoutBounds $ getNewIDWithTag "QuickReplyItems"
          if layout.height == 0 then 32 else layout.height + 2

quickReplyItem :: forall w action. (action -> Effect Unit) -> MessageNotificationView action -> String -> Int -> PrestoDOM ( Effect Unit) w
quickReplyItem push state item idx = 
  let message = getMessageFromKey item $ getLanguageLocale languageKey
      isLastItem = (idx == (length $ state.chatSuggestions) - 1)
  in
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ WRAP_CONTENT
  , cornerRadius 13.0
  , clickable true
  , rippleColor Color.rippleShade
  , accessibility ENABLE
  , margin $ MarginRight if isLastItem then 18 else 12
  , visibility $ boolToVisibility $ not $ DS.null message
  , accessibilityHint $ (getMessageFromKey item $ "EN_US") <> ": Button : Select to send message to driver"
  , onClick (\action -> do
                when (not $ DS.null state.lastReceivedMessage.sentBy) $ do void $ startTimer 3 ("ChatNotificationRemoval" <> (show $ state.timerCounter)) "3" push state.messageExpiryAction
                push action)
            (const $ state.sendQuickMessageAction item)
  , background Color.white900
  , padding $ Padding 16 6 16 6
  ][ textView $
      [ text $ message
      , color Color.black900
      ] <> FontStyle.tags TypoGraphy
  ]
