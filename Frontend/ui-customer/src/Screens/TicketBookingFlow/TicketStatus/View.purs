module Screens.TicketBookingFlow.TicketStatus.View where

import Common.Types.App
import Screens.TicketBookingFlow.TicketStatus.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Array (length, uncons, cons, take, drop, find, elem, mapWithIndex, filter, null)
import Data.Foldable (or)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String as DS
import Data.String.Common (joinWith)
import Effect (Effect)
import Engineering.Helpers.Commons (getCurrentUTC, screenWidth, flowRunner)
import Data.Foldable (foldl, foldMap)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (incrOrDecrTimeFrom, getCurrentDatev2, getMinutesBetweenTwoUTChhmmss, fetchImage, FetchImageFrom(..), decodeError, convertUTCToISTAnd12HourFormat, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer)
import JBridge as JB
import Prelude (not, Unit, discard, void, bind, const, pure, unit, ($), (&&), (/=), (&&), (<<<), (+), (<>), (==), map, show, (||), show, (-), (>), (>>=), mod, negate, (<=), (>=), (<))
import PrestoDOM (FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), shimmerFrameLayout, afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, clickable, id, imageUrl, maxLines, ellipsize, lineHeight, fillViewport)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingFlow.TicketStatus.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingFlow.TicketStatus.ComponentConfig 
import Resources.Constants -- TODO:: Replace these constants with API response
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag, convertUTCTimeToISTTimeinHHMMSS)
import Services.API (BookingStatus(..), TicketPlaceResponse(..), TicketPlaceResp(..), TicketServiceResp(..), PlaceType(..), BusinessHoursResp(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), TicketServicesResponse(..), SpecialDayType(..))
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array (catMaybes, head, (..), any)
import Data.Maybe (fromMaybe, isJust, Maybe(..), maybe)
import Debug
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Data.Either (Either(..))
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Effect.Class (liftEffect)
import Types.App (GlobalState, defaultGlobalState)
import Data.Time.Duration (Milliseconds(..))
import Services.API as API
import Storage (KeyStore(..), setValueToLocalStore, getValueToLocalStore)
import Effect.Uncurried  (runEffectFn1)
import PaymentPage (consumeBP)
import Engineering.Helpers.Commons as EHC
import Data.Ord (comparing)
import Data.Function.Uncurried (runFn3)
import Mobility.Prelude (groupAdjacent)
import Language.Strings (getString)
import Language.Types (STR(..))

screen :: ST.TicketStatusScreenState -> Screen Action ST.TicketStatusScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TicketBookingScreen"
  -- , globalEvents : [if initialState.props.actionType == ST.ZooTicketToPaymentStatusEntry then getPlaceDataEvent else  pure (pure unit)]
  , globalEvents : [getPlaceDataEvent]
  , eval :
    \action state -> do
        let _ = spy "TicketStatusScreen action " action
        let _ = spy "TicketStatusScreen state " state
        eval action state
  }
  where
  getPlaceDataEvent push = do
    void $ runEffectFn1 consumeBP unit
    if initialState.props.actionType == ST.ZooTicketToPaymentStatusEntry then
      void $ launchAff $ flowRunner defaultGlobalState $ paymentStatusPooling initialState.data.shortOrderId  5 3000.0 initialState push PaymentStatusAction
    else
      pure unit
    pure $ pure unit
--------------------------------------------------------------------------------------------

paymentStatusPooling :: forall action. String -> Int -> Number -> ST.TicketStatusScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
paymentStatusPooling shortOrderId count delayDuration state push action = 
  if (getValueToLocalStore PAYMENT_STATUS_POOLING) == "true" && state.props.currentStage == ST.BookingConfirmationStage  && count > 0 && shortOrderId /= "" then do
    ticketStatus <- Remote.getTicketStatus shortOrderId
    _ <- pure $ spy "ticketStatus" ticketStatus
    case ticketStatus of
      Right (API.GetTicketStatusResp resp) -> do
        if (DA.any (_ == resp) ["Booked", "Failed"]) then do
            _ <- pure $ setValueToLocalStore PAYMENT_STATUS_POOLING "false"
            doAff do liftEffect $ push $ action resp
        else do
            void $ delay $ Milliseconds delayDuration
            paymentStatusPooling shortOrderId (count - 1) delayDuration state push action
      Left _ -> pure unit
    else pure unit
    

view :: forall w . (Action -> Effect Unit) -> ST.TicketStatusScreenState -> PrestoDOM (Effect Unit) w
view push state =
    PrestoAnim.animationSet [Anim.fadeIn true]  $ relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , onBackPressed push $ const BackPressed
    ]
    [ shimmerView state
    , linearLayout 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , visibility if (state.props.currentStage == ST.DescriptionStage && state.props.showShimmer) then GONE else VISIBLE
        , margin $ MarginBottom if state.props.currentStage == ST.BookingConfirmationStage then 0 else 84
        ]
        [ linearLayout
          [ height $ V 1 
          , width MATCH_PARENT
          , background Color.grey900
          ] []
        , separatorView Color.greySmoke
        , scrollView
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , background Color.white900
            , afterRender push $ const AfterRender
            , fillViewport true
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , orientation VERTICAL
                ]
                [ bookingStatusView state push state.props.paymentStatus ]
            ]
        ]
    , linearLayout [ visibility GONE ] []
    , bookingConfirmationActions state push state.props.paymentStatus
    ]

shimmerView :: forall w . ST.TicketStatusScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , visibility if state.props.showShimmer then VISIBLE else GONE
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height (V 235)
        , margin (Margin 16 15 16 0)
        , background Color.greyDark
        , cornerRadius 16.0
        ] []
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin (MarginTop 258)
        ] (DA.mapWithIndex 
            (\index item ->
                linearLayout
                  [ width MATCH_PARENT
                  , height (V 60)
                  , margin (Margin 16 16 16 0)
                  , cornerRadius 12.0
                  , background Color.greyDark
                  ][]
            ) (1 .. 7)
          )
    ]

separatorView :: forall w. String -> PrestoDOM (Effect Unit) w
separatorView color =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background color
  ][]



bookingStatusView :: forall w. ST.TicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingStatusView state push paymentStatus = 
  let refundInfoView = if state.props.currentStage == ST.BookingConfirmationStage && state.props.actionType == ST.MetroTicketToPaymentStatusEntry && state.props.paymentStatus == Common.Pending then
                         refundInfoTextView 
                       else
                         linearLayout [visibility GONE] []
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ PaddingTop 20
    , background "#E2EAFF"
    , gravity CENTER
    , orientation VERTICAL
    ][ 
      paymentStatusHeader state push paymentStatus
    , bookingStatusBody state push paymentStatus
    , refundInfoView
    ]

copyTransactionIdView :: forall w. ST.TicketStatusScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
copyTransactionIdView state push visibility' = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER
  , visibility if visibility' then VISIBLE else GONE
  , onClick push $ const $ Copy state.data.shortOrderId
  ][  commonTV push "TransactionID" Color.black700 (FontStyle.body3 TypoGraphy) 0 CENTER NoAction
    , textView $ 
      [ text state.data.shortOrderId
      , margin $ MarginLeft 3
      , color Color.black700
      , padding $ PaddingBottom 1
      ] <> FontStyle.h3 TypoGraphy
  , imageView
     [ width $ V 16
     , height $ V 16
     , margin $ MarginLeft 3
     , imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy"
     ] 
  ]

bookingStatusBody :: forall w. ST.TicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus ->  PrestoDOM (Effect Unit) w
bookingStatusBody state push paymentStatus = 
  let 
    headerImgConfig = case state.props.actionType of
                        ST.MetroTicketToPaymentStatusEntry -> {
                          src : fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro"
                        , width : V 41
                        , height : V 41
                        }
                        ST.ZooTicketToPaymentStatusEntry -> {
                          src : fetchImage FF_ASSET "ny_ic_ticket_black"
                        , width : V 24
                        , height : V 24
                        }
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , weight 1.0
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , visibility if paymentStatus == Common.Failed then GONE else VISIBLE
    ][ scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ][ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity CENTER
            , orientation VERTICAL
            , padding $ Padding 10 10 10 10
            , cornerRadius 8.0
            , background Color.white900
            ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER_VERTICAL
                ][ imageView
                    [ width headerImgConfig.width
                    , height headerImgConfig.height
                    , imageWithFallback headerImgConfig.src
                    , margin $ MarginRight 4
                    ]
                  , commonTV push state.data.ticketName Color.black900 (FontStyle.subHeading1 TypoGraphy) 0 LEFT NoAction
                ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ] $ DA.mapWithIndex ( \index item ->  keyValueView push state item.key item.val index) state.data.keyValArray
          ]
        ]
      ]

bookingConfirmationActions :: forall w. ST.TicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingConfirmationActions state push paymentStatus = 
  linearLayout
  [ width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  , padding $ PaddingBottom 20
  , alignParentBottom "true,-1"
  , background Color.white900
  , visibility if (state.props.currentStage == ST.BookingConfirmationStage) then VISIBLE else GONE
  ][ linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.grey900
      ][]
   , PrimaryButton.view (push <<< ViewTicketAC) (viewTicketButtonConfig primaryButtonText $ paymentStatus /= Common.Pending)
   , linearLayout
     [ width $ MATCH_PARENT
     , height WRAP_CONTENT
     , onClick push $ const GoHome
     , gravity CENTER
     ][commonTV push secondaryButtonText Color.black900 (FontStyle.subHeading1 TypoGraphy) 5 CENTER GoHome]
  ]
  where primaryButtonText = case paymentStatus of
                              Common.Success -> "View Ticket"
                              Common.Failed -> "Try Again"
                              _ -> ""
        secondaryButtonText = case paymentStatus of
                              Common.Success -> "Go Home"
                              _ -> "Go Back"

paymentStatusHeader :: forall w. ST.TicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
paymentStatusHeader state push paymentStatus = 
  let transcationConfig = getTransactionConfig paymentStatus
      paymentSt = spy "paymentStatus" paymentStatus
  in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ][ relativeLayout
      [ width $ MATCH_PARENT
      , height $ WRAP_CONTENT
      , gravity CENTER
      ][imageView
        [ width $ MATCH_PARENT
        , height $ V 100
        , visibility if paymentStatus == Common.Success then VISIBLE else GONE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_confetti"
        ] 
      , linearLayout
        [ width $ MATCH_PARENT
        , height $ WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginTop 50
        ][ imageView
          [ width $ V 65
          , height $ V 65
          , imageWithFallback transcationConfig.image
          ]
        ]
      ]
      , commonTV push transcationConfig.title Color.black900 (FontStyle.h2 TypoGraphy) 14 CENTER NoAction
      , commonTV push transcationConfig.statusTimeDesc Color.black700 (FontStyle.body3 TypoGraphy) 5 CENTER NoAction
      , copyTransactionIdView state push $ paymentStatus == Common.Failed
      , if (paymentStatus == Common.Success) then (linearLayout [][]) else (PrimaryButton.view (push <<< RefreshStatusAC) (refreshStatusButtonConfig state))

    ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> Action -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' action =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , onClick push $ const action
  , margin $ MarginTop marginTop
  ] <> fontStyle

keyValueView :: (Action -> Effect Unit) -> ST.TicketStatusScreenState -> String -> String -> Int -> forall w . PrestoDOM (Effect Unit) w
keyValueView push state key value index = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , margin $ Margin 5 12 5 12
      , height $ V 1
      , background Color.grey700
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginHorizontal 5 5
      ][ textView $ 
        [ text key
        , margin $ MarginRight 8
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , gravity RIGHT
        ][ if key == "Booking For" then 
             bookingForView state 
           else 
            textView $ 
              [ text value
              , color Color.black800
              , onClick push $ const $ 
                  if key == "Booking ID" || key == "Transaction ID" then
                    Copy value 
                  else
                    NoAction -- needs refactoring
              ] <> FontStyle.body6 TypoGraphy
          ]
      ]
  ]

bookingForView :: forall w. ST.TicketStatusScreenState -> PrestoDOM (Effect Unit) w
bookingForView state = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ](map ( \item -> 
      textView $
      [ text item
      , padding $ Padding 6 4 6 4
      , cornerRadius 20.0
      , margin $ MarginLeft 5
      , background Color.blue600
      ] <> FontStyle.tags TypoGraphy
    ) state.data.bookedForArray)

getTransactionConfig :: Common.PaymentStatus -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig status = 
  case status of
    Common.Success -> {image : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", statusTimeDesc : "Your ticket has been generated below", title : "Your booking is Confirmed!"}
    Common.Pending -> {image : fetchImage FF_COMMON_ASSET "ny_ic_transaction_pending", statusTimeDesc : "Please check back in a few minutes.", title : "Your booking is Pending!"}
    Common.Failed  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_payment_failed", statusTimeDesc : "Please retry booking.", title : "Booking Failed!"}
    Common.Scheduled  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_pending", statusTimeDesc : "", title : ""}


refundInfoTextView :: forall w. PrestoDOM (Effect Unit) w
refundInfoTextView = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 12 16 0
  ][
    linearLayout[
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    ][
      imageView [
        width $ V 16
      , height $ V 16
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
      ]
    ]
  , textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text "Incase of failure, any money debited will be refunded within 5 - 7 working days."
    , color Color.black700
    ] <> FontStyle.body3 TypoGraphy
  ]