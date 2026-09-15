module Screens.TicketBookingFlow.TicketList.View where

import Common.Types.App
import Screens.TicketBookingFlow.TicketList.ComponentConfig

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
import Screens.TicketBookingFlow.TicketList.Controller (Action(..), ScreenOutput, eval, getLimitOfDaysAccToPlaceType)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingFlow.TicketList.ComponentConfig 
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
import Presto.Core.Types.Language.Flow (doAff, Flow)
import Helpers.Pooling(delay)
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
import Helpers.CommonView as CommonView

screen :: ST.TicketBookingScreenState -> Screen Action ST.TicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TicketBookingScreen"
  , globalEvents : [getPlaceDataEvent]
  , eval :
    \action state -> do
        let _ = spy "ZooTicketBookingFlow TicketList action " action
        let _ = spy "ZooTicketBookingFlow TicketList state " state
        eval action state
  }
  where
  getPlaceDataEvent push = do
    void $ runEffectFn1 consumeBP unit
    void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ getPlaceDataEvent' push
    void $ launchAff $ flowRunner defaultGlobalState $ paymentStatusPooling initialState.data.shortOrderId  5 3000.0 initialState push PaymentStatusAction
    pure $ pure unit

  getPlaceDataEvent' push = do
    if (any (_ == initialState.props.currentStage) [ST.DescriptionStage , ST.ViewTicketStage]) then do
      case initialState.data.placeInfo of
        Just (TicketPlaceResp place) -> do
          servicesResp <- Remote.getTicketPlaceServicesBT place.id initialState.data.dateOfVisit
          lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData (Just $ TicketPlaceResp place) (Just servicesResp)
        Nothing -> lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData Nothing Nothing
    else pure unit
--------------------------------------------------------------------------------------------

paymentStatusPooling :: forall action. String -> Int -> Number -> ST.TicketBookingScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
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
    

view :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [Anim.fadeIn true]  $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , onBackPressed push $ const BackPressed
  , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
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
    [ headerView state push
    , scrollView
      [ height WRAP_CONTENT
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
          (mainView state push)
      ]
    ]
  , actionsView state push
  ]
  where
  actionsView state push =
    case state.props.currentStage of
      ST.BookingConfirmationStage -> linearLayout [ visibility GONE ] []
      ST.TicketInfoStage -> linearLayout [ visibility GONE ] []
      ST.DescriptionStage -> generalActionButtons state push
      _ -> generalActionButtons state push

  headerView state push =
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , CommonView.horizontalSeparatorView Color.grey900
    ]

  mainView state push =
    [ ticketsListView state push ]
  
noDataView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> String -> PrestoDOM (Effect Unit) w
noDataView state push msg =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , margin $ MarginHorizontal 16 16
    ]
    [ textView $
      [ text msg
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy 
    ]

shimmerView :: forall w . ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
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

generalActionButtons :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
generalActionButtons state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginBottom 16
        , orientation VERTICAL
        , background Color.white900
        ]
        $ [ linearLayout  
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ MarginBottom 16
            , background Color.grey900
            ][]
        ] <>
          if state.props.currentStage == ST.ChooseTicketStage then 
            [PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig1 state) ]
            else  [PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state) ]
      ]

termsAndConditionsView :: forall w . Array String -> Boolean -> PrestoDOM (Effect Unit) w
termsAndConditionsView termsAndConditions isMarginTop =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ if isMarginTop then MarginTop 10 else MarginTop 0
  ] (mapWithIndex (\index item ->
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][ textView $
         [ textFromHtml $ " •  " <> item
         , color Color.black700
         ] <> FontStyle.tags TypoGraphy
      ]
  ) termsAndConditions )

ticketsListView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketsListView state push = 
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  , padding $ PaddingHorizontal 16 16
  ][ if DA.null state.props.ticketBookingList.booked then linearLayout[height $ V 0][] else ticketsCardListView state push state.props.ticketBookingList.booked "Booked Tickets"
  ,  if DA.null state.props.ticketBookingList.pendingBooking then linearLayout[height $ V 0][] else ticketsCardListView state push state.props.ticketBookingList.pendingBooking "Pending Payment"
  ,  if DA.null state.props.ticketBookingList.cancelled then linearLayout[height $ V 0][] else ticketsCardListView state push state.props.ticketBookingList.cancelled "Cancelled Tickets"
  , emptyTicketsView state push
  ]

emptyTicketsView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptyTicketsView state push = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , background Color.blue600
  , orientation VERTICAL
  , visibility if DA.null state.props.ticketBookingList.booked && DA.null state.props.ticketBookingList.pendingBooking then VISIBLE else GONE
  , padding $ Padding 16 16 16 16
  , gravity CENTER
  , cornerRadius 8.0
  ][linearLayout
    [ height $ WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginBottom 12
    ][imageView
      [ height $ V 96
      , width $ V 96
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_deer"
      ]
    ]
  , textView $ 
    [ text $ "You can book tickets to the Alipore Zoo by clicking the button."
    , color Color.black900
    , gravity CENTER
    ] <> FontStyle.paragraphText LanguageStyle
  ]

ticketsCardListView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Array ST.TicketBookingItem -> String -> PrestoDOM (Effect Unit) w
ticketsCardListView state push list title =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ textView $
     [ text title
     , width $ WRAP_CONTENT
     , height $ WRAP_CONTENT
     , margin $ MarginBottom 12
     , color Color.black900
     ] <> FontStyle.subHeading1 TypoGraphy
  ,  linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , margin $ MarginBottom 12
     ] (map (\item -> ticketInfoCardView state push item) list)
  ]

ticketInfoCardView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> ST.TicketBookingItem -> PrestoDOM (Effect Unit) w
ticketInfoCardView state push booking = 
  let property = getTicketStatusBackgroundColor booking.status
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , padding $ Padding 16 16 16 16
  , orientation VERTICAL
  , cornerRadius 8.0
  , stroke $ "1," <>  Color.grey700
  , margin $ MarginBottom 12
  , onClick push $ const $ GetBookingInfo booking.shortId booking.status
  , clickable true
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][  textView $ 
          [ text booking.ticketPlaceName
          , height WRAP_CONTENT
          , weight 1.0
          , maxLines 2
          , ellipsize true
          , lineHeight "20"
          , margin $ MarginBottom 8
          , color Color.black900
          ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , height $ V 20
          , background $ property.bgColor
          , padding $ PaddingRight 10
          , cornerRadius 11.0
          , margin $ MarginLeft 5
          , gravity CENTER
          ][  imageView
              [ imageWithFallback $ fetchImage FF_ASSET $ getTicketStatusImage booking.status
              , width $ V $ if booking.status == Booked then 13 else 20
              , height $ V $ if booking.status == Booked then 8 else 20
              , margin $ if booking.status == Booked then (Margin 5 4 5 4) else (Margin 0 0 0 0)
              ]
            , textView $
              [ text $ property.statusText
              , color Color.white900
              ] <> FontStyle.tags LanguageStyle
          ]
      ]
    , textView $
      [ text (convertUTCtoISC booking.visitDate "Do MMM, YYYY")
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 12
      , textSize $ FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ][  textView 
          [ text "View"
          , color Color.blue900
          , margin $ MarginRight 8
          , padding $ Padding 4 8 4 8
          , textSize $ FontSize.a_14
          , fontStyle $ FontStyle.medium LanguageStyle
          ]
        , imageView
          [ width $ V 10
          , height $ V 8
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_blue_arrow"
          ]
      ]
  ]

separatorView :: forall w. String -> PrestoDOM (Effect Unit) w
separatorView color =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background color
  ][]

getTicketStatusImage :: BookingStatus -> String
getTicketStatusImage status = fetchImage FF_COMMON_ASSET $ case status of 
  Pending -> "ny_ic_transaction_pending"
  Booked -> "ny_ic_white_tick"
  Failed -> "ny_ic_payment_failed"
  Cancelled -> "ny_ic_cancelled"

getTicketStatusBackgroundColor :: BookingStatus -> {bgColor :: String, statusText :: String }
getTicketStatusBackgroundColor status = case status of 
  Pending -> { bgColor : Color.yellow900, statusText : "Pending" }
  Booked ->  { bgColor : Color.green900, statusText : "Booked" }
  Failed ->  { bgColor : Color.red900, statusText : "Cancelled" }
  Cancelled ->  { bgColor : Color.red900, statusText : "Cancelled" }

getShareButtonIcon :: String -> String
getShareButtonIcon ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> "ny_ic_share_unfilled_white"
  _ -> "ny_ic_share_unfilled_black"

getShareButtonColor :: String -> String
getShareButtonColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.white900
  _ -> Color.black900

getPlaceColor :: String -> String
getPlaceColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.white900
  "Videography Fee" -> Color.black800
  "Aquarium Fee" -> Color.black800
  _ -> Color.black800

getInfoColor :: String -> String
getInfoColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.white900
  "Videography Fee" -> Color.black900
  "Aquarium Fee" -> Color.black900
  _ -> Color.black800

getPillInfoColor :: String -> String
getPillInfoColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.grey900
  "Videography Fee" -> Color.black800
  "Aquarium Fee" ->  Color.white900
  _ -> Color.white900

copyTransactionIdView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
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
