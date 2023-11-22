module Screens.TicketBookingScreen.View where

import Common.Types.App
import Screens.TicketBookingScreen.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Foldable (or)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, flowRunner)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, convertUTCToISTAnd12HourFormat, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getPreviousVersion, getSearchType, parseFloat, storeCallBackCustomer)
import JBridge as JB
import Prelude (Unit, discard, void, bind, const, pure, unit, ($), (&&), (/=), (<<<), (<>), (==), map, show, (||), show, (-), (>), (>>=))
import PrestoDOM (FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), shimmerFrameLayout, afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, clickable, id)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingScreen.ComponentConfig 
import Resources.Constants -- TODO:: Replace these constants with API response
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag)
import Services.API (BookingStatus(..), TicketPlaceResponse(..), TicketPlaceResp(..))
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array (catMaybes, head, (..), any)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
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
import Engineering.Helpers.MobilityPrelude(isStrEmpty)

screen :: ST.TicketBookingScreenState -> Screen Action ST.TicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TicketBookingScreen"
  , globalEvents : [getPlaceDataEvent]
  , eval :
    \action state -> do
        let _ = spy "ZooTicketBookingFlow action " action
        let _ = spy "ZooTicketBookingFlow state " state
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
      (TicketPlaceResponse placesResp) <- Remote.getTicketPlacesBT ""
      let mFirstPlace = head placesResp -- TODO:: Remove this once Screen for choosing place is ready
      case mFirstPlace of
        Just (TicketPlaceResp firstPlace) -> do
          servicesResp <- Remote.getTicketPlaceServicesBT firstPlace.id
          lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData (Just $ TicketPlaceResp firstPlace) (Just servicesResp)
        Nothing -> lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData Nothing Nothing
    else pure unit

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
    ]
    [ shimmerView state
    , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , visibility if (state.props.currentStage == ST.DescriptionStage && state.props.showShimmer) then GONE else VISIBLE
        , margin $ MarginBottom if state.props.currentStage == ST.BookingConfirmationStage then 0 else 84
        ]
        [ headerView state push
        , linearLayout
          [ height $ V 1 
          , width MATCH_PARENT
          , background Color.grey900
          ] []
        , separatorView Color.greySmoke
        , scrollView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.white900
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , orientation VERTICAL
                , padding $ PaddingBottom 20
                ]
                (mainView state push)
            ]
        ]
    , actionsView state push
    , bookingConfirmationActions state push state.props.paymentStatus
    ]
  where
  actionsView state push =
    case state.props.currentStage of
      ST.BookingConfirmationStage -> linearLayout [ visibility GONE ] []
      ST.TicketInfoStage -> linearLayout [ visibility GONE ] []
      ST.DescriptionStage -> generalActionButtons state push
      _ -> generalActionButtons state push

  headerView state push =
    case state.props.currentStage of
      ST.BookingConfirmationStage -> linearLayout [ visibility GONE ][]
      _ -> GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)

  mainView state push =
    if (state.props.currentStage == ST.DescriptionStage) 
      then
        case state.data.placeInfo of
          Just placeInfo -> descriptionStateMainView state push placeInfo
          Nothing -> [ noDataView state push "No ticketing zones in this area" ]
    else if (state.props.currentStage == ST.ChooseTicketStage) then [ chooseTicketsView state push ]
    else if (state.props.currentStage == ST.BookingConfirmationStage) then [ bookingStatusView state push state.props.paymentStatus ]
    else if (state.props.currentStage == ST.ViewTicketStage) then [ ticketsListView state push ]
    else if (state.props.currentStage == ST.TicketInfoStage) then [ individualBookingInfoView state push ]
    else []

  descriptionStateMainView state push placeInfo =
      [ imageView
          [ height $ V 370
          , width MATCH_PARENT
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_animal_1" 
          , margin $ MarginBottom 15
          ]
      , descriptionView state push placeInfo
      ]
  
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

descriptionView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> TicketPlaceResp -> PrestoDOM (Effect Unit) w
descriptionView state push (TicketPlaceResp placeInfo) = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16 
  ][  textView $ 
      [ text placeInfo.name
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy 
    , textView $ 
      [ text (fromMaybe placeInfo.name placeInfo.description)
      , color Color.black800 
      ] <> FontStyle.body1 TypoGraphy 
    , locationView state push 
    , feeBreakUpView state push state.data.servicesInfo
  ]

locationView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locationView state push =
  linearLayout
  [ height WRAP_CONTENT 
  , width MATCH_PARENT
  , margin $ MarginTop 24
  , orientation VERTICAL
  ][  textView $ 
      [ text "Location"
      , color Color.black800
      , margin $ MarginBottom 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , imageView
      [ height $ V 200
      , width MATCH_PARENT
      , cornerRadius 8.0 
      , layoutGravity "center_horizontal"
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_zoo_alipore_map" 
      ]
  ]

feeBreakUpView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Array ST.TicketServiceData -> PrestoDOM (Effect Unit) w
feeBreakUpView state push services = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600
  , orientation VERTICAL 
  , padding $ Padding 20 20 20 20
  , margin $ MarginTop 24
  ][ textView $
      [ text "Fee & Timings"
      , color Color.black800
      ] <> FontStyle.subHeading1 TypoGraphy
    ,textView $
    [ text "(Zoo is closed on Thursdays)"
    , color Color.black700
    ] <> FontStyle.body1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map ( \item -> 
              linearLayout
                [ height WRAP_CONTENT
                , margin $ MarginTop 16
                , width MATCH_PARENT
                ][  imageView
                  [ height $ V 24 
                  , width $ V 24
                  , margin $ MarginRight 16
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET item.image 
                  ]
                , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL 
                  ][  textView $
                      [ text item.headingText 
                      , color Color.black800
                      ] <> FontStyle.body6 TypoGraphy
                    , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ](map ( \subTextItem -> 
                                  textView $ 
                                  [ text subTextItem 
                                  , color Color.black700 
                                  , margin $ MarginTop 6
                                  ] <> FontStyle.body1 TypoGraphy ) item.subtext
                        )
                    ]                             
                  ]
      ) (convertServicesDataToViewData services)
    )
  ]

convertServicesDataToViewData :: Array ST.TicketServiceData -> Array {headingText :: String , subtext :: Array String, image :: String}
convertServicesDataToViewData services = do
  let timingsObject = 
        { headingText : "Zoo Timings"
        , image : "ny_ic_timing"
        , subtext : catMaybes $ map createTimingsSubtext services
        }
  [timingsObject] <> (map createFeeObject services)
  where
  createTimingsSubtext :: ST.TicketServiceData -> Maybe String
  createTimingsSubtext service = do
    openingTime <-  service.openTimings >>= convertUTCToISTAnd12HourFormat
    closingTime <- service.closeTimings >>= convertUTCToISTAnd12HourFormat
    pure $ service.service <> " Time: " <> openingTime <> " to " <> closingTime

  createFeeObject :: ST.TicketServiceData -> {headingText :: String , subtext :: Array String, image :: String}
  createFeeObject service = do
    { headingText : service.service <> " Fee"
    , image : iconMap service.service
    , subtext : map createFeeSubtext service.prices
    }

  createFeeSubtext :: ST.TicketServicePriceData -> String
  createFeeSubtext price =
    (priceInfoMap price.attendeeType) <> ": ₹" <> (show price.pricePerUnit)

  -- need to add this data at backend
  iconMap "Entrance" = "ny_ic_entry"
  iconMap "Aquarium" = "ny_ic_aquarium"
  iconMap "Videography" = "ny_ic_videography"
  iconMap _ = "ny_ic_entry"
  
  priceInfoMap "Adult" = "Visitors above the age of 5 years"
  priceInfoMap "Kid" = "Up to the age of 5 years"
  priceInfoMap "CameraUnit" = "Price per unit"
  priceInfoMap _ = "Price per unit"

chooseTicketsView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chooseTicketsView state push = 
  PrestoAnim.animationSet [Anim.fadeIn true]  $  
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  ][  textView $ 
      [ text "Date of Visit"
      , color Color.black900
      , margin $ MarginBottom 9
      ] <> FontStyle.subHeading1 TypoGraphy 
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 8.0 
      , background Color.white900
      , stroke $ "1," <> if state.props.validDate || (isStrEmpty state.data.dateOfVisit ) then Color.grey900 else Color.red
      , padding $ Padding 20 15 20 15
      , onClick (\action -> do
                _ <- push action
                JB.datePicker "" push $ DatePicker "DATE_OF_VISIT"
          ) (const NoAction)
      ][  imageView
          [ height $ V 22 
          , width $ V 22
          , margin $ MarginRight 8
          , layoutGravity "bottom"
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_calendar" 
          ]
        , textView $ 
          [ text if isStrEmpty state.data.dateOfVisit  then "Select Date Of Visit" else state.data.dateOfVisit
          , color Color.black800
          ] <> FontStyle.h3 TypoGraphy
      ]
    , textView $
      [ text "Tickets are available current day onwards" -- Tickets are available for upto 90 days in advance
      , visibility if state.props.validDate || isStrEmpty state.data.dateOfVisit  then GONE else VISIBLE
      , color Color.red 
      ] <> FontStyle.tags TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginTop 20
      ](map (ticketInputView push) (convertServicesDataToTicketsData state.data.servicesInfo))
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity BOTTOM
      , onClick push $ const ToggleTermsAndConditions
      ][  imageView
          [ height $ V 16
          , width $ V 16 
          , layoutGravity "center_vertical"
          , margin $ MarginRight 8
          , imageWithFallback $ fetchImage FF_COMMON_ASSET (if state.props.termsAndConditionsSelected then "ny_ic_checked" else "ny_ic_unchecked")
          ]
        , textView $ 
          [ text "I agree to the"
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , textView $ 
          [ text " Terms & Conditions"
          , color Color.blue900
          , onClick (\action -> do
                  _<- push action
                  _ <- JB.openUrlInApp $ "https://docs.google.com/document/d/1Aa5PRGaTTZM4HDdmvU_7_59B58wCQ0-bRezbsu-Inqw"
                  pure unit
                  ) (const NoAction)
          ] <> FontStyle.body1 TypoGraphy
      ]
    , textView $ 
      [ textFromHtml " &#8226;&ensp; Cancellation of tickets is not applicable"
      , margin $ MarginTop 13
      , color Color.black700
      ] <> FontStyle.tags TypoGraphy
  ]

convertServicesDataToTicketsData :: Array ST.TicketServiceData -> Array ST.Ticket
convertServicesDataToTicketsData services = do
  map createServiceTicket services
  where
  createServiceTicket :: ST.TicketServiceData -> ST.Ticket
  createServiceTicket service = do
    { title : service.service <> " Fee"
    , ticketID : service.id
    , ticketOption : map (convertServiceTicketOption service.id) service.prices
    , isExpanded : service.isExpanded
    }
  
  convertServiceTicketOption :: String -> ST.TicketServicePriceData -> ST.TicketOption
  convertServiceTicketOption ticketId price =
    { ticketID : ticketId
    , title : (ticketInfoMap price.attendeeType) <> " (₹" <> (show price.pricePerUnit) <> " per " <> (unitInfoMap price.attendeeType) <> ")"
    , currentValue : price.currentValue
    , subcategory : price.attendeeType
    }

  -- need to add this data at backend
  ticketInfoMap "CameraUnit" = "Devices"
  ticketInfoMap attendeeType = attendeeType <> " Ticket"

  unitInfoMap "Adult" = "person"
  unitInfoMap "Kid" = "person"
  unitInfoMap "CameraUnit" = "device"
  unitInfoMap _ = "unit"

ticketInputView :: forall w. (Action -> Effect Unit) -> ST.Ticket -> PrestoDOM (Effect Unit) w
ticketInputView push ticket = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600 
  , orientation VERTICAL
  , padding $ Padding 20 20 20 20
  , margin $ MarginBottom 20
  ][ linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , onClick push $ const (ToggleTicketOption ticket.ticketID)
    ][ textView $
        [ text ticket.title
        , color Color.black800
        ] <> FontStyle.h2 TypoGraphy
      , linearLayout
        [weight 1.0][]
      , imageView 
        [ height $ V 20 
        , width $ V 20 
        , imageWithFallback $ fetchImage FF_COMMON_ASSET if ticket.isExpanded then "ny_ic_checked" else "ny_ic_unchecked" 
        ]
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility if ticket.isExpanded then VISIBLE else GONE
        , orientation VERTICAL
        ](map (incrementDecrementView push) ticket.ticketOption)
  ]

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.TicketOption -> PrestoDOM (Effect Unit) w
incrementDecrementView push ticketOption = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  ][  textView $
      [ text ticketOption.title 
      , color Color.black800
      , margin $ MarginBottom 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , padding $ Padding 4 4 4 4
      , cornerRadius 8.0
      , background Color.white900
      , stroke $ "1," <> Color.grey900
      ][  textView $
          [ background Color.grey700
          , text "-"
          , gravity CENTER
          , cornerRadius 4.0
          , width WRAP_CONTENT
          , padding $ Padding 28 1 28 7
          , onClick push $ const (DecrementTicket ticketOption)
          , height WRAP_CONTENT
          ] <> FontStyle.body10 TypoGraphy
        , textView $
          [ background Color.white900
          , text $ show ticketOption.currentValue
          , height WRAP_CONTENT
          , color Color.black800
          , weight 1.0
          , gravity CENTER
          ] <> FontStyle.body13 TypoGraphy
        , textView $
          [ background Color.black900
          , text "+"
          , color Color.yellow900
          , padding $ Padding 28 1 28 7
          , cornerRadius 4.0
          , onClick push $ const (IncrementTicket ticketOption)
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ] <> FontStyle.body10 TypoGraphy
      ]
  ]

ticketsListView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketsListView state push = 
  linearLayout
  [ width $ MATCH_PARENT
  , height $ MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  , padding $ PaddingHorizontal 16 16
  ][ if DA.null state.props.ticketBookingList.booked then linearLayout[][] else ticketsCardListView state push state.props.ticketBookingList.booked "Booked Tickets"
  ,  if DA.null state.props.ticketBookingList.pendingBooking then linearLayout[][] else ticketsCardListView state push state.props.ticketBookingList.pendingBooking "Pending Payment"
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
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 120 -- $ WRAP_CONTENT
  , padding $ Padding 16 16 16 16
  , orientation HORIZONTAL
  , cornerRadius 8.0
  , stroke $ "1," <>  Color.grey700
  , margin $ MarginBottom 12
  , onClick push $ const $ GetBookingInfo booking.shortId booking.status
  , clickable true
  ][  imageView
      [ imageWithFallback $ getTicketStatusImage booking.status
      , width $ V 20
      , height $ V 20
      , margin $ Margin 0 4 12 0
      ]
    , linearLayout
      [ width $ MATCH_PARENT
      , height $ WRAP_CONTENT
      , orientation VERTICAL
      ][ textView $ 
         [ text booking.ticketPlaceName
         , width MATCH_PARENT
         , height WRAP_CONTENT
         , margin $ MarginBottom 8
         , color Color.black900
         ] <> FontStyle.subHeading1 TypoGraphy
      ,  textView
         [ text (convertUTCtoISC booking.visitDate "Do MMM, YYYY")
         , width MATCH_PARENT
         , height WRAP_CONTENT
         , margin $ MarginBottom 12
         ]
      ,  linearLayout
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , orientation HORIZONTAL
         , gravity CENTER_VERTICAL
         ][ textView 
            [ text "View"
            , color Color.blue900
            , margin $ MarginRight 8]
          , imageView
            [ width $ V 10
            , height $ V 8
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_blue_arrow"
            ]
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
  Booked -> "ny_ic_green_tick"
  Failed -> "ny_ic_payment_failed"


individualBookingInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
individualBookingInfoView state push =
  PrestoAnim.animationSet
  [ translateInXForwardAnim true , translateInXForwardAnim true] $ linearLayout
  [ height WRAP_CONTENT
  , width $ MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 16 20 16 20
  , onBackPressed push $ const BackPressed
  , afterRender push (const AfterRender)
  ][ zooTicketView state push
  ,  carouselDotView state push
  ]

zooTicketView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
zooTicketView state push =
  let activeItem = state.props.activeListItem
  in
  PrestoAnim.animationSet
  [ translateInXForwardAnim true , translateInXForwardAnim true] $
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , background $ getTicketBackgroundColor activeItem.ticketServiceName
  , orientation VERTICAL
  , padding $ Padding 16 24 16 24
  , cornerRadius 8.0
  ][ ticketHeaderView state push (getPlaceColor activeItem.ticketServiceName) (getInfoColor activeItem.ticketServiceName)
  ,  ticketImageView state push
  ,  bookingInfoView state push
  -- ,  shareTicketView state push --TODO:: Need to remove this from this screen
  ]

getTicketBackgroundColor :: String -> String
getTicketBackgroundColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.black900
  "Videography Fee" -> Color.yellow800
  "Aquarium Fee" -> "#DFE8FF"
  _ -> Color.grey900

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
  _ -> Color.grey900

getInfoColor :: String -> String
getInfoColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.white900
  "Videography Fee" -> Color.black900
  "Aquarium Fee" -> Color.black900
  _ -> Color.grey900

ticketHeaderView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> String -> String -> PrestoDOM (Effect Unit) w
ticketHeaderView state push placeColor infoColor  =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  ][ imageView
     [ margin $ Margin 0 0 10 0
     , width $ V 24
     , height $ V 24
     , imageWithFallback $ getTicketImage activeItem.ticketServiceName 
     ]
  ,  linearLayout
     [ width $ MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     ][ tvView state.props.selectedBookingInfo.ticketPlaceName placeColor (MarginBottom 0) (FontStyle.body3 TypoGraphy)
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][ tvView (convertUTCtoISC state.props.selectedBookingInfo.visitDate "Do MMM, YYYY") infoColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
        ,  dotView placeColor (Margin 10 10 10 10) 5
        ,  tvView ("Total : ₹ " <>  show state.props.selectedBookingInfo.amount) infoColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
        ]
     ]
  
  ]

getTicketImage :: String -> String
getTicketImage ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> fetchImage FF_ASSET "ny_ic_ticket"
  _ -> fetchImage FF_ASSET "ny_ic_ticket_black"

carouselDotView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
carouselDotView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop 12
  ] (DA.mapWithIndex (\index item -> if index == state.props.activeIndex then (dotView Color.black900 (Margin 2 2 2 2) 6) else (dotView Color.grey900 (Margin 2 2 2 2) 6) ) state.props.selectedBookingInfo.services)

ticketImageView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketImageView state push =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , margin $ MarginVertical 16 16
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      ][  linearLayout
          [ height WRAP_CONTENT
          , gravity CENTER
          ][ imageView
              [ width $ V 24
              , height $ V 24
              , imageWithFallback $ getLeftButtonForSlider state.props.activeListItem.ticketServiceName state.props.leftButtonDisable
              , onClick push $ const DecrementSliderIndex
              , visibility $ if state.props.leftButtonDisable then INVISIBLE else VISIBLE
              , clickable $ if state.props.leftButtonDisable then false else true
              ]
          ]
        , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , gravity CENTER
          ][ imageView
            [ width $ V 194
            , height $ V 194
            , id $ getNewIDWithTag "ticketQRView"
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , afterRender push (const (TicketQRRendered (getNewIDWithTag "ticketQRView") activeItem.ticketServiceShortId ))
            ]
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , gravity CENTER
          ][ imageView
            [ width $ V 24
            , height $ V 24
            , imageWithFallback $ getRightButtonForSlider state.props.activeListItem.ticketServiceName state.props.rightButtonDisable
            , onClick push $ const IncrementSliderIndex
            , visibility $ if state.props.rightButtonDisable then INVISIBLE else VISIBLE
            , clickable $ if state.props.rightButtonDisable then false else true
            ]
          ]
      ]
    , tvView (getTextForQRType activeItem.ticketServiceName) (getInfoColor activeItem.ticketServiceName) (MarginVertical 10 10) (FontStyle.subHeading1 TypoGraphy)
    , pillView state push (getPillBackgroundColor activeItem.ticketServiceName) (getPillInfoColor activeItem.ticketServiceName)
  ]

getTextForQRType :: String -> String
getTextForQRType ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> "Zoo Entry"
  "Videography Fee" -> "Photo/VideoGraphy"
  "Aquarium Fee" -> "Aquarium Entry"
  _ -> Color.white900
 
getPillBackgroundColor :: String -> String
getPillBackgroundColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.black6000
  "Videography Fee" -> Color.yellow900
  "Aquarium Fee" ->  Color.blue800
  _ -> Color.white900

getPillInfoColor :: String -> String
getPillInfoColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.grey900
  "Videography Fee" -> Color.black800
  "Aquarium Fee" ->  Color.white900
  _ -> Color.white900
  
getLeftButtonForSlider :: String -> Boolean -> String
getLeftButtonForSlider ticketServiceName buttonDisabled = case ticketServiceName of
  "Entrance Fee" -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_left_white"
  _ -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_left"

getRightButtonForSlider :: String -> Boolean -> String
getRightButtonForSlider ticketServiceName buttonDisabled = case ticketServiceName of
  "Entrance Fee" -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_right_white"
  _ -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_right"

pillView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> String -> String -> PrestoDOM (Effect Unit) w
pillView state push backgroudColor textColor =
  let activeItem = state.props.activeListItem
      itemLength = (DA.length activeItem.prices) - 1
  in
  linearLayout
  [ width $ WRAP_CONTENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , background backgroudColor
  , cornerRadius 30.0
  , padding $ Padding 16 6 16 6
  ] (DA.mapWithIndex (\index item ->
      linearLayout
      [ width $ WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ]([  tvView (show item.numberOfUnits <> " " <> item.attendeeType) textColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
      ] <> if index == itemLength then [] else [dotView (getPillInfoColor activeItem.ticketServiceName) (MarginHorizontal 6 6) 5] )
   ) activeItem.prices )

dotView :: forall w. String -> Margin -> Int -> PrestoDOM (Effect Unit) w
dotView color layMargin size =
  linearLayout
  [ width $ V size
  , height $ V size
  , background color
  , cornerRadius 30.0
  , margin $ layMargin
  ][]

tvView :: forall w. String -> String -> Margin -> (forall properties. (Array (Prop properties))) -> PrestoDOM (Effect Unit) w
tvView textString textColor textMargin fontSt = 
  textView
  ([ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text textString
  , color textColor
  , margin textMargin
  , gravity CENTER
  ] <>  fontSt )

bookingInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bookingInfoView state push =
  let activeItem = state.props.activeListItem
      validityTime = (fromMaybe "" activeItem.expiryDate)
      validUntil = (convertUTCtoISC validityTime "hh:mm A") <> ", " <> (convertUTCtoISC validityTime "Do MMM YYYY")
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  bookingInfoListItemView state "Service ID" activeItem.ticketServiceShortId
    , separatorView (getSeparatorColor activeItem.ticketServiceName)
    , bookingInfoListItemView state "Zoo Entry" (show activeItem.amount)
    , separatorView (getSeparatorColor activeItem.ticketServiceName)
    , bookingInfoListItemView state "Valid Until" validUntil
  ]

getSeparatorColor :: String -> String
getSeparatorColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.black700
  "Videography Fee" -> Color.white900
  "Aquarium Fee" -> Color.white900
  _ -> Color.white900

bookingInfoListItemView :: forall w.  ST.TicketBookingScreenState -> String -> String -> PrestoDOM (Effect Unit ) w
bookingInfoListItemView state key value =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 12 12
  ][  textView
      ([ weight 1.0
      , height WRAP_CONTENT
      , text key
      , color $ getPlaceColor activeItem.ticketServiceName
      ] <> FontStyle.body3 TypoGraphy)
    , textView
      ([ weight 1.0
      , height WRAP_CONTENT
      , text value
      , color $ getPlaceColor activeItem.ticketServiceName
      , gravity RIGHT
      ] <> FontStyle.body3 TypoGraphy)
  ]

shareTicketView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
shareTicketView state push =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop 16
  ][imageView
    [ height $ V 16
    , width $ V 16
    , imageWithFallback $ fetchImage FF_ASSET $ getShareButtonIcon state.props.activeListItem.ticketServiceName
    , margin $ MarginRight 8
    ]
  , textView $ 
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    , padding $ PaddingBottom 5
    , textFromHtml $ "<u>" <> "Share" <> "</u>"
    , color $ getShareButtonColor state.props.activeListItem.ticketServiceName
    ] <> FontStyle.body1 TypoGraphy
  ]

bookingStatusView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingStatusView state push paymentStatus = 
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , padding $ PaddingTop 20
  , background "#E2EAFF"
  ][ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ][  paymentStatusHeader state push paymentStatus
        , bookingStatusBody state push paymentStatus
      ]
  ]

copyTransactionIdView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
copyTransactionIdView state push visibility' = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER
  , visibility if visibility' then VISIBLE else GONE
  , onClick push $ const Copy
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

bookingStatusBody :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus ->  PrestoDOM (Effect Unit) w
bookingStatusBody state push paymentStatus = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  , orientation VERTICAL
  , margin $ Margin 16 16 16 16
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
              ][ imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ticket_black" 
                  , margin $ MarginRight 4
                  ]
                , commonTV push state.data.zooName Color.black900 (FontStyle.subHeading1 TypoGraphy) 0 LEFT NoAction
              ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ](DA.mapWithIndex ( \index item ->  keyValueView state item.key item.val index) state.data.keyValArray)
          ]
      ]
  ]

bookingConfirmationActions :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingConfirmationActions state push paymentStatus = 
  linearLayout
  [ width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  , padding $ PaddingBottom 20
  , alignParentBottom "true,-1"
  , visibility if (state.props.currentStage == ST.BookingConfirmationStage) then VISIBLE else GONE
  ][ linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.grey900
      , visibility GONE
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

paymentStatusHeader :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
paymentStatusHeader state push paymentStatus = 
  let transcationConfig = getTransactionConfig paymentStatus
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
      , PrimaryButton.view (push <<< RefreshStatusAC) (refreshStatusButtonConfig state)

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

keyValueView :: ST.TicketBookingScreenState -> String -> String -> Int -> forall w . PrestoDOM (Effect Unit) w
keyValueView state key value index = 
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
        ][ if index == 1 then bookingForView state else 
           textView $ 
            [ text value
            , color Color.black800
            ] <> FontStyle.body6 TypoGraphy
          ]
      ]
  ]

bookingForView :: forall w. ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
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