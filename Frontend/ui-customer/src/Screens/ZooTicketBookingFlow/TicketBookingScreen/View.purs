module Screens.TicketBookingScreen.View where

import Common.Types.App
import Screens.TicketBookingScreen.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Array (length, uncons, cons, take, drop, find, elem, mapWithIndex, filter)
import Data.Foldable (or)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String as DS
import Data.String.Common (joinWith)
import Effect (Effect)
import Engineering.Helpers.Commons (getCurrentUTC, screenWidth, flowRunner)
import Data.Foldable (foldl, foldMap)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getCurrentDatev2, getMinutesBetweenTwoUTChhmmss, fetchImage, FetchImageFrom(..), decodeError, convertUTCToISTAnd12HourFormat, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer)
import JBridge as JB
import Prelude (not, Unit, discard, void, bind, const, pure, unit, ($), (&&), (/=), (&&), (<<<), (+), (<>), (==), map, show, (||), show, (-), (>), (>>=), mod, negate, (<=), (>=), (<))
import PrestoDOM (FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), shimmerFrameLayout, afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, clickable, id, imageUrl)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingScreen.Controller (Action(..), ScreenOutput, eval, getLimitOfDaysAccToPlaceType)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingScreen.ComponentConfig 
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
      case initialState.data.placeInfo of
        Just (TicketPlaceResp place) -> do
          servicesResp <- Remote.getTicketPlaceServicesBT place.id
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
        , linearLayout
          [ height $ V 1 
          , width MATCH_PARENT
          , background Color.grey900
          ] []
        , if (state.props.currentStage == ST.ChooseTicketStage) && (not $ allowFutureBooking state.data.servicesInfo) && (placeClosed state.data.placeInfo) then (headerBannerView push state ("Booking closed currently. Opens after " <> getOpeningTiming state.data.placeInfo))
          else if (state.props.currentStage == ST.ChooseTicketStage) && (allowFutureBooking state.data.servicesInfo) && (placeClosedToday state.data.placeInfo state.data.dateOfVisit) then (headerBannerView push state ("Services closed for today. Tickets are available next day onwards"))
          else if (state.props.currentStage == ST.ChooseTicketStage) && (not $ allowFutureBooking state.data.servicesInfo) && (shouldHurry  state.data.placeInfo) then (headerBannerView push state ("Hurry! Booking closes at " <> getClosingTiming state.data.placeInfo))
          else linearLayout [][]
        , separatorView Color.greySmoke
        , scrollView
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , background Color.white900
            , afterRender push $ const AfterRender
            , id $ EHC.getNewIDWithTag "ParentScrollView"
            ]
            [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , gravity CENTER
                , orientation VERTICAL
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
  allowFutureBooking services = foldl (\acc service -> acc || service.allowFutureBooking) false services

  shouldHurry mbPlaceInfo = do
    case mbPlaceInfo of
      Nothing -> false
      Just (TicketPlaceResp pInfo) -> case pInfo.closeTimings of
        Nothing -> false
        Just closeTime -> do
          let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
          if currentTime < (convertUTCTimeToISTTimeinHHMMSS closeTime) then 
            case (getMinutesBetweenTwoUTChhmmss currentTime (convertUTCTimeToISTTimeinHHMMSS closeTime)) of
              Nothing -> false
              Just mins -> mins < 15
          else false

  getOpeningTiming mbPlaceInfo = do
    case mbPlaceInfo of
      Nothing -> ""
      Just (TicketPlaceResp pInfo) -> case pInfo.openTimings of
        Nothing -> ""
        Just time -> do
          let openTime = fromMaybe "" (convertUTCToISTAnd12HourFormat time)
          (replace (Pattern "00:00") (Replacement "12:00") openTime)

  getClosingTiming mbPlaceInfo = do
    case mbPlaceInfo of
      Nothing -> ""
      Just (TicketPlaceResp pInfo) -> case pInfo.closeTimings of
        Nothing -> ""
        Just time -> do
          let closeTime = fromMaybe "" (convertUTCToISTAnd12HourFormat time)
          (replace (Pattern "00:00") (Replacement "12:00") closeTime)

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
    else if (state.props.currentStage == ST.ChooseTicketStage) then [chooseTicketsView state push]
    else if (state.props.currentStage == ST.BookingConfirmationStage) then [ bookingStatusView state push state.props.paymentStatus ]
    else if (state.props.currentStage == ST.ViewTicketStage) then [ ticketsListView state push ]
    else if (state.props.currentStage == ST.TicketInfoStage) then [ individualBookingInfoView state push ]
    else []

  descriptionStateMainView state push placeInfo = 
    let (API.TicketPlaceResp place) = placeInfo
    in[ linearLayout
        [ width $ MATCH_PARENT
        , height $ V 360
        , gravity CENTER
        , margin $ MarginBottom 15
        ][  imageView
            [ height $ V $ (screenWidth unit) + 50
            , width  $ V $ (screenWidth unit) + 50
            , imageUrl $ fromMaybe "" place.iconUrl -- TODO:: Need to replace this default icon
            , layoutGravity "center"
            ]
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
    , locationView state push placeInfo.mapImageUrl placeInfo.lat placeInfo.lon
    , serviceBreakUpView state push state.data.servicesInfo (TicketPlaceResp placeInfo)
  ]

termsAndConditionsView :: forall w . Array String -> PrestoDOM (Effect Unit) w
termsAndConditionsView termsAndConditions =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ] (mapWithIndex (\index item ->
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][ textView $
         [ textFromHtml $ " &#8226;&ensp; " <> item
         , color Color.black900
         ] <> FontStyle.tags TypoGraphy
      ]
  ) termsAndConditions )

locationView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Maybe String -> Maybe Number -> Maybe Number -> PrestoDOM (Effect Unit) w
locationView state push icon lat lon =
  linearLayout
  [ height WRAP_CONTENT 
  , width MATCH_PARENT
  , margin $ MarginTop 24
  , orientation VERTICAL
  , onClick push (const $ OpenGoogleMap lat lon)
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
      , imageUrl $ fromMaybe "" icon -- TODO:: Need to replace this default icon
      ]
  ]

serviceBreakUpView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Array ST.TicketServiceData -> TicketPlaceResp -> PrestoDOM (Effect Unit) w
serviceBreakUpView state push services (TicketPlaceResp ticketPlaceResp) =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600
  , orientation VERTICAL 
  , padding $ Padding 20 20 20 20
  , margin $ MarginTop 24
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map ( \item ->
              let transformedServiceData = transformBusinessHours item.businessHours item.timeIntervalData
              in
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][  textView $
                  [ text $ item.serviceName
                  , color Color.black800
                  , margin $ MarginVertical 10 10
                  ] <> FontStyle.subHeading1 TypoGraphy
                , businessHoursView push state item.serviceName transformedServiceData
                , feesBreakUpView push state item.serviceName transformedServiceData
              ]) 
      services)
    , termsAndConditionsView ticketPlaceResp.termsAndConditions
  ]

  where
    transformBusinessHours bhs slotTimeIntervals = do
      let mbSelectedMaxOperationalDaysBH = findMaxOperationalDays slotTimeIntervals
      case mbSelectedMaxOperationalDaysBH of
        Nothing -> { timings : [], fees : []} 
        Just slotTimeIntervalInfo -> do
          let bhId = case head slotTimeIntervalInfo.slot of
                        Nothing -> maybe "" (\x -> x.bhourId) (head slotTimeIntervalInfo.timeIntervals)
                        Just slot -> slot.bhourId
          { timings : map (getBusinessHoursAndTimings) slotTimeIntervals, fees : getFeesForService bhId bhs} 

    getBusinessHoursAndTimings slotTimeInterval = 
      let timeIntervalString = joinWith " , " (map getTimeIntervals slotTimeInterval.timeIntervals)
          slotIntervalString = joinWith " , " (map get12HoursFormat slotTimeInterval.slot)
          finalSlotTimeIntervalString = slotIntervalString <> (if (slotIntervalString /= "" && timeIntervalString /= "") then (",") else "") <> timeIntervalString
      in { key : joinWith ", " (map (\x -> DS.take 3 x ) slotTimeInterval.operationalDays), 
           val : if finalSlotTimeIntervalString == "" then "Closed" else finalSlotTimeIntervalString
         }


    get12HoursFormat slot = case (convertUTCToISTAnd12HourFormat slot.slot) of
       Nothing -> ""
       Just sl -> sl

    getTimeIntervals timeInterval = if timeInterval.startTime == "" then 
                                      if timeInterval.endTime /= "" then " till " <>  (fromMaybe "" (convertUTCToISTAnd12HourFormat timeInterval.endTime)) 
                                      else ""
                                    else (fromMaybe "" (convertUTCToISTAnd12HourFormat timeInterval.startTime)) <> if timeInterval.endTime /= "" then " to " <>  (fromMaybe "" (convertUTCToISTAnd12HourFormat timeInterval.endTime)) else " onwards"

    getFeesForService bhId bhs = do
      case (find (\bh -> bh.bhourId == bhId) bhs) of
        Nothing -> []
        Just bh -> map (getCategoryMap (length bh.categories == 1)) bh.categories

    getCategoryMap isSingleCategory cat  = { key : cat.categoryName, val : map (getPCMap isSingleCategory (length cat.peopleCategories == 1)) cat.peopleCategories, disableCategory : isSingleCategory}

    getPCMap isSingleCategory isSinglePC pc = {key : getCategoryNameMap pc.peopleCategoryName isSingleCategory isSinglePC , val : "₹" <>  show pc.pricePerUnit }

    getCategoryNameMap catName isSingleCat isSinglePC = case catName, isSinglePC, isSingleCat  of 
                                                      "Adult", true, _ -> "Per Person"
                                                      "Adult", _, true -> "Visitors above the age of 5 years"
                                                      "Adult", _, _    -> "Adult (5+ years)"
                                                      "Kid", true, _ -> "Per Person"
                                                      "Kid", _, true -> "Up to the age of 5 years"
                                                      "Kid", _, _    -> "Child (<5 years)"
                                                      "Cruise", _, _ -> "Per Person"
                                                      "Passenger Vessel", _, _ -> "Per Person"
                                                      _, true, _       -> "Per Unit"
                                                      _, _, _          -> "Per Person"

    findMaxOperationalDays :: Array ST.SlotsAndTimeIntervalData -> Maybe ST.SlotsAndTimeIntervalData
    findMaxOperationalDays [] = Nothing
    findMaxOperationalDays xs = case uncons xs of
                                  Nothing -> Nothing
                                  Just {head: x, tail: ys} ->  Just $ foldl (\maxElem elem -> if length elem.operationalDays > length maxElem.operationalDays then elem else maxElem) x ys

businessHoursView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> ST.TiketingListTransformedData -> PrestoDOM (Effect Unit) w
businessHoursView push sate serviceName screenData =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 10
  ][  imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_timing"
      , margin $ MarginRight 5
      ]
   ,  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  textView
          [ text "Timings"
          , color Color.black800
          , textSize FontSize.a_14
          , margin $ MarginBottom 5
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ] 
        , linearLayout
          [ width $ MATCH_PARENT
          , height $ WRAP_CONTENT
          , orientation VERTICAL
          ] (map (\item ->  
                  textView $
                    [ textFromHtml $ "<b>" <> item.key <> " : " <> "</b>" <> item.val
                    , gravity LEFT
                    , textSize FontSize.a_14
                    , fontStyle $ FontStyle.bold LanguageStyle
                    ] <>  FontStyle.body1 TypoGraphy       
            ) screenData.timings)
        ]
  ]

feesBreakUpView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> ST.TiketingListTransformedData -> PrestoDOM (Effect Unit) w
feesBreakUpView push state serviceName screenData =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 10
  ][  imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_entry"
      , margin $ MarginRight 5
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  textView
          [ text "Fees"
          , color Color.black800
          , textSize FontSize.a_14
          , margin $ MarginBottom 5
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ] 
        , linearLayout
          [ width $ MATCH_PARENT
          , height $ WRAP_CONTENT
          , orientation VERTICAL
          ] (map (\item ->  
                    linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation HORIZONTAL
                    ][  textView
                        [ width $ V 100
                        , text $ item.key <> " : "
                        , color Color.black700
                        , textSize FontSize.a_14
                        , fontStyle $ FontStyle.bold LanguageStyle
                        , visibility $ if item.disableCategory then GONE else VISIBLE
                        ] 
                     ,  priceView item.val item.disableCategory
                    ]
          
          ) screenData.fees)
      ]
  ]


priceView :: forall w . Array ST.KeyVal -> Boolean -> PrestoDOM (Effect Unit) w
priceView prices categoryDisabled =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ]( map (\item ->
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][  textView $
            [ text $ item.key <> " : "
            , gravity LEFT
            , textSize $ if categoryDisabled then FontSize.a_14 else FontSize.a_12
            ] <> ( if categoryDisabled then [fontStyle $ FontStyle.bold LanguageStyle] else (FontStyle.body1 TypoGraphy) )
              
          , textView $
            [ text item.val
            , gravity LEFT
            , textSize FontSize.a_12
            ] <> FontStyle.body1 TypoGraphy 
        ]  
  ) prices)


chooseTicketsView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chooseTicketsView state push = 
  let serviceData = (convertServicesDataToTicketsData state.props.selectedOperationalDay state.data.servicesInfo)
      filteredServiceData = DA.filter (\ticket -> not (length ticket.timeIntervals == 0 && length (getFilteredSlots ticket.slot state) == 0)) serviceData
      slotsStillThere = DA.filter (\ticket -> (length ticket.timeIntervals /= 0 || length ticket.slot /= 0)) serviceData
  in
  PrestoAnim.animationSet [Anim.fadeIn true]  $  
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.grey700
  , padding $ Padding 16 24 16 16
  , orientation VERTICAL
  ] $ [ linearLayout
        [ width $ MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility $ if checkIfDateOfTripVisible state.data.servicesInfo then VISIBLE else GONE
        , margin $ MarginBottom 20
        ][  textView $ 
            [ text "Date of Trip"
            , color Color.black900
            , margin $ MarginBottom 9
            ] <> FontStyle.subHeading1 TypoGraphy 
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius 8.0 
            , background Color.white900
            , stroke $ "1," <> if state.props.validDate || (state.data.dateOfVisit == "") then Color.grey900 else Color.red
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
                [ text $ if state.data.dateOfVisit == "" then "Select Date Of Visit" else (convertUTCtoISC state.data.dateOfVisit "dddFull, DD/MM/YY")
                , color Color.black800
                ] <> FontStyle.h3 TypoGraphy
            ]
          , textView $
            [ text $ getMessageForSelectedDate state -- Tickets are available for upto 90 days in advance
            , visibility if state.props.validDate || state.data.dateOfVisit == "" then GONE else VISIBLE
            , color Color.red 
            , margin $ MarginVertical 8 8
            ] <> FontStyle.tags TypoGraphy
        ]
      , if (length filteredServiceData == 0) && (length slotsStillThere /= 0) then (noDataView state push "Services closed for today. Tickets are available next day onwards")
        else if length filteredServiceData == 0 then (noDataView state push "No services available for selected date")
        else (linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ](map (ticketInputView push state) filteredServiceData))
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
      , termsAndConditionsView (getTermsAndConditions state.data.placeInfo)
  ]
  where 
    checkIfDateOfTripVisible services = foldl (\acc service -> acc || service.allowFutureBooking) false services 
    getTermsAndConditions placeInfo = maybe [] (\(TicketPlaceResp x ) -> x.termsAndConditions) placeInfo
    getMessageForSelectedDate state = do
      if (getCurrentDatev2 "") < state.data.dateOfVisit then "Date Error! Booking allowed only upto " <> show (getLimitOfDaysAccToPlaceType state) <> " days in advance"
      else "Tickets are available current day onwards"

    getFilteredSlots slots state = do
      let currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
      if currentDate == (convertUTCtoISC state.data.dateOfVisit "YYYY-MM-DD") then do
        let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
        filter (\slot -> (convertUTCTimeToISTTimeinHHMMSS slot.slot) > currentTime) slots
      else slots

convertServicesDataToTicketsData :: String -> Array ST.TicketServiceData -> Array ST.Ticket
convertServicesDataToTicketsData selectedOperationalDay services = do
  map createServiceTicket services
  where
  createServiceTicket :: ST.TicketServiceData -> ST.Ticket
  createServiceTicket service = do
    let  slotTIInfo = getTimeIntervalDataForSelectedBH service.timeIntervalData selectedOperationalDay
    { title : service.serviceName <> " Fee"
    , shortDesc : service.shortDesc
    , ticketID : service.id
    , isExpanded : service.isExpanded
    , businessHours : map (convertToTicketBusinessHours service.id) service.businessHours
    , timeIntervals : slotTIInfo.timeIntervals
    , slot : slotTIInfo.slot
    , selectedBHid : service.selectedBHid
    , selectedSlot : service.selectedSlot
    }

  getTimeIntervalDataForSelectedBH :: Array ST.SlotsAndTimeIntervalData -> String -> { timeIntervals :: Array ST.TimeInterval, slot :: Array ST.SlotInterval}
  getTimeIntervalDataForSelectedBH slotsTimeIntervalInfo selectedOperationalDay = do
    let mbSlotTimeInterval = find (\sti -> selectedOperationalDay `elem` sti.operationalDays) slotsTimeIntervalInfo
    maybe  { timeIntervals : [], slot : [] }  (\x -> { timeIntervals : x.timeIntervals, slot : x.slot } )  mbSlotTimeInterval

  convertToTicketBusinessHours :: String -> ST.BusinessHoursData -> ST.TicketBusinessHoursOptionData
  convertToTicketBusinessHours serviceId serviceBusinessHr = do
    { ticketID : serviceId,
      bhourId :serviceBusinessHr.bhourId,
      categories : map (convertToTicketCategories serviceId) serviceBusinessHr.categories,
      operationalDays : serviceBusinessHr.operationalDays
    }

  convertToTicketCategories :: String -> ST.TicketCategoriesData -> ST.TicketCategoriesOptionData
  convertToTicketCategories serviceId category = do
    { ticketID : serviceId,
      categoryName : category.categoryName, -- (SEAT-TYPES, DESTINATION, ZOO)
      categoryId : category.categoryId,
      availableSeats : category.availableSeats,
      allowedSeats : category.allowedSeats,
      bookedSeats : category.bookedSeats,
      peopleCategories : map (convertServiceTicketOption serviceId)  category.peopleCategories,
      isSelected : category.isSelected
    }

  convertServiceTicketOption :: String -> ST.PeopleCategoriesRespData -> ST.TicketPeopleCategoriesOptionData
  convertServiceTicketOption ticketId peopleCategory =
    { ticketID : ticketId
    , title : (ticketInfoMap peopleCategory.peopleCategoryName) <> " (₹" <> (show peopleCategory.pricePerUnit) <> " per " <> (unitInfoMap peopleCategory.peopleCategoryName) <> ")"
    , currentValue : peopleCategory.currentValue
    , subcategory : peopleCategory.peopleCategoryName
    , pricePerUnit : peopleCategory.pricePerUnit
    , ticketLimitCrossed : peopleCategory.ticketLimitCrossed
    , peopleCategoryId : peopleCategory.peopleCategoryId
    }

  -- need to add this data at backend
  ticketInfoMap "CameraUnit" = "Devices"
  ticketInfoMap peopleCategoryName = peopleCategoryName <> " Ticket"

  unitInfoMap "Adult" = "person"
  unitInfoMap "Kid" = "person"
  unitInfoMap "CameraUnit" = "device"
  unitInfoMap _ = "unit"

ticketInputView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.Ticket -> PrestoDOM (Effect Unit) w
ticketInputView push state ticket = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ] $ [] <> (if isValid ticket then [individualTicketView push state ticket] else [])
  where
    isValid ticket = not (length ticket.timeIntervals == 0 && length (getFilteredSlots ticket.slot state) == 0)

    getFilteredSlots slots state = do
      let currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
      if currentDate == (convertUTCtoISC state.data.dateOfVisit "YYYY-MM-DD") then do
        let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
        filter (\slot -> (convertUTCTimeToISTTimeinHHMMSS slot.slot) > currentTime) slots
      else slots

individualTicketView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.Ticket -> PrestoDOM (Effect Unit) w
individualTicketView push state ticket =
  let valBH =  (findValidBusinessHour state.props.selectedOperationalDay ticket.selectedBHid ticket.timeIntervals ticket.slot state ticket.businessHours)
      -- bookingClosed = (not state.props.validDate) || ((not $ allowFutureBooking state.data.servicesInfo) && (placeClosed state.data.placeInfo)) || (allowFutureBooking state.data.servicesInfo && (not $ isJust valBH))
      bookingClosedForService = (not state.props.validDate) || not (validHourPresent state.props.selectedOperationalDay ticket.selectedBHid ticket.timeIntervals ticket.slot state ticket.businessHours)
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.white900 
  , orientation VERTICAL
  , padding $ Padding 20 20 20 20
  , margin $ MarginBottom 20
  , stroke $ "1," <> Color.grey900
  , clickable (not bookingClosedForService)
  ] $ [  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , clickable (not bookingClosedForService)
      , onClick push $ const (ToggleTicketOption ticket.ticketID)
      ][  linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , orientation VERTICAL
          ][ textView $
             [ text ticket.title
             , color $ if bookingClosedForService then Color.black600 else Color.black800
             ] <> FontStyle.h2 TypoGraphy
          ,  textView $
             [ text $ fromMaybe "" ticket.shortDesc
             , color $ if bookingClosedForService then Color.greyDark else Color.black800
             , visibility $ maybe GONE (\x -> VISIBLE) ticket.shortDesc
             ] <> FontStyle.body1 TypoGraphy
          ]
        -- , linearLayout
        --   [weight 1.0][]
        , imageView 
          [ height $ V 20 
          , width $ V 20 
          , margin $ MarginLeft 10
          , imageWithFallback $ fetchImage FF_COMMON_ASSET if (ticket.isExpanded && not bookingClosedForService) then "ny_ic_checked" else "ny_ic_unchecked" 
          ]
      ]
  ] <> (if ticket.isExpanded && (not bookingClosedForService) then [individualTicketBHView push state valBH ticket] else [])
  where
    -- allowFutureBooking services = foldl (\acc service -> acc || service.allowFutureBooking) false services

    validHourPresent selOperationalDay selBHId timeIntervals slots state bhs = 
      if (length timeIntervals == 0) && (length slots == 0) then do
              let now = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
              false
      else case selBHId of 
        Nothing -> do
          let now = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
              currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
              selTimeInterval = timeIntervals DA.!! 0
          if currentDate == state.data.dateOfVisit then
            case selTimeInterval of
              Nothing -> (length slots > 0)
              Just sti -> do
                let startTime = if sti.startTime /= "" then convertUTCTimeToISTTimeinHHMMSS sti.startTime else ""
                    endTime = if sti.endTime /= "" then convertUTCTimeToISTTimeinHHMMSS sti.endTime else ""
                if (startTime /= "" && endTime /= "") then do
                  if (startTime < now && now < endTime) then true
                  else false
                else if (endTime /= "") then do
                  if (now < endTime) then true
                  else false 
                else (length slots > 0)
          else true
        Just bhId -> true

    findValidBusinessHour selOperationalDay selBHId timeIntervals slot state  bhs = 
      if (length timeIntervals == 0) && (length slot == 0) then do
        let now = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
        Nothing
      else case selBHId of 
        Nothing -> do
          let now = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
              currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
              selTimeInterval = timeIntervals DA.!! 0
          case selTimeInterval of
              Nothing -> Nothing
              Just sti -> do
                let startTime = if sti.startTime /= "" then convertUTCTimeToISTTimeinHHMMSS sti.startTime else ""
                    endTime = if sti.endTime /= "" then convertUTCTimeToISTTimeinHHMMSS sti.endTime else ""
                if currentDate == state.data.dateOfVisit then do
                    if (startTime /= "" && endTime /= "") then do
                      if (startTime < now && now < endTime) then find (\bh -> bh.bhourId == sti.bhourId && selOperationalDay `elem` bh.operationalDays) bhs
                      else Nothing
                    else if (endTime /= "") then do
                      if (now < endTime) then find (\bh -> bh.bhourId == sti.bhourId && selOperationalDay `elem` bh.operationalDays) bhs
                      else Nothing 
                    else Nothing
                else find (\bh -> bh.bhourId == sti.bhourId && selOperationalDay `elem` bh.operationalDays) bhs
        Just bhId -> find (\bh -> bh.bhourId == bhId && selOperationalDay `elem` bh.operationalDays) bhs

placeClosed :: Maybe TicketPlaceResp -> Boolean
placeClosed mbPlaceInfo = do
  case mbPlaceInfo of
    Nothing -> false
    Just (TicketPlaceResp pInfo) -> do
      let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
      case pInfo.closeTimings of
        Nothing -> false
        Just closeTime -> case pInfo.openTimings of
            Nothing -> false
            Just startTime -> do
              if (convertUTCTimeToISTTimeinHHMMSS startTime) <= currentTime && currentTime <= (convertUTCTimeToISTTimeinHHMMSS closeTime) then false else true

placeClosedToday :: Maybe TicketPlaceResp -> String -> Boolean
placeClosedToday mbPlaceInfo dateOfVisit = do
  case mbPlaceInfo of
    Nothing -> false
    Just (TicketPlaceResp pInfo) -> do
      let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
          currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
      if currentDate == dateOfVisit then
        case pInfo.closeTimings of
          Nothing -> false
          Just closeTime -> do
            if currentTime <= closeTime then false else true
      else false

individualTicketBHView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> Maybe ST.TicketBusinessHoursOptionData -> ST.Ticket -> PrestoDOM (Effect Unit) w
individualTicketBHView push state valBH ticket =
  PrestoAnim.animationSet [
    Anim.translateInYAnim translateYAnimConfig { duration = 3000 , fromY = -10 , toY = 0}
  ] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ] $ [
  ] 
    <> (case valBH of 
          Nothing -> []
          Just bh -> if DA.length bh.categories > 1 then [selectDestinationView push state bh.categories] else (map (incrementDecrementView push state) (convertBHToPeopleCategory bh.categories)))
    <> (if DA.length ticket.slot > 0 then [timeSlotView push state ticket.ticketID ticket.selectedBHid ticket.slot] else [])
  where 
    convertBHToPeopleCategory categories = do
      let mbCurrentCategory = DA.head categories
      case mbCurrentCategory of
        Nothing -> []
        Just category -> category.peopleCategories

selectDestinationView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> Array ST.TicketCategoriesOptionData -> PrestoDOM (Effect Unit) w
selectDestinationView push state categories =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ] [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ] (map (incrementDecrementView push state) (convertBHToPeopleCategory filteredCategories))
    , textView $
      [ text "Select your ticket category"
      , color Color.black800
      , margin $ MarginVertical 20 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ] (map (selectDestinationViewPill push state) categories)
  ]
  where filteredCategories = DA.filter (\category -> category.isSelected) categories
        convertBHToPeopleCategory categories = do
          let mbCurrentCategory = DA.head categories
          case mbCurrentCategory of
            Nothing -> []
            Just category -> category.peopleCategories

timeSlotView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> Maybe String -> Array ST.SlotInterval -> PrestoDOM (Effect Unit) w
timeSlotView push state ticketID selectedBHid slots = 
  let filteredSlots = getFilteredSlots slots state
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ if DA.length filteredSlots > 0 then VISIBLE else GONE
  ] [ textView $
      [ text "Time Slot"
      , color Color.black800
      , margin $ MarginVertical 20 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ] ( map (timeSlotRowView push ticketID selectedBHid) (convertTimeSlotsToGroupedArray filteredSlots))
  ]
  where
    getFilteredSlots slots state = do
      let currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
      if currentDate == (convertUTCtoISC state.data.dateOfVisit "YYYY-MM-DD") then do
        let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
        filter (\slot -> (convertUTCTimeToISTTimeinHHMMSS slot.slot) > currentTime) slots
      else slots

timeSlotRowView :: forall w . (Action -> Effect Unit) -> String -> Maybe String -> Array ST.SlotInterval -> PrestoDOM (Effect Unit) w
timeSlotRowView push ticketID selectedBHid slotArr =
  let len = DA.length slotArr
  in linearLayout
     [ width $ MATCH_PARENT
     , height MATCH_PARENT
     , orientation HORIZONTAL
     , margin $ MarginBottom 8
     ] $ [
     ] <> (case slotArr DA.!! 0 of
             Nothing -> []
             Just val -> [timeSlotPillView push ticketID selectedBHid val len true])
       <> (case slotArr DA.!! 1 of
             Nothing -> []
             Just val -> [timeSlotPillView push ticketID selectedBHid val len false])

timeSlotPillView :: forall w . (Action -> Effect Unit) -> String ->  Maybe String -> ST.SlotInterval -> Int -> Boolean  -> PrestoDOM (Effect Unit) w
timeSlotPillView push ticketID selectedBHid slotInterval len isfirst =
  linearLayout
  [ weight 1.0
  , height WRAP_CONTENT
  , stroke $ "1," <> (if selectedBHid == Just slotInterval.bhourId then Color.blue800 else Color.grey900)
  , background $ (if selectedBHid == Just slotInterval.bhourId then Color.blue600 else Color.white900)
  , padding $ Padding 16 8 16 8
  , gravity CENTER
  , onClick push (const $ SelectSlot ticketID slotInterval)
  , cornerRadius 8.0
  , margin $ if len == 1 then Margin 0 0 0 0 else if isfirst then MarginRight 5 else MarginLeft 5
  ][ textView $
     [ text $ fromMaybe "" $ convertUTCToISTAnd12HourFormat slotInterval.slot
     , width $ V 100
     , gravity CENTER
     , color $ (if selectedBHid == Just slotInterval.bhourId then Color.blue800 else Color.black700)
     , fontStyle $ FontStyle.bold LanguageStyle
     ]
  ]

selectDestinationViewPill :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.TicketCategoriesOptionData -> PrestoDOM (Effect Unit) w
selectDestinationViewPill push state category =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , onClick push (const $ SelectDestination category)
  , padding $ Padding 16 16 16 16
  , stroke $ "1," <> (if category.isSelected then Color.blue800 else Color.grey900)
  , cornerRadius 8.0
  , margin $ MarginBottom 8
  , background $ (if category.isSelected then Color.blue600 else Color.white900)
  , gravity CENTER
  ][  imageView
      [ imageWithFallback $ fetchImage FF_COMMON_ASSET if category.isSelected then "ny_ic_radio_selected_blue" else "ny_ic_radio_unselected" 
      , width $ V 20
      , height $ V 20
      , margin $ MarginRight 8
      ]
    , textView $
      [ text category.categoryName
      , color $ (if category.isSelected then Color.blue800 else Color.black700)
      , gravity LEFT
      ] <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
      [weight 1.0][]
  ]

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.TicketPeopleCategoriesOptionData -> PrestoDOM (Effect Unit) w
incrementDecrementView push state pcCategory  =
  let ticketLimit = getTicketIncrementLimit state
  in
  PrestoAnim.animationSet [
    Anim.translateInYAnim translateYAnimConfig { duration = 3000 , fromY = -5, toY = 0}
  ] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  ][  textView $
      [ text pcCategory.title 
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
          , onClick push $ const (DecrementTicket pcCategory ticketLimit)
          , height WRAP_CONTENT
          ] <> FontStyle.body10 TypoGraphy
        , textView $
          [ background Color.white900
          , text $ show pcCategory.currentValue
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
          , onClick push $ const (IncrementTicket pcCategory ticketLimit)
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ] <> FontStyle.body10 TypoGraphy
      ]
    , textView $
      [ text $ "Upto " <> show ticketLimit <> " tickets can only be booked at a time"
      , visibility $ if pcCategory.ticketLimitCrossed then VISIBLE else GONE
      , color Color.red 
      , margin $ MarginTop 8
      ] <> FontStyle.tags TypoGraphy
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
  let property = getTicketStatusBackgroundColor booking.status
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 120 -- $ WRAP_CONTENT
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
          , width WRAP_CONTENT
          , margin $ MarginBottom 8
          , color Color.black900
          ] <> FontStyle.subHeading1 TypoGraphy
        ,linearLayout
         [ weight 1.0 ][]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , background $ property.bgColor
          , padding $ Padding 0 0 10 0
          , cornerRadius 30.0
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
              ]
          ]
      ]
    , textView
      [ text (convertUTCtoISC booking.visitDate "Do MMM, YYYY")
      , width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 12
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ][  textView 
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

getTicketStatusBackgroundColor :: BookingStatus -> {bgColor :: String, statusText :: String }
getTicketStatusBackgroundColor status = case status of 
  Pending -> { bgColor : Color.yellow900, statusText : "Pending" }
  Booked ->  { bgColor : Color.green900, statusText : "Booked" }
  Failed ->  { bgColor : Color.red900, statusText : "Cancelled" }

individualBookingInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
individualBookingInfoView state push =
  PrestoAnim.animationSet
  [ translateInXForwardAnim true , translateInXForwardAnim true] $ linearLayout
  [ height WRAP_CONTENT
  , width $ MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 16 20 16 20
  -- , onBackPressed push $ const BackPressed
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
  , background $ Color.black800 -- getTicketBackgroundColor activeItem.ticketServiceName
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
  _ -> Color.black800

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
      peopleCatInfo = getPCs activeItem.categories
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
      ]([  tvView (show item.numberOfUnits <> " " <> item.name) textColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
      ] <> if index == peopleCatInfo.length then [] else [dotView (getPillInfoColor activeItem.ticketServiceName) (MarginHorizontal 6 6) 5] )
   ) peopleCatInfo.pcs )
   where
    getPCs :: Array ST.TicketBookingCategoryDetails -> {length :: Int, pcs :: Array ST.TicketBookingPeopleCategoryDetails}
    getPCs categories = case categories DA.!! 0 of
      Nothing -> { length : 0, pcs : []}
      Just cat -> {length : length cat.peopleCategories, pcs : cat.peopleCategories}

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

errorView :: forall w. ST.TicketBookingScreenState -> String -> PrestoDOM (Effect Unit) w
errorView state message =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ textView $
     [ text message
     , color Color.red900
     ] <> FontStyle.body3 TypoGraphy
  ]

headerBannerView :: forall w . (Action -> Effect Unit) ->  ST.TicketBookingScreenState -> String -> PrestoDOM (Effect Unit) w
headerBannerView push state message =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , background Color.yellow800
  ][ textView $
     [ text message -- "Hurry! Limited number of tickets available!"
     , color Color.black900
     , padding $ Padding 16 10 16 12
     ] <> FontStyle.subHeading1 TypoGraphy
  ]

getTransactionConfig :: Common.PaymentStatus -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig status = 
  case status of
    Common.Success -> {image : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", statusTimeDesc : "Your ticket has been generated below", title : "Your booking is Confirmed!"}
    Common.Pending -> {image : fetchImage FF_COMMON_ASSET "ny_ic_transaction_pending", statusTimeDesc : "Please check back in a few minutes.", title : "Your booking is Pending!"}
    Common.Failed  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_payment_failed", statusTimeDesc : "Please retry booking.", title : "Booking Failed!"}
    Common.Scheduled  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_pending", statusTimeDesc : "", title : ""}

convertTimeSlotsToGroupedArray :: Array ST.SlotInterval -> Array (Array ST.SlotInterval)
convertTimeSlotsToGroupedArray slots = groupAdjacent slots

groupAdjacent :: forall a. Array a -> Array (Array a)
groupAdjacent [] = []
groupAdjacent x = cons (take 2 x) (groupAdjacent (drop 2 x))

getTicketIncrementLimit :: ST.TicketBookingScreenState -> Int
getTicketIncrementLimit state = case state.data.placeInfo of
  Nothing -> 4
  Just (TicketPlaceResp pInfo) -> case pInfo.placeType of
    "HeritageSite" -> 4
    _ -> 100