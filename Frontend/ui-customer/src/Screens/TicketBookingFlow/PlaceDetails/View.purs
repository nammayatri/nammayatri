module Screens.TicketBookingFlow.PlaceDetails.View where

import Common.Types.App
import Screens.TicketBookingFlow.PlaceDetails.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Domain.Payments as PP
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Array (foldr, length, uncons, cons, take, drop, find, elem, mapWithIndex, filter, null, sortBy, groupBy, (:))
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
import Prelude (not, Unit, discard, void, bind, const, pure, unit, ($), (&&), (/=), (&&), (<<<), (+), (<>), (==), map, show, (||), show, (-), (>), (>>=), mod, negate, (<=), (>=), (<),(>>>),(<$>))
import PrestoDOM (FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), shimmerFrameLayout, afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, clickable, id, imageUrl, maxLines, ellipsize, lineHeight, fillViewport, alpha)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingFlow.PlaceDetails.Controller (Action(..), ScreenOutput, eval, getLimitOfDaysAccToPlaceType)
import Screens.Types as ST
import Styles.Colors as Color
import Data.Ord (compare)
import Data.Array.NonEmpty as DAN

import Resources.Constants -- TODO:: Replace these constants with API response
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag, convertUTCTimeToISTTimeinHHMMSS)
import Services.API (ServiceExpiry(..), BookingStatus(..), TicketPlaceResponse(..), TicketPlaceResp(..), TicketServiceResp(..), PlaceType(..), BusinessHoursResp(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), TicketServicesResponse(..), SpecialDayType(..))
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
import Mobility.Prelude (groupAdjacent, sortAccToDayName, boolToVisibility)
import Language.Strings (getString)
import Language.Types (STR(..))

screen :: ST.TicketBookingScreenState -> Screen Action ST.TicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TicketBookingScreen"
  , globalEvents : [getPlaceDataEvent]
  , eval :
    \action state -> do
        let _ = spy "ZooTicketBookingFlow PlaceDetails action " action
        let _ = spy "ZooTicketBookingFlow PlaceDetails state " state
        eval action state
  }
  where
  getPlaceDataEvent push = do
    void $ runEffectFn1 consumeBP unit
    void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ getPlaceDataEvent' push
    pure $ pure unit

  getPlaceDataEvent' push = do
    if (any (_ == initialState.props.currentStage) [ST.DescriptionStage]) then do
      case initialState.data.placeInfo of
        Just (TicketPlaceResp place) -> do
          servicesResp <- Remote.getTicketPlaceServicesBT place.id
          lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData (Just $ TicketPlaceResp place) (Just servicesResp)
        Nothing -> lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData Nothing Nothing
    else pure unit
--------------------------------------------------------------------------------------------

view :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
    PrestoAnim.animationSet [Anim.fadeIn true]  $ relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
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
        , serviceClosedView 
        , separatorView Color.greySmoke
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          ][  scrollView
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , background Color.white900
              , afterRender push $ const AfterRender
              , fillViewport true
              ]
              [ linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , gravity CENTER
                  , orientation VERTICAL
                  ]
                  (mainView state push)
            ] 
        ]]
    , actionsView state push
    ]
  where
  actionsView state push =
    case state.props.currentStage of
      ST.BookingConfirmationStage -> linearLayout [ visibility GONE ] []
      ST.TicketInfoStage -> linearLayout [ visibility GONE ] []
      ST.DescriptionStage -> generalActionButtons state push
      _ -> generalActionButtons state push
  allowFutureBooking services = foldl (\acc service -> acc || service.allowFutureBooking) false services

  serviceClosedView = if (state.props.currentStage == ST.ChooseTicketStage) && (not $ allowFutureBooking state.data.servicesInfo) && (placeClosed state.data.placeInfo) then (headerBannerView push state ("Booking closed currently. Opens after " <> getOpeningTiming state.data.placeInfo))
                      else if (state.props.currentStage == ST.ChooseTicketStage) && (allowFutureBooking state.data.servicesInfo) && (placeClosedToday state.data.placeInfo state.data.dateOfVisit) then (headerBannerView push state ("Services closed for today. Tickets are available next day onwards"))
                      else if (state.props.currentStage == ST.ChooseTicketStage) && (not $ allowFutureBooking state.data.servicesInfo) && (shouldHurry  state.data.placeInfo) then (headerBannerView push state ("Hurry! Booking closes at " <> getClosingTiming state.data.placeInfo))
                      else if (state.props.currentStage == ST.ChooseTicketStage) && (checkIfSameDayBookingNotAllowedForToday state) then (headerBannerView push state ("Same-day booking is not allowed. Please select a future date for ticket booking."))
                      else linearLayout [height $ V 0][]

  shouldHurry mbPlaceInfo =
    case mbPlaceInfo of
      Nothing -> false
      Just (TicketPlaceResp pInfo) -> case pInfo.closeTimings of
        Nothing -> false
        Just closeTime -> 
          let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
          in
          if currentTime < (convertUTCTimeToISTTimeinHHMMSS closeTime) then 
            case (getMinutesBetweenTwoUTChhmmss currentTime (convertUTCTimeToISTTimeinHHMMSS closeTime)) of
              Nothing -> false
              Just mins -> mins < 15
          else false

  getOpeningTiming mbPlaceInfo =
    case mbPlaceInfo of
      Nothing -> ""
      Just (TicketPlaceResp pInfo) -> case pInfo.openTimings of
        Nothing -> ""
        Just time ->
          let openTime = fromMaybe "" (convertUTCToISTAnd12HourFormat time)
          in (replace (Pattern "00:00") (Replacement "12:00") openTime)

  getClosingTiming mbPlaceInfo =
    case mbPlaceInfo of
      Nothing -> ""
      Just (TicketPlaceResp pInfo) -> case pInfo.closeTimings of
        Nothing -> ""
        Just time -> 
          let closeTime = fromMaybe "" (convertUTCToISTAnd12HourFormat time)
          in (replace (Pattern "00:00") (Replacement "12:00") closeTime)

  headerView state push =
    GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)

  mainView state push = 
    ([chooseTicketsView state push] <> 
    if (state.props.currentStage == ST.DescriptionStage) 
      then
        case state.data.placeInfo of
          Just placeInfo -> descriptionStateMainView state push placeInfo
          Nothing -> [ noDataView state push "No ticketing zones in this area" ]
    else [])

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
      , width $ MATCH_PARENT
      , height WRAP_CONTENT
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
    , background Color.transparent
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingBottom 16
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
         [ textFromHtml $ " &#8226;&ensp; " <> item
         , color Color.black700
         , height WRAP_CONTENT
         , width WRAP_CONTENT
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

serviceBreakUpView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) ->  Array ST.TicketServiceData -> TicketPlaceResp -> PrestoDOM (Effect Unit) w
serviceBreakUpView state push servicesv2 (TicketPlaceResp ticketPlaceResp) =
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
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][  textView $
                  [ text $ item.serviceName
                  , color Color.black800
                  , margin $ MarginVertical 10 10
                  ] <> FontStyle.subHeading1 TypoGraphy
                , linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ]( map (\cat -> 
                     let singleServiceCategory = (length item.serviceCategories) == 1
                         transformedServiceCatData = transformServiceCatData cat
                     in
                     linearLayout
                     [ width MATCH_PARENT
                     , height WRAP_CONTENT
                     , orientation VERTICAL
                     ][ linearLayout
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , orientation HORIZONTAL
                        , visibility $ if singleServiceCategory then GONE else VISIBLE
                        , margin $ MarginBottom 10
                        ][  imageView
                            [ width $ V 20
                            , height $ V 20
                            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_entry"
                            , margin $ MarginRight 5
                            ]
                          , textView $
                            [ text $ cat.categoryName <> " : "
                            , color $ Color.black800
                            ] <> FontStyle.body6 TypoGraphy
                        ]
                      , businessHoursView push state item.serviceName transformedServiceCatData (not singleServiceCategory)
                      , feesBreakUpView push state item.serviceName transformedServiceCatData (not singleServiceCategory)
                     ]
                  
                  ) item.serviceCategories)
              ]) 
      servicesv2)
    , termsAndConditionsView ticketPlaceResp.termsAndConditions false
  ]

  where
    transformServiceCatData category = 
      let bhData = map getBusinessHoursAndTimings category.operationalDays
          groupedData = groupBy (\a b -> a.val == b.val) $ sortBy (\item1 item2 -> compare item1.val item2.val ) bhData
          groupedAndSortedData = DA.concat $ map generateBHConcatenatedData groupedData
          finalSortedData = DA.sortBy (\a b -> compare (indexOf (DS.take 3 a.key) daysOrder) (indexOf (DS.take 3 b.key) daysOrder) ) groupedAndSortedData
      in {timings : finalSortedData, fees : map (getFeeForPeopleCat (length category.peopleCategories == 1)) category.peopleCategories }

    generateBHConcatenatedData nanarr = 
      let arr = DAN.toArray nanarr
      in [{key : concatKeys $ sortAccToDayName $ concatKeysArr arr, val : maybe "" (\obj -> obj.val) (arr DA.!! 0)}]

    concatKeysArr arr = DA.concat $ map (\obj -> obj.key) arr
    concatKeys arr = foldr (\obj acc -> if not DS.null acc then obj <> ", " <> acc else  obj ) "" arr 

    daysOrder = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    indexOf x xs = maybe 8 (\idx -> idx) (DA.findIndex (_ == x) xs)
    
    getFeeForPeopleCat isSinglePC peopleCat  = {key : getCategoryNameMap peopleCat.peopleCategoryName isSinglePC, val :  "₹" <>  show peopleCat.pricePerUnit }

    getCategoryNameMap catName isSinglePC = case catName, isSinglePC  of 
                                              "Adult", true         -> "Per Person"
                                              "Adult", false        -> "Visitors above the age of 5 years"
                                              "Kid", true           -> "Per Person"
                                              "Kid", false          -> "Up to the age of 5 years"
                                              "Cruise", _           -> "Per Person"
                                              "Passenger Vessel", _ -> "Per Person"
                                              _, _                  -> catName

    getBusinessHoursAndTimings slotTimeInterval = 
      let timeIntervalString = joinWith " , " (map getTimeIntervals slotTimeInterval.timeIntervals)
          slotIntervalString = joinWith " , " (map get12HoursFormat slotTimeInterval.slot)
          finalSlotTimeIntervalString = slotIntervalString <> (if (not DS.null slotIntervalString && not DS.null timeIntervalString) then (",") else "") <> timeIntervalString
      in { key : map (\x -> DS.take 3 x ) slotTimeInterval.operationalDays, 
           val : if DS.null finalSlotTimeIntervalString then "Closed" else finalSlotTimeIntervalString
         }

    get12HoursFormat slot = replace (Pattern "00:00") (Replacement "12:00") $ fromMaybe "" (convertUTCToISTAnd12HourFormat slot.slot)

    getTimeIntervals timeInterval = if DS.null timeInterval.startTime then 
                                      if not DS.null timeInterval.endTime then " till " <>  replace (Pattern "00:00") (Replacement "12:00") (fromMaybe "" (convertUTCToISTAnd12HourFormat timeInterval.endTime))
                                      else ""
                                    else replace (Pattern "00:00") (Replacement "12:00") (fromMaybe "" (convertUTCToISTAnd12HourFormat timeInterval.startTime))
                                         <> if not DS.null timeInterval.endTime then " to " <>  replace (Pattern "00:00") (Replacement "12:00") (fromMaybe "" (convertUTCToISTAnd12HourFormat timeInterval.endTime)) else " onwards"


businessHoursView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> ST.TiketingListTransformedData -> Boolean -> PrestoDOM (Effect Unit) w
businessHoursView push sate serviceName screenData paddingEnabled =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 10
  , padding $ if paddingEnabled then PaddingLeft 15 else PaddingLeft 0
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
      ][  textView $
          [ text "Timings"
          , color Color.black800
          , margin $ MarginBottom 5
          ] <> FontStyle.body6 TypoGraphy
        , linearLayout
          [ width $ MATCH_PARENT
          , height $ WRAP_CONTENT
          , orientation VERTICAL
          ] (map (\item ->  
                  textView $
                    [ textFromHtml $ "<b>" <> item.key <> " : " <> "</b>" <> item.val
                    , gravity LEFT
                    ] <>  FontStyle.paragraphText TypoGraphy
            ) screenData.timings)
        ]
  ]

feesBreakUpView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> ST.TiketingListTransformedData -> Boolean -> PrestoDOM (Effect Unit) w
feesBreakUpView push state serviceName screenData paddingEnabled =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 10
  , padding $ if paddingEnabled then PaddingLeft 15 else PaddingLeft 0
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
      ][  textView $ 
          [ text "Fees"
          , color Color.black800
          , margin $ MarginBottom 5
          ] <> FontStyle.body6 TypoGraphy
        , linearLayout
          [ width $ MATCH_PARENT
          , height $ WRAP_CONTENT
          , orientation VERTICAL
          ] (map (\item ->  
                    linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation HORIZONTAL
                    ][  textView $
                        [ textFromHtml $ "<b>" <> item.key <> " : " <> "</b>" <> item.val
                        , gravity LEFT
                        ] <> FontStyle.paragraphText TypoGraphy
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
  let filteredServiceDatav2  = filterServiceDataAccordingToOpDay state.props.selectedOperationalDay state.data.servicesInfo
      filteresServiceCatData = map (\service -> service { serviceCategories = (getFilteredServiceCategories state service.expiry service.serviceCategories) } ) state.data.servicesInfo
  in
  PrestoAnim.animationSet [Anim.fadeIn ( state.props.currentStage == ST.ChooseTicketStage)]  $  
  linearLayout[
    height MATCH_PARENT
  , width $ V $ screenWidth unit
  , background Color.grey700
  , padding $ Padding 16 24 16 16
  , visibility $ boolToVisibility $ state.props.currentStage == ST.ChooseTicketStage
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
            , height WRAP_CONTENT
            , width MATCH_PARENT
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
                      JB.datePicker (if isSameDayBookingAllowed state.data.placeInfo then "" else "MINIMUM_NEXT_DATE") push (DatePicker "DATE_OF_VISIT") ""
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
                , width MATCH_PARENT
                , height WRAP_CONTENT
                ] <> FontStyle.h3 TypoGraphy
            ]
          , textView $
            [ text $ getMessageForSelectedDate state -- Tickets are available for upto 90 days in advance
            , visibility if state.props.validDate || state.data.dateOfVisit == "" then GONE else VISIBLE
            , color Color.red 
            , margin $ MarginVertical 8 8
            ] <> FontStyle.tags TypoGraphy
        ]
      , if length filteredServiceDatav2 == 0 then (noDataView state push "No services available for selected date")
        else if (checkIfServiceClosedForToday filteredServiceDatav2) then (noDataView state push "Services closed for today. Tickets are available next day onwards") -- refactor this 
        else if checkIfSameDayBookingNotAllowedForToday state then (noDataView state push "Same-day booking is not allowed. Please select a future date for ticket booking.")
        else (linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ](map (serviceInputView push state) filteresServiceCatData))
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 20
        -- , gravity BOTTOM
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
                    _ <- JB.openUrlInApp $ getTermsAndConditionsUrlV2 state.data.placeInfo
                    pure unit
                    ) (const NoAction)
            ] <> FontStyle.body1 TypoGraphy
        ]
      , termsAndConditionsView (getTermsAndConditions state.data.placeInfo) true
  ]
  where
    checkIfDateOfTripVisible services = foldl (\acc service -> acc || service.allowFutureBooking) false services 
    getTermsAndConditions placeInfo = maybe [] (\(TicketPlaceResp x ) -> x.termsAndConditions) placeInfo
    getTermsAndConditionsUrlV2 placeInfo = maybe "" (\(TicketPlaceResp x ) -> case x.termsAndConditionsUrl of
      Just url -> url
      Nothing -> getTermsAndConditionsUrl placeInfo) placeInfo

    getTermsAndConditionsUrl placeInfo = maybe "" (\(TicketPlaceResp x ) -> case x.name of
                                                                              "Alipore Zoo" -> "https://docs.google.com/document/d/1Aa5PRGaTTZM4HDdmvU_7_59B58wCQ0-bRezbsu-Inqw"
                                                                              "Kolkata Heritage River Cruise" -> "https://docs.google.com/document/d/1pOirWof7bnNDoYBFgyKknddKPoKE-7SZK6eFPncAmyQ/edit#heading=h.nq90u3fvhtgz"
                                                                              "Millenium Park Shipping Jetty" -> "https://docs.google.com/document/d/1pOirWof7bnNDoYBFgyKknddKPoKE-7SZK6eFPncAmyQ/edit#heading=h.nq90u3fvhtgz"
                                                                              "Nicco Park" -> "https://assets.moving.tech/beckn/jatrisaathi/user/docs/T_&_C_Nicco_Park.pdf"
                                                                              _ -> ""
                                         ) placeInfo 
    getMessageForSelectedDate state = do
      if (getCurrentDatev2 "") < state.data.dateOfVisit then "Date Error! Booking allowed only upto " <> show (getLimitOfDaysAccToPlaceType state) <> " days in advance"
      else "Tickets are available current day onwards"

    checkIfServiceClosedForToday services = maybe true (\_ -> false) $ find (\service -> not DA.null (getFilteredServiceCategories state service.expiry service.serviceCategories)) services

serviceInputView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.TicketServiceData -> PrestoDOM (Effect Unit) w
serviceInputView push state service = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ] $ [] <>  [individualServiceView push state service]

individualServiceView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.TicketServiceData -> PrestoDOM (Effect Unit) w
individualServiceView push state service =
  let bookingClosedForService = (not state.props.validDate) || (not isValid service.serviceCategories)
      listOfShortDesc = maybe [] extractListItems service.shortDesc
      headingOfShortDesc =  service.shortDesc >>= extractHeading
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
      , onClick push $ const (ToggleTicketOption service.id)
      ][  linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , orientation VERTICAL
          ][ textView $
             [ text service.serviceName
             , color $ if bookingClosedForService then Color.black600 else Color.black800
             ] <> FontStyle.h2 TypoGraphy
          , textView $
             [  text $ fromMaybe "" headingOfShortDesc
              , padding $ PaddingVertical 4 4
              , visibility $ maybe GONE (\x -> VISIBLE) headingOfShortDesc
              , color $ if bookingClosedForService then Color.greyDark else Color.black800
             ] <> FontStyle.body2 TypoGraphy
          ,  linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , gravity CENTER_VERTICAL
              ](map (shortDescListView push bookingClosedForService) listOfShortDesc)
          ,  if length service.serviceCategories == 1 then
               case service.serviceCategories DA.!! 0 of
                 Nothing -> linearLayout [height $ V 0][]
                 Just val -> textView $
                              [ text $ val.categoryName
                              , color $ if bookingClosedForService then Color.greyDark else Color.black700
                              , visibility $ if val.categoryName == "all" then GONE else VISIBLE
                              ] <> FontStyle.body1 TypoGraphy
             else linearLayout [height $ V 0][]
          ]
        -- , linearLayout
        --   [weight 1.0][]
        , imageView 
          [ height $ V 20 
          , width $ V 20 
          , margin $ MarginLeft 10
          , imageWithFallback $ fetchImage FF_COMMON_ASSET if (service.isExpanded && not bookingClosedForService) then "ny_ic_checked" else "ny_ic_unchecked" 
          ]
      ]
  ] <> (if service.isExpanded && (not bookingClosedForService) then [individualServiceBHView push state service] else [] )--[individualTicketBHView push state valBH service] else [])
  where
    isValid serviceCategories = foldl (\acc serviceCategory -> acc || isServiceCatValid state service.expiry serviceCategory.validOpDay ) false serviceCategories
    extractHeading :: String -> Maybe String
    extractHeading html = do
      startIndex <- DS.indexOf (Pattern "<h") html
      endIndex <- DS.indexOf (Pattern "</h") html
      let startContentIndex = startIndex + DS.length "<h >"
      pure $ DS.take (endIndex - startContentIndex) (DS.drop startContentIndex html)

    extractListItems :: String -> Array String
    extractListItems html = DA.reverse $ extractListItemsHelper html []

    extractListItemsHelper :: String -> Array String -> Array String
    extractListItemsHelper html acc =
      case DS.indexOf (Pattern "<li>") html of
        Nothing -> acc
        Just startIndex -> 
          let 
            afterOpenTag = DS.drop (startIndex + 4) html 
          in
            case DS.indexOf (Pattern "</li>") afterOpenTag of
              Nothing -> acc
              Just endIndex ->
                let
                  content = DS.take endIndex afterOpenTag
                  trimmedContent = trim content
                  remainingHtml = DS.drop (endIndex + 5) afterOpenTag
                in
                  extractListItemsHelper remainingHtml (trimmedContent : acc)

    trim :: String -> String
    trim = trimStart >>> trimEnd

    trimStart :: String -> String
    trimStart str = 
      case DS.indexOf (Pattern " ") str of
        Just 0 -> trimStart (DS.drop 1 str)
        _ -> str

    trimEnd :: String -> String
    trimEnd str =
      let len = DS.length str
      in if len > 0 && (DS.take 1 (DS.drop (len - 1) str) == " ")
        then trimEnd (DS.take (len - 1) str)
        else str

shortDescListView :: forall w. (Action -> Effect Unit) -> Boolean -> String -> PrestoDOM (Effect Unit) w
shortDescListView push bookingClosedForService str =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 0 0 0 4
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ][
      dotView (Color.black800) (MarginHorizontal 6 6) 5,
      textView $ 
      [ textFromHtml str
      , color $ if bookingClosedForService then Color.greyDark else Color.black800
      , visibility $ boolToVisibility $ not $ str == ""
      ] <> FontStyle.body3 TypoGraphy
    ]

placeClosed :: Maybe TicketPlaceResp -> Boolean
placeClosed mbPlaceInfo = case mbPlaceInfo of
    Nothing -> false
    Just (TicketPlaceResp pInfo) ->
      let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
      in
      case pInfo.closeTimings, pInfo.openTimings of
        Just closeTime, Just startTime -> not $ (convertUTCTimeToISTTimeinHHMMSS startTime) <= currentTime && currentTime <= (convertUTCTimeToISTTimeinHHMMSS closeTime)
        _, _ -> false

placeClosedToday :: Maybe TicketPlaceResp -> String -> Boolean
placeClosedToday mbPlaceInfo dateOfVisit = case mbPlaceInfo of
    Nothing -> false
    Just (TicketPlaceResp pInfo) ->
      let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
          currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
      in
      (currentDate == dateOfVisit) && (maybe false (\closeTime -> currentTime > convertUTCTimeToISTTimeinHHMMSS closeTime) pInfo.closeTimings)

individualServiceBHView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> ST.TicketServiceData -> PrestoDOM (Effect Unit) w
individualServiceBHView push state service =
  let mbSelectedCategory = find (\elem -> elem.isSelected) service.serviceCategories
  in PrestoAnim.animationSet 
       [ if EHC.os == "ANDROID" 
         then Anim.translateInYAnim translateYAnimConfig { duration = 300 , fromY = 10 , toY = 0} 
         else Anim.fadeIn true
       ]
       $ linearLayout
           [ height WRAP_CONTENT
           , width MATCH_PARENT
           , orientation VERTICAL
           ] 
           $ [] <> if DA.length service.serviceCategories > 1 
                   then maybe [] (\selServiceCat -> [multipleServiceCategory push state service.id service.selectedBHId service.serviceCategories selServiceCat]) mbSelectedCategory
                   else maybe [] (\selServiceCat -> if shouldDisplayIncDscView selServiceCat.validOpDay service.selectedBHId
                                                   then map (incrementDecrementView push state service.id selServiceCat.categoryId) selServiceCat.peopleCategories
                                                   else []
                                  ) mbSelectedCategory
                   <> maybe [] (\selServiceCat -> [timeSlotView push state service.id selServiceCat.categoryId service.selectedBHId (getSlots selServiceCat.validOpDay)]) mbSelectedCategory
  where
    getSlots mbOpDay = maybe [] (\opDayElem -> opDayElem.slot) mbOpDay

multipleServiceCategory :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> Maybe String -> Array ST.ServiceCategory -> ST.ServiceCategory -> PrestoDOM (Effect Unit) w
multipleServiceCategory push state serviceId selectedBHId categories selectedCategory =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ] [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ] ( if (shouldDisplayIncDscView selectedCategory.validOpDay selectedBHId) then (map (incrementDecrementView push state serviceId selectedCategory.categoryId) selectedCategory.peopleCategories)
          else [] )
    , textView $
      [ text "Select your ticket category"
      , color Color.black800
      , margin $ MarginVertical 20 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ] (map (selectDestinationViewPill push state serviceId) categories)
  ]


shouldDisplayIncDscView :: Maybe ST.OperationalDaysData -> Maybe String -> Boolean
shouldDisplayIncDscView validOpDay selectedBHId =
  let slots = maybe [] (\opDayElem -> opDayElem.slot) validOpDay
  in (if DA.null slots then true  else maybe false (\_ -> true) selectedBHId)

timeSlotView :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> String -> Maybe String -> Array ST.SlotInterval -> PrestoDOM (Effect Unit) w
timeSlotView push state serviceId serviceCatId selectedBHId slots = 
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
      ] ( map (timeSlotRowView push serviceId serviceCatId selectedBHId) (convertTimeSlotsToGroupedArray filteredSlots))
  ]

timeSlotRowView :: forall w . (Action -> Effect Unit) -> String -> String -> Maybe String -> Array ST.SlotInterval -> PrestoDOM (Effect Unit) w
timeSlotRowView push serviceId serviceCatId selectedBHId slotArr =
  let len = DA.length slotArr
  in linearLayout
     [ width $ MATCH_PARENT
     , height MATCH_PARENT
     , orientation HORIZONTAL
     , margin $ MarginBottom 8
     ] $ [
     ] <> (case slotArr DA.!! 0 of
             Nothing -> []
             Just val -> [timeSlotPillView push serviceId serviceCatId selectedBHId val len true])
       <> (case slotArr DA.!! 1 of
             Nothing -> []
             Just val -> [timeSlotPillView push serviceId serviceCatId selectedBHId val len false])


timeSlotPillView :: forall w . (Action -> Effect Unit) -> String -> String -> Maybe String -> ST.SlotInterval -> Int -> Boolean  -> PrestoDOM (Effect Unit) w
timeSlotPillView push serviceId serviceCatId selectedBHid slotInterval len isfirst =
  linearLayout
  [ weight 1.0
  , height WRAP_CONTENT
  , stroke $ "1," <> (if selectedBHid == Just slotInterval.bhourId then Color.blue800 else Color.grey900)
  , background $ (if selectedBHid == Just slotInterval.bhourId then Color.blue600 else Color.white900)
  , padding $ Padding 16 8 16 8
  , gravity CENTER
  , onClick push (const $ SelectSlot serviceId serviceCatId slotInterval)
  , cornerRadius 8.0
  , margin $ if len == 1 then Margin 0 0 0 0 else if isfirst then MarginRight 5 else MarginLeft 5
  ][ textView $
     [ text $ replace (Pattern "00:00") (Replacement "12:00") $ fromMaybe "" $ convertUTCToISTAnd12HourFormat slotInterval.slot
     , width $ V 100
     , gravity CENTER
     , color $ (if selectedBHid == Just slotInterval.bhourId then Color.blue800 else Color.black700)
     , fontStyle $ FontStyle.bold LanguageStyle
     ]
  ]

selectDestinationViewPill :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> ST.ServiceCategory -> PrestoDOM (Effect Unit) w
selectDestinationViewPill push state serviceId category =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , onClick push (const $ SelectServiceCategory serviceId category)
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

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.TicketBookingScreenState-> String -> String -> ST.PeopleCategoriesData -> PrestoDOM (Effect Unit) w
incrementDecrementView push state  serviceId serviceCatId pcCategory  =
  let ticketLimit = getTicketIncrementLimit state
  in
  PrestoAnim.animationSet [
    Anim.translateInYAnim translateYAnimConfig { duration = 300 , fromY = -5, toY = 0}
  ] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  ][  textView $
      [ text $ (ticketInfoMap pcCategory.peopleCategoryName) <> " (₹" <> (show pcCategory.pricePerUnit) <> " per " <> (unitInfoMap pcCategory.peopleCategoryName) <> ")"
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
      , gravity CENTER_VERTICAL
      , stroke $ "1," <> Color.grey900
      ][  textView $
          [ background Color.grey700
          , text "-"
          , gravity CENTER
          , cornerRadius 4.0
          , width $ V 72
          , height $ V 36
          , padding $ Padding 28 1 28 7
          , onClick push $ const (DecrementTicket serviceId serviceCatId pcCategory ticketLimit)
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
          , width $ V 72
          , height $ V 36
          , onClick push $ const (IncrementTicket serviceId serviceCatId pcCategory ticketLimit)
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

  where  
    -- need to add this data at backend
    ticketInfoMap "CameraUnit" = "Devices"
    ticketInfoMap peopleCategoryName = peopleCategoryName <> " Ticket"

    unitInfoMap "Adult" = "person"
    unitInfoMap "Kid" = "person"
    unitInfoMap "CameraUnit" = "device"
    unitInfoMap _ = "unit"

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

getTicketImage :: String -> String
getTicketImage ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> fetchImage FF_ASSET "ny_ic_ticket"
  _ -> fetchImage FF_ASSET "ny_ic_ticket_black"
 
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
      ]([  textContentView (show item.numberOfUnits <> " " <> item.name) textColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
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

textContentView :: forall w. String -> String -> Margin -> (forall properties. (Array (Prop properties))) -> PrestoDOM (Effect Unit) w
textContentView textString textColor textMargin fontSt = 
  textView
  ([ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text textString
  , color textColor
  , margin textMargin
  , gravity CENTER
  ] <>  fontSt )


getSeparatorColor :: String -> String
getSeparatorColor ticketServiceName = case ticketServiceName of
  "Entrance Fee" -> Color.black700
  "Videography Fee" -> Color.white900
  "Aquarium Fee" -> Color.white900
  _ -> Color.white900


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

keyValueView :: (Action -> Effect Unit) -> ST.TicketBookingScreenState -> String -> String -> Int -> forall w . PrestoDOM (Effect Unit) w
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
        ][ if index == 1 then bookingForView state else 
           textView $ 
            [ text value
            , color Color.black800
            , onClick push $ const $ if key == "Booking ID" || key == "Transaction ID" then Copy value else NoAction -- needs refactoring
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
     [ text message
     , color Color.black900
     , padding $ Padding 16 10 16 12
     ] <> FontStyle.subHeading1 TypoGraphy
  ]

getTransactionConfig :: PP.PaymentStatus -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig status = 
  case status of
    PP.Success -> {image : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", statusTimeDesc : "Your ticket has been generated below", title : "Your booking is Confirmed!"}
    PP.Pending -> {image : fetchImage FF_COMMON_ASSET "ny_ic_transaction_pending", statusTimeDesc : "Please check back in a few minutes.", title : "Your booking is Pending!"}
    PP.Failed  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_payment_failed", statusTimeDesc : "Please retry booking.", title : "Booking Failed!"}
    PP.Scheduled  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_pending", statusTimeDesc : "", title : ""}

convertTimeSlotsToGroupedArray :: Array ST.SlotInterval -> Array (Array ST.SlotInterval)
convertTimeSlotsToGroupedArray slots = groupAdjacent slots

getTicketIncrementLimit :: ST.TicketBookingScreenState -> Int
getTicketIncrementLimit state = case state.data.placeInfo of
  Nothing -> 4
  Just (TicketPlaceResp pInfo) -> case pInfo.placeType of
    "HeritageSite" -> 4
    _ -> 100

filterServiceDataAccordingToOpDay :: String -> Array ST.TicketServiceData -> Array ST.TicketServiceData
filterServiceDataAccordingToOpDay selectedOpDay services = do
  DA.concat $ map modifyServiceData services
  where
    modifyServiceData :: ST.TicketServiceData -> Array ST.TicketServiceData
    modifyServiceData service = 
      let serviceCategoriesAccToSelOpDay =  DA.concat $ map modifySerivceCategories service.serviceCategories in
      if DA.null serviceCategoriesAccToSelOpDay then []
      else [service { serviceCategories = serviceCategoriesAccToSelOpDay}]

    modifySerivceCategories :: ST.ServiceCategory -> Array ST.ServiceCategory
    modifySerivceCategories serviceCategory = 
      let operationalDaysAccToSelOpDay = maybe [] (\elem -> [elem]) (find (\opDayElem -> selectedOpDay `elem` opDayElem.operationalDays) serviceCategory.operationalDays)
      in
      if DA.null operationalDaysAccToSelOpDay then []
      else [serviceCategory { operationalDays = operationalDaysAccToSelOpDay}]


getFilteredServiceCategories :: ST.TicketBookingScreenState -> ServiceExpiry -> Array ST.ServiceCategory -> Array ST.ServiceCategory
getFilteredServiceCategories state expiry serviceCategories =  filter (\serviceCat -> isServiceCatValid state expiry serviceCat.validOpDay ) serviceCategories

isServiceCatValid :: ST.TicketBookingScreenState -> ServiceExpiry -> Maybe ST.OperationalDaysData -> Boolean
isServiceCatValid state expiry mbValidOpDay = maybe false (\opday -> (not DA.null (getFilteredSlots opday.slot state)) || (getFilteredTime opday.timeIntervals)) mbValidOpDay
  where
    getFilteredTime timeIntervals =
      let now = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
          currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"
          selTimeInterval = timeIntervals DA.!! 0
      in
      if currentDate == state.data.dateOfVisit then
        case selTimeInterval of
          Nothing -> false
          Just sti ->
            let startTime = if not DS.null sti.startTime then convertUTCTimeToISTTimeinHHMMSS sti.startTime else ""
                endTime = if not DS.null  sti.endTime then convertUTCTimeToISTTimeinHHMMSS sti.endTime else ""
                newEndTime = if not DS.null endTime then case expiry of
                                  API.InstantExpiry val -> runFn3 incrOrDecrTimeFrom endTime val false
                                  _ -> endTime
                             else ""
            in
            if not DS.null startTime && not DS.null newEndTime then now > startTime && now < newEndTime
            else if not DS.null startTime then now > startTime
            else if not DS.null newEndTime then now < newEndTime
            else false
      else true

getFilteredSlots :: Array ST.SlotInterval -> ST.TicketBookingScreenState -> Array ST.SlotInterval
getFilteredSlots slots state =
      let currentDate = convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD" in
      if currentDate == (convertUTCtoISC state.data.dateOfVisit "YYYY-MM-DD") then do
        let currentTime = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
        filter (\slot -> (convertUTCTimeToISTTimeinHHMMSS slot.slot) > currentTime) slots
      else slots

checkIfSameDayBookingNotAllowedForToday :: ST.TicketBookingScreenState -> Boolean
checkIfSameDayBookingNotAllowedForToday state = 
      let isTodayDate = (getCurrentDatev2 "") == state.data.dateOfVisit
          allowSameDayBooking = isSameDayBookingAllowed state.data.placeInfo
      in
        not allowSameDayBooking && isTodayDate

isSameDayBookingAllowed placeInfo = case placeInfo of
      Just (TicketPlaceResp placeInfo) -> fromMaybe true placeInfo.allowSameDayBooking
      Nothing -> true