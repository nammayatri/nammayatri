module Screens.TicketBookingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, map, unit, min, max, bind, ($), not, (+), (-), (==), (*), (<>), show, void, (+), (==), (-), show, (&&), (>), (/=), (||), (<=), (>=), (<))
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (getDateAfterNDaysv2, compareDate, getCurrentDatev2)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (OperationalDaysData, PeopleCategoriesDataV22, FlattenedBusinessHourData, TicketServiceDataV22, ServiceCategoryV22, TimeInterval, TicketBookingScreenState, TicketBookingItem(..), HomeScreenState, SlotInterval(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR, incrOrDecrTimeFrom)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!), filter, elem, groupBy, find, sortBy, concat)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC, getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), BusinessHoursResp(..), TicketServiceResp(..), PeopleCategoriesResp(..), BookingStatus(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), PlaceType(..))
import Data.Int (ceil)
import Common.Types.App as Common
import Screens.TicketBookingScreen.ScreenData as TicketBookingScreenData
import Data.Function.Uncurried as Uncurried
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Data.Array.NonEmpty as DAN
import Data.Ord (compare)
import Services.API (ServiceExpiry(..))
import Data.Function.Uncurried (runFn3)
import Language.Strings (getString)
import Language.Types (STR(..))
import Accessor (_id, _categoryId, _name, _availableSeats, _allowedSeats, _bookedSeats, _peopleCategories)
import Data.Lens ((^.))
import Data.String(null)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit
    
data Action = AfterRender
            | UpdatePlacesData (Maybe TicketPlaceResp) (Maybe TicketServicesResponse)
            | GenericHeaderAC GenericHeader.Action 
            | PrimaryButtonAC PrimaryButton.Action
            | ShareTicketAC PrimaryButton.Action
            | ViewTicketAC PrimaryButton.Action
            | ToggleTicketOption String 
            | IncrementTicket String String PeopleCategoriesDataV22 Int
            | DecrementTicket String String PeopleCategoriesDataV22 Int
            | DatePicker String String Int Int Int
            | ToggleTermsAndConditions
            | OpenCalendar 
            | NoAction
            | BackPressed
            | GetBookingInfo String BookingStatus
            | TicketQRRendered String String
            | IncrementSliderIndex
            | DecrementSliderIndex
            | PaymentStatusAction String
            | GoHome
            | RefreshStatusAC PrimaryButton.Action
            | SelectServiceCategory String ServiceCategoryV22
            | SelectSlot String String SlotInterval
            | OpenGoogleMap (Maybe Number) (Maybe Number)
            | Copy String

data ScreenOutput = GoToHomeScreen TicketBookingScreenState
                  | GoToTicketPayment TicketBookingScreenState
                  | GoToGetBookingInfo TicketBookingScreenState BookingStatus
                  | TryAgain TicketBookingScreenState
                  | RefreshPaymentStatus TicketBookingScreenState
                  | BookTickets TicketBookingScreenState
                  | GoToOpenGoogleMaps TicketBookingScreenState Number Number

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState

eval AfterRender state = continue state

eval (ToggleTicketOption serviceId) state = do
  let updatedServicesInfo = map (updateExpandService serviceId) state.data.servicesInfoV2
      updatedAmount = calculateTotalAmount updatedServicesInfo
  continue state { data { servicesInfoV2 = updatedServicesInfo , totalAmount = updatedAmount } }

eval (IncrementTicket serviceId serviceCatId peopleCat limit) state = do
  if peopleCat.currentValue == limit then continue state { data {servicesInfoV2 = markLimitCrossed serviceId serviceCatId peopleCat true state.data.servicesInfoV2}}
  else do
    let {finalAmount, updateServicesInfo} = updateAndCalculateTotalServiceAmount serviceId serviceCatId peopleCat (Just true) state state.data.servicesInfoV2
    continue state { data { totalAmount = finalAmount, servicesInfoV2 = updateServicesInfo } }

eval (DecrementTicket serviceId serviceCatId peopleCat limit) state = do
  let {finalAmount, updateServicesInfo} = updateAndCalculateTotalServiceAmount serviceId serviceCatId peopleCat (Just false) state state.data.servicesInfoV2
      limitUpdatedServicesInfo = markLimitCrossed serviceId serviceCatId peopleCat false updateServicesInfo
  continue state { data { totalAmount = finalAmount, servicesInfoV2 = limitUpdatedServicesInfo } }

eval  (DatePicker _ resp year month date ) state = do
  case resp of 
    "SELECTED" -> do 
      let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if date < 10 then "0"  else "") <> (show date)
          validDate = (unsafePerformEffect $ runEffectFn2 compareDate (getDateAfterNDaysv2 (getLimitOfDaysAccToPlaceType state)) selectedDateString)
                      && (unsafePerformEffect $ runEffectFn2 compareDate selectedDateString (getCurrentDatev2 "" ))
          selectedOpDay = convertUTCtoISC ((show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month + 1)) <> "-" <> (show date)) "dddFull"
          modifiedServicesData = map (modifyService selectedOpDay) state.data.servicesInfoV2
          
      continue state { props{selectedOperationalDay = selectedOpDay, validDate = validDate },data { totalAmount = 0, servicesInfoV2 = modifiedServicesData, dateOfVisit = selectedDateString }}
    _ -> continue state
  where
    modifyService selectedOpDay service = 
      let modifiedServiceCategories = selectByDefaultOneServiceCategory (map (modifyServiceCategories selectedOpDay) service.serviceCategories) selectedOpDay
      in service { selectedBHId = Nothing, serviceCategories = modifiedServiceCategories}

    modifyServiceCategories selectedOpDay serviceCategory = serviceCategory { peopleCategories = map initPeopleCategories serviceCategory.peopleCategories, validOpDay = find (\opDay -> selectedOpDay `elem` opDay.operationalDays) serviceCategory.operationalDays}
    initPeopleCategories pc = pc {currentValue = 0}

eval ToggleTermsAndConditions state = continue state{props{termsAndConditionsSelected = not state.props.termsAndConditionsSelected}}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do 
  case state.props.currentStage of 
    DescriptionStage -> continue state{props{currentStage = ChooseTicketStage}}
    ChooseTicketStage -> updateAndExit state{props{previousStage = ChooseTicketStage}} $ GoToTicketPayment state{props{previousStage = ChooseTicketStage}}
    ViewTicketStage -> exit $ BookTickets state
    _ -> continue state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (UpdatePlacesData placeData Nothing) state = do
  let newState = state { data { placeInfo = placeData }, props { showShimmer = false } }
  continue newState

eval (UpdatePlacesData placeData (Just (TicketServicesResponse serviceData))) state = do
  let selectedOpDay = convertUTCtoISC (getCurrentUTC "") "dddFull"
      servicesInfo2 = mapWithIndex (\i it -> transformRespToStateDatav2 (i==0) it state selectedOpDay) serviceData
  continue state { data { placeInfo = placeData, servicesInfoV2 = servicesInfo2}, props {selectedOperationalDay = selectedOpDay, showShimmer = false } }

eval BackPressed state = do
  case state.props.currentStage of 
    DescriptionStage -> exit $ GoToHomeScreen state {props {currentStage = DescriptionStage}}
    ChooseTicketStage -> continue state{props{currentStage = if state.props.previousStage == ChooseTicketStage then DescriptionStage else state.props.previousStage}}
    ViewTicketStage -> exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}
    TicketInfoStage -> continue state{props{currentStage = ViewTicketStage}}
    BookingConfirmationStage -> if state.props.previousStage == ViewTicketStage then continue state {props{currentStage = state.props.previousStage}}
                                else exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}
    _ -> continue state

eval GoHome state = if state.props.previousStage == ViewTicketStage then continue state {props{currentStage = state.props.previousStage}}
                    else exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}

eval (GetBookingInfo bookingShortId bookingStatus) state = do
  let newState = state { props { selectedBookingId = bookingShortId } }
  updateAndExit newState $ GoToGetBookingInfo newState bookingStatus

eval (ViewTicketAC (PrimaryButton.OnClick)) state = 
  case state.props.paymentStatus of
   Common.Success -> continueWithCmd state [do pure (GetBookingInfo state.props.selectedBookingId Booked)]
   Common.Failed -> exit $ GoToHomeScreen state
   _ -> continue state

eval (PaymentStatusAction status) state =
  case status of 
    "Booked" -> continue state{props{paymentStatus = Common.Success}}
    "Failed" -> continue state{props{paymentStatus = Common.Failed}}
    _ -> continue state

eval (RefreshStatusAC (PrimaryButton.OnClick)) state = exit $ RefreshPaymentStatus state

eval (SelectServiceCategory serviceId selCategory) state =
  let modifiedServicesData = map modifyService state.data.servicesInfoV2
      finalAmount = calculateTotalAmount modifiedServicesData
  in continue state {data {totalAmount = finalAmount, servicesInfoV2 = modifiedServicesData}}
  where
    modifyService service = if service.id == serviceId then service {selectedBHId = Nothing, serviceCategories = map modifyServiceCategory service.serviceCategories} else service {selectedBHId = Nothing}
    modifyServiceCategory category = category { isSelected = selCategory.categoryId == category.categoryId}

eval (SelectSlot serviceId servcieCatId slot) state = 
  let modifiedServicesData = map modifyService state.data.servicesInfoV2 
      finalAmount = calculateTotalAmount modifiedServicesData
  in
  continue state {data {totalAmount = finalAmount, servicesInfoV2 = modifiedServicesData}}
  where
    modifyService service = if serviceId == service.id then service {selectedBHId = Just slot.bhourId } else service

eval (OpenGoogleMap lat long) state = do
  let dstLat = fromMaybe 0.0 lat
      dstLong = fromMaybe 0.0 long
  updateAndExit state $ GoToOpenGoogleMaps state dstLat dstLong

eval (Copy text) state = continueWithCmd state [ do 
    _ <- pure $ JB.copyToClipboard text
    _ <- pure $ JB.toast (getString COPIED)
    pure NoAction
  ]

eval _ state = continue state

transformRespToStateDatav2 :: Boolean -> TicketServiceResp -> TicketBookingScreenState -> String -> TicketServiceDataV22
transformRespToStateDatav2 isFirstElement (TicketServiceResp service) state selOpDay =
  let serviceCatData = transformBusinessHoursToServiceCategoriesData service.businessHours selOpDay
      modifiedSelectedServiceCategoriesData = selectByDefaultOneServiceCategory serviceCatData selOpDay
  in
  { id : service.id,
    serviceName : service.name,
    allowFutureBooking : service.allowFutureBooking,
    shortDesc : service.shortDesc,
    expiry : service.expiry,
    isExpanded : isFirstElement,
    serviceCategories : modifiedSelectedServiceCategoriesData,
    selectedBHId : Nothing
  }
  where
    transformBusinessHoursToServiceCategoriesData :: Array BusinessHoursResp -> String -> Array ServiceCategoryV22
    transformBusinessHoursToServiceCategoriesData respArr selOpDay =
      let flattenedBusinessHourData = flattenBusinessHourData respArr
          sortedServicesData = sortBy (\bh1 bh2-> compare (bh1.category ^. _id) (bh2.category ^. _id)) flattenedBusinessHourData
          groupedBusinessHoursAccordingToServiceCategories = groupBy (\bh1 bh2 -> (bh1.category ^. _id) == (bh2.category ^. _id)) sortedServicesData
          hello = spy "flattenedBusinessHourData" flattenedBusinessHourData
          hello2 = spy "groupedBusinessHoursAccordingToServiceCategories" groupedBusinessHoursAccordingToServiceCategories
      in concat $ map (generateServiceCategoryData selOpDay) groupedBusinessHoursAccordingToServiceCategories


    generateServiceCategoryData :: String -> DAN.NonEmptyArray FlattenedBusinessHourData -> Array ServiceCategoryV22
    generateServiceCategoryData selOpDay respNEArr = do
      let respArr = DAN.toArray respNEArr
          operationalDaysData = generateOperationalDaysData respArr
      case head respArr of
        Nothing -> []
        Just bhData ->
          [{  categoryId : bhData.category ^. _id,
              categoryName : bhData.category ^. _name,
              availableSeats : bhData.category ^. _availableSeats,
              allowedSeats : bhData.category ^. _allowedSeats,
              bookedSeats :  bhData.category ^. _bookedSeats,
              isSelected : false,
              peopleCategories : generatePeopleCategories (bhData.category ^. _peopleCategories),
              operationalDays : operationalDaysData,
              validOpDay : find (\opDay -> selOpDay `elem` opDay.operationalDays) operationalDaysData
          }]

    generateOperationalDaysData :: Array FlattenedBusinessHourData -> Array OperationalDaysData
    generateOperationalDaysData respArr =
      let sortedData = sortBy (\bh1 bh2 -> compare bh1.operationalDays bh2.operationalDays) respArr
          groupedData = groupBy (\bh1 bh2 -> bh1.operationalDays == bh2.operationalDays) sortedData
      in map generateSlotData groupedData

    generateSlotData :: DAN.NonEmptyArray FlattenedBusinessHourData -> OperationalDaysData
    generateSlotData bhs = 
      let headBH = DAN.head bhs
          businessHours = DAN.toArray bhs
      in
        { operationalDays : headBH.operationalDays,
          slot : if (isClosed headBH.specialDayType) then [] else filter (\x -> not null x.slot ) (map (\bh -> {bhourId : bh.id, slot : fromMaybe "" bh.slot }) businessHours ),
          timeIntervals : if (isClosed headBH.specialDayType) then [] else filter (\x -> not null x.startTime || not null x.endTime ) (map (\bh -> {bhourId : bh.id, startTime : fromMaybe "" bh.startTime, endTime : fromMaybe "" bh.endTime }) businessHours)
        }
      where
        isClosed specialDayType = maybe false (\val -> val == "Closed") specialDayType

    generatePeopleCategories :: Array PeopleCategoriesResp -> Array PeopleCategoriesDataV22
    generatePeopleCategories arr = map (\(PeopleCategoriesResp resp) -> 
          { peopleCategoryName : resp.name,
            pricePerUnit : ceil resp.pricePerUnit,
            currentValue : 0,
            peopleCategoryId : resp.id,
            ticketLimitCrossed : false
          }) arr

    flattenBusinessHourData :: Array BusinessHoursResp -> Array FlattenedBusinessHourData
    flattenBusinessHourData respArr = 
      concat
      ( map (\(BusinessHoursResp bh) ->
           map (\(TicketCategoriesResp cat) -> 
            { id : bh.id,
              slot : bh.slot,
              startTime : bh.startTime,
              endTime : bh.endTime,
              specialDayDescription : bh.specialDayDescription,
              specialDayType : bh.specialDayType,
              operationalDays : sortBy (\d1 d2 -> compare d1 d2) bh.operationalDays,
              category : (TicketCategoriesResp cat)
            }) bh.categories
        ) respArr
      )

    getCategoriesId :: Array TicketCategoriesResp -> Array String
    getCategoriesId categoryRespArr = map (\(TicketCategoriesResp x) -> x.id) categoryRespArr

selectByDefaultOneServiceCategory :: Array ServiceCategoryV22 -> String -> Array ServiceCategoryV22
selectByDefaultOneServiceCategory arr selOpDay = 
  case (find (\serviceCat -> length (filter (\opDay -> selOpDay `elem` opDay.operationalDays) serviceCat.operationalDays) > 0 ) arr) of
    Nothing -> map (\elem -> elem {isSelected = false}) arr
    Just serviceCat -> map (\elem -> if elem.categoryId == serviceCat.categoryId then elem {isSelected = true} else elem{isSelected = false}) arr

updateExpandService serviceId service = if service.id == serviceId then do toggleAndInitServiceIfNotExpanded service else service

toggleAndInitServiceIfNotExpanded :: TicketServiceDataV22 -> TicketServiceDataV22
toggleAndInitServiceIfNotExpanded service = if service.isExpanded then 
    service { isExpanded = false,
              serviceCategories = map initServiceCategories service.serviceCategories,
              selectedBHId = Nothing -- this will only be Just when slot are selected
            }
  else service {isExpanded = true}
  where
    initServiceCategories serviceCat = serviceCat {peopleCategories = map initPeopleCategories serviceCat.peopleCategories} --bh { categories = map (initCategories bh) bh.categories }
    initPeopleCategories pc = pc {currentValue = 0}

updateTicketAmount :: TicketBookingScreenState -> String -> Array TicketServiceDataV22 -> Int -> Int
updateTicketAmount state serviceId service actualAmount = sum (map calculatePrice service)
  where
  calculatePrice :: TicketServiceDataV22 -> Int
  calculatePrice service = sum (map (\serviceCat -> if serviceCat.isSelected then (sum (map calculateAmountPrice serviceCat.peopleCategories)) else 0) service.serviceCategories)

  calculateAmountPrice :: PeopleCategoriesDataV22 -> Int
  calculateAmountPrice pc = pc.currentValue * pc.pricePerUnit

updateAndCalculateTotalServiceAmount :: String -> String -> PeopleCategoriesDataV22 -> Maybe Boolean -> TicketBookingScreenState -> Array TicketServiceDataV22 -> {finalAmount :: Int, updateServicesInfo :: Array TicketServiceDataV22 }
updateAndCalculateTotalServiceAmount serviceId selServiceCategoryId selPeopleCat isIncrement state services = do
  let updatedServices = map updateService services
      finalAmount = calculateTotalAmount updatedServices
  
  {finalAmount: finalAmount, updateServicesInfo: updatedServices}
  where
    updateService :: TicketServiceDataV22 -> TicketServiceDataV22
    updateService service = if service.id == serviceId then service {serviceCategories = map updateServiceCategories service.serviceCategories} else service

    updateServiceCategories serviceCategory = if serviceCategory.categoryId == selServiceCategoryId 
                                                then serviceCategory {peopleCategories = map updatePeopleCategories serviceCategory.peopleCategories}
                                                else serviceCategory {peopleCategories = map initPeopleCategories serviceCategory.peopleCategories}
    initPeopleCategories pc = pc {currentValue = 0}

    updatePeopleCategories :: PeopleCategoriesDataV22 -> PeopleCategoriesDataV22
    updatePeopleCategories peopleCategory =
      if peopleCategory.peopleCategoryId == selPeopleCat.peopleCategoryId then
        peopleCategory {  currentValue = min 99 (max 0 ( case isIncrement of 
                                                  Nothing -> peopleCategory.currentValue
                                                  Just isInc -> if isInc then peopleCategory.currentValue + 1 else peopleCategory.currentValue - 1))
                        , ticketLimitCrossed = case isIncrement of
                                                  Nothing -> peopleCategory.ticketLimitCrossed
                                                  Just val -> if val then peopleCategory.ticketLimitCrossed else false
                        }
      else peopleCategory


calculateTotalAmount :: Array TicketServiceDataV22 -> Int
calculateTotalAmount service = sum (map calculateAmountAtServiceLevel service)
  where
    calculateAmountAtServiceLevel service = if service.isExpanded then sum(map calculateAmountAtServiceCatLevel service.serviceCategories) else 0
    calculateAmountAtServiceCatLevel serviceCategory = if serviceCategory.isSelected then sum (map calculatePCAmount serviceCategory.peopleCategories) else 0
    calculatePCAmount pc = pc.pricePerUnit * pc.currentValue

markLimitCrossed :: String -> String -> PeopleCategoriesDataV22 -> Boolean -> Array TicketServiceDataV22 -> Array TicketServiceDataV22
markLimitCrossed serviceId serviceCatId selPeopleCat value services = do
  map updateService services
  where
    updateService service = if serviceId == service.id then service {serviceCategories = map updateServiceCategories service.serviceCategories} else service
    updateServiceCategories serviceCat = if serviceCat.categoryId == serviceCatId then serviceCat { peopleCategories = map updatePeopleCat serviceCat.peopleCategories} else serviceCat
    updatePeopleCat peopleCategory = if selPeopleCat.peopleCategoryId == peopleCategory.peopleCategoryId then peopleCategory{ticketLimitCrossed = value} else peopleCategory

sum :: Array Int -> Int
sum = foldl (\acc x -> acc + x) 0

getLimitOfDaysAccToPlaceType :: TicketBookingScreenState -> Int
getLimitOfDaysAccToPlaceType state = case state.data.placeInfo of
  Nothing -> 7
  Just (TicketPlaceResp pInfo) -> case (getPlaceTypeFromString pInfo.placeType) of
    HeritageSite -> 7
    _ -> 90

getPlaceTypeFromString :: String -> PlaceType
getPlaceTypeFromString pType = case pType of
  "Museum" -> Museum
  "ThemePark" -> ThemePark
  "AmusementPark" -> AmusementPark
  "WaterPark" -> WaterPark
  "WildLifeSanctuary" -> WildLifeSanctuary
  "ArtGallery" -> ArtGallery
  "HeritageSite" -> HeritageSite
  "ReligiousSite" -> ReligiousSite
  _ -> Other
