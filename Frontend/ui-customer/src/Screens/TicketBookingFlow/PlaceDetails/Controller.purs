module Screens.TicketBookingFlow.PlaceDetails.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, map, unit, min, max, bind, ($), not, (+), (-), (==), (*), (<>), show, void, (+), (==), (-), show, (&&), (>), (/=), (||), (<=), (>=), (<))
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (getDateAfterNDaysv2, getCurrentDatev2)
import Engineering.Helpers.Utils(compareDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Helpers.Utils (emitTerminateApp, isParentView)
import Common.Types.App (LazyCheck(..))
import Screens.Types (OperationalDaysData, PeopleCategoriesData, FlattenedBusinessHourData, TicketServiceData, ServiceCategory, TimeInterval, TicketBookingScreenState, TicketBookingItem(..), HomeScreenState, SlotInterval(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR,isParentView,emitTerminateApp)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!), filter, elem, groupBy, find, sortBy, concat)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC, getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), BusinessHoursResp(..), TicketServiceResp(..), PeopleCategoriesResp(..), BookingStatus(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), PlaceType(..))
import Data.Int (ceil)
import Common.Types.App as Common
import Screens.TicketBookingFlow.PlaceDetails.ScreenData as TicketBookingScreenData
import Data.Function.Uncurried as Uncurried
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import JBridge as JB
import Services.API (ServiceExpiry(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.TicketBookingFlow.PlaceDetails.Transformer (selectByDefaultOneServiceCategory, transformRespToStateDatav2)
import Common.Types.App

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
            | ToggleTicketOption String 
            | IncrementTicket String String PeopleCategoriesData Int ServiceCategory
            | DecrementTicket String String PeopleCategoriesData Int ServiceCategory
            | DatePicker String String Int Int Int
            | ToggleTermsAndConditions
            | NoAction
            | BackPressed
            | GoHome
            | SelectServiceCategory String ServiceCategory
            | SelectSlot String String SlotInterval
            | OpenGoogleMap (Maybe Number) (Maybe Number)
            | Copy String

data ScreenOutput = GoToHomeScreen TicketBookingScreenState
                  | GoToTicketPayment TicketBookingScreenState
                  | BookTickets TicketBookingScreenState
                  | GoToOpenGoogleMaps TicketBookingScreenState Number Number
                  | GoToTicketBook TicketBookingScreenState String

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState

eval (ToggleTicketOption serviceId) state = do
  let updatedServicesInfo = map (updateExpandService serviceId) state.data.servicesInfo
      updatedAmount = calculateTotalAmount updatedServicesInfo
  continue state { data { servicesInfo = updatedServicesInfo , totalAmount = updatedAmount } }

eval (IncrementTicket serviceId serviceCatId peopleCat limit serviceCategory) state = do
  let totalSeatsBooked = foldl (\acc peopleCategory -> peopleCategory.currentValue + acc) 0 serviceCategory.peopleCategories

  if peopleCat.currentValue == limit then continue state { data {servicesInfo = markLimitCrossed serviceId serviceCatId peopleCat true state.data.servicesInfo}}
  else if maybe false (\availableSeats -> totalSeatsBooked >= availableSeats - serviceCategory.bookedSeats) serviceCategory.availableSeats then continue state { data { servicesInfo = showNoRemainingTicketsAvailable serviceId serviceCatId peopleCat true state.data.servicesInfo}}
  else do
    let {finalAmount, updateServicesInfo} = updateAndCalculateTotalServiceAmount serviceId serviceCatId peopleCat (Just true) state state.data.servicesInfo
    continue state { data { totalAmount = finalAmount, servicesInfo = updateServicesInfo } }

eval (DecrementTicket serviceId serviceCatId peopleCat limit serviceCategory) state = do
  let {finalAmount, updateServicesInfo} = updateAndCalculateTotalServiceAmount serviceId serviceCatId peopleCat (Just false) state state.data.servicesInfo
      limitUpdatedServicesInfo = markLimitCrossed serviceId serviceCatId peopleCat false updateServicesInfo
  continue state { data { totalAmount = finalAmount, servicesInfo = limitUpdatedServicesInfo } }

eval  (DatePicker _ resp year month date ) state = do
  case resp of 
    "SELECTED" -> do 
      let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if date < 10 then "0"  else "") <> (show date)
          validDate = (unsafePerformEffect $ runEffectFn2 compareDate (getDateAfterNDaysv2 (getLimitOfDaysAccToPlaceType state)) selectedDateString)
                      && (unsafePerformEffect $ runEffectFn2 compareDate selectedDateString (getCurrentDatev2 "" ))
          selectedOpDay = convertUTCtoISC ((show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month + 1)) <> "-" <> (show date)) "dddFull"
          modifiedServicesData = map (modifyService selectedOpDay) state.data.servicesInfo
          updatedState = state { props{selectedOperationalDay = selectedOpDay, validDate = validDate},data { totalAmount = 0, servicesInfo = modifiedServicesData, dateOfVisit = selectedDateString }}
      updateAndExit updatedState $ GoToTicketBook updatedState selectedDateString
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


eval (UpdatePlacesData placeData mbServiceData) state = do
  case mbServiceData of 
    Just (TicketServicesResponse serviceData) -> do
      let selectedOpDay = convertUTCtoISC (state.data.dateOfVisit) "dddFull"
          servicesInfo2 = mapWithIndex (\i it -> transformRespToStateDatav2 (i==0) it state selectedOpDay) serviceData
          
      continue state { data { placeInfo = placeData, servicesInfo = servicesInfo2}, props {selectedOperationalDay = selectedOpDay, showShimmer = false } }
    Nothing -> do
      let newState = state { data { placeInfo = placeData }, props { showShimmer = false } }
      continue newState
  

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
                    else if isParentView FunctionCall
                        then do
                            void $ pure $ emitTerminateApp Nothing true
                            continue state
                        else exit $ GoToHomeScreen state{props{currentStage = DescriptionStage, showShimmer = true}}

eval (SelectServiceCategory serviceId selCategory) state =
  let modifiedServicesData = map modifyService state.data.servicesInfo
      finalAmount = calculateTotalAmount modifiedServicesData
  in continue state {data {totalAmount = finalAmount, servicesInfo = modifiedServicesData}}
  where
    modifyService service = if service.id == serviceId then service {selectedBHId = Nothing, serviceCategories = map modifyServiceCategory service.serviceCategories} else service {selectedBHId = Nothing}
    modifyServiceCategory category = category { isSelected = selCategory.categoryId == category.categoryId}

eval (SelectSlot serviceId servcieCatId slot) state = 
  let modifiedServicesData = map modifyService state.data.servicesInfo 
      finalAmount = calculateTotalAmount modifiedServicesData
  in
  continue state {data {totalAmount = finalAmount, servicesInfo = modifiedServicesData}}
  where
    modifyService service = if serviceId == service.id then service {selectedBHId = Just slot.bhourId } else service

eval (OpenGoogleMap lat long) state = do
  let dstLat = fromMaybe 0.0 lat
      dstLong = fromMaybe 0.0 long
  updateAndExit state $ GoToOpenGoogleMaps state dstLat dstLong

eval (Copy text) state = continueWithCmd state [ do 
    void $ pure $ JB.copyToClipboard text
    void $ pure $ EHU.showToast (getString COPIED)
    pure NoAction
  ]

eval _ state = update state

updateExpandService serviceId service = if service.id == serviceId then do toggleAndInitServiceIfNotExpanded service else service

toggleAndInitServiceIfNotExpanded :: TicketServiceData -> TicketServiceData
toggleAndInitServiceIfNotExpanded service = if service.isExpanded then 
    service { isExpanded = false,
              serviceCategories = map initServiceCategories service.serviceCategories,
              selectedBHId = Nothing -- this will only be Just when slot are selected
            }
  else service {isExpanded = true}
  where
    initServiceCategories serviceCat = serviceCat {peopleCategories = map initPeopleCategories serviceCat.peopleCategories} --bh { categories = map (initCategories bh) bh.categories }
    initPeopleCategories pc = pc {currentValue = 0}

updateTicketAmount :: TicketBookingScreenState -> String -> Array TicketServiceData -> Int -> Int
updateTicketAmount state serviceId service actualAmount = sum (map calculatePrice service)
  where
  calculatePrice :: TicketServiceData -> Int
  calculatePrice service = sum (map (\serviceCat -> if serviceCat.isSelected then (sum (map calculateAmountPrice serviceCat.peopleCategories)) else 0) service.serviceCategories)

  calculateAmountPrice :: PeopleCategoriesData -> Int
  calculateAmountPrice pc = pc.currentValue * pc.pricePerUnit

updateAndCalculateTotalServiceAmount :: String -> String -> PeopleCategoriesData -> Maybe Boolean -> TicketBookingScreenState -> Array TicketServiceData -> {finalAmount :: Int, updateServicesInfo :: Array TicketServiceData }
updateAndCalculateTotalServiceAmount serviceId selServiceCategoryId selPeopleCat isIncrement state services = do
  let updatedServices = map updateService services
      finalAmount = calculateTotalAmount updatedServices
  
  {finalAmount: finalAmount, updateServicesInfo: updatedServices}
  where
    updateService :: TicketServiceData -> TicketServiceData
    updateService service = if service.id == serviceId then service {serviceCategories = map updateServiceCategories service.serviceCategories} else service

    updateServiceCategories serviceCategory = if serviceCategory.categoryId == selServiceCategoryId 
                                                then serviceCategory {peopleCategories = map updatePeopleCategories serviceCategory.peopleCategories}
                                                else serviceCategory {peopleCategories = map initPeopleCategories serviceCategory.peopleCategories}
    initPeopleCategories pc = pc {currentValue = 0}

    updatePeopleCategories :: PeopleCategoriesData -> PeopleCategoriesData
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


calculateTotalAmount :: Array TicketServiceData -> Int
calculateTotalAmount service = sum (map calculateAmountAtServiceLevel service)
  where
    calculateAmountAtServiceLevel service = if service.isExpanded then sum(map calculateAmountAtServiceCatLevel service.serviceCategories) else 0
    calculateAmountAtServiceCatLevel serviceCategory = if serviceCategory.isSelected then sum (map calculatePCAmount serviceCategory.peopleCategories) else 0
    calculatePCAmount pc = pc.pricePerUnit * pc.currentValue

markLimitCrossed :: String -> String -> PeopleCategoriesData -> Boolean -> Array TicketServiceData -> Array TicketServiceData
markLimitCrossed serviceId serviceCatId selPeopleCat value services = do
  map updateService services
  where
    updateService service = if serviceId == service.id then service {serviceCategories = map updateServiceCategories service.serviceCategories} else service
    updateServiceCategories serviceCat = if serviceCat.categoryId == serviceCatId then serviceCat { peopleCategories = map updatePeopleCat serviceCat.peopleCategories, noRemainingTicketAvailable = value} else serviceCat
    updatePeopleCat peopleCategory = if selPeopleCat.peopleCategoryId == peopleCategory.peopleCategoryId then peopleCategory{ticketLimitCrossed = value} else peopleCategory

showNoRemainingTicketsAvailable :: String -> String -> PeopleCategoriesData -> Boolean -> Array TicketServiceData -> Array TicketServiceData
showNoRemainingTicketsAvailable serviceId serviceCatId selPeopleCat value services = do
  map updateAvailableRemainingTicket services
  where
    updateAvailableRemainingTicket service = if serviceId == service.id then service {serviceCategories = map updateServiceCategories service.serviceCategories} else service
    updateServiceCategories serviceCat = if serviceCat.categoryId == serviceCatId then serviceCat { noRemainingTicketAvailable = value} else serviceCat

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
