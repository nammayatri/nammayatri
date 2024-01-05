module Screens.TicketBookingFlow.PlaceDetails.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, map, unit, min, max, bind, ($), not, (+), (-), (==), (*), (<>), show, void, (+), (==), (-), show, (&&), (>), (/=), (||), (<=), (>=), (<))
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState, TicketBookingScreenStage(..), TicketServiceI(..))
import Helpers.Utils (getDateAfterNDaysv2, compareDate, getCurrentDatev2)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Screens.Types (TimeInterval, TicketBookingScreenState, TicketBookingItem(..), HomeScreenState, TicketServiceData, PeopleCategoriesRespData, TicketPeopleCategoriesOptionData, PeopleCategoriesRespData, BusinessHoursData, TicketCategoriesData, TicketCategoriesData, TicketCategoriesOptionData, SlotsAndTimeIntervalData(..), SlotInterval(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, (:), foldl, mapWithIndex, head, (!!), filter, elem, groupBy, find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engineering.Helpers.Commons(convertUTCTimeToISTTimeinHHMMSS, getCurrentUTC, convertUTCtoISC, getNewIDWithTag)
import Resources.Constants
import Services.API (TicketPlaceResp(..), TicketServicesResponse(..), BusinessHoursResp(..), TicketServiceResp(..), PeopleCategoriesResp(..), BookingStatus(..), PeopleCategoriesResp(..), TicketCategoriesResp(..), PlaceType(..))
import Data.Int (ceil)
import Common.Types.App as Common
import Screens.TicketBookingFlow.PlaceDetails.ScreenData as TicketBookingScreenData
import Data.Function.Uncurried as Uncurried
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Services.API (ServiceExpiry(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.TicketBookingFlow.PlaceDetails.Transformer (transformRespToStateData, getValidBHid, getValidTimeIntervals)

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
            | IncrementTicket TicketPeopleCategoriesOptionData Int
            | DecrementTicket TicketPeopleCategoriesOptionData Int
            | DatePicker String String Int Int Int
            | ToggleTermsAndConditions
            | NoAction
            | BackPressed
            | GoHome
            | SelectDestination TicketCategoriesOptionData
            | SelectSlot String SlotInterval
            | OpenGoogleMap (Maybe Number) (Maybe Number)
            | Copy String

data ScreenOutput = GoToHomeScreen TicketBookingScreenState
                  | GoToTicketPayment TicketBookingScreenState
                  | BookTickets TicketBookingScreenState
                  | GoToOpenGoogleMaps TicketBookingScreenState Number Number

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState

eval (ToggleTicketOption ticketID) state = do
  let updatedServicesInfo = map (updateExpandService ticketID) state.data.servicesInfo
      updatedAmount = updateTicketAmount state ticketID updatedServicesInfo state.data.totalAmount
  continue state { data { servicesInfo = updatedServicesInfo, totalAmount = updatedAmount } }

eval (IncrementTicket peopleCat limit) state = do
  if peopleCat.currentValue == limit then do
    let updatedData = markLimitCrossed peopleCat true state.data.servicesInfo
    continue state { data {servicesInfo = markLimitCrossed peopleCat true state.data.servicesInfo}}
  else do
    let {finalAmount, updateServicesInfo} = calculateTicketAmount peopleCat (Just true) state state.data.servicesInfo
    continue state { data { totalAmount = finalAmount, servicesInfo = updateServicesInfo } }

eval (DecrementTicket peopleCat limit) state = do
  let {finalAmount, updateServicesInfo} = calculateTicketAmount peopleCat (Just false) state state.data.servicesInfo
      limitUpdatedServicesInfo = markLimitCrossed peopleCat false updateServicesInfo
  continue state { data { totalAmount = finalAmount, servicesInfo = limitUpdatedServicesInfo } }

eval  (DatePicker _ resp year month date ) state = do
  case resp of 
    "SELECTED" -> do 
      let selectedDateString = (show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month+1)) <> "-" <> (if date < 10 then "0"  else "") <> (show date)
          validDate = (unsafePerformEffect $ runEffectFn2 compareDate (getDateAfterNDaysv2 (getLimitOfDaysAccToPlaceType state)) selectedDateString)
                      && (unsafePerformEffect $ runEffectFn2 compareDate selectedDateString (getCurrentDatev2 "" ))
          modifiedServicesData = map (modifyServicSelectedBHid selectedOpDay selectedDateString) state.data.servicesInfo
          selectedOpDay = convertUTCtoISC ((show year) <> "-" <> (if (month + 1 < 10) then "0" else "") <> (show (month + 1)) <> "-" <> (show date)) "dddFull"
      continue state { props{selectedOperationalDay = selectedOpDay, validDate = validDate },data { totalAmount = 0, servicesInfo = modifiedServicesData, dateOfVisit = selectedDateString }}
    _ -> continue state
  where
    modifyServicSelectedBHid selectedOpDay selectedDateString service = service {businessHours = map initBusinessHours service.businessHours, selectedSlot = Nothing, selectedBHid = getValidBHid (getValidTimeIntervals service.timeIntervalData selectedOpDay) selectedOpDay selectedDateString state service.expiry}
    initBusinessHours bh = bh { categories = map (initCategories bh) bh.categories }
    initCategories bh cat = cat {isSelected = length bh.categories <= 1, peopleCategories = map initPeopleCategories cat.peopleCategories}
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
      servicesInfo = mapWithIndex (\i it -> transformRespToStateData (i==0) it state selectedOpDay) serviceData
  continue state { data { placeInfo = placeData, servicesInfo = servicesInfo}, props {selectedOperationalDay = selectedOpDay, showShimmer = false } }

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

eval (SelectDestination selCategory) state = do
  let modifiedServicesData = map (modifyDestinationInfo) state.data.servicesInfo
      finalAmount = calculateTotalAmount state modifiedServicesData
  continue state {data {totalAmount = finalAmount, servicesInfo = modifiedServicesData}}
  where
    modifyDestinationInfo service = if service.id == selCategory.ticketID then service {businessHours = map (modifyBH service.selectedBHid) service.businessHours} else service
    modifyBH selectedBHid bh = if (Just bh.bhourId == selectedBHid) then bh { categories = map modifyCategory bh.categories} else bh
    modifyCategory category = category { isSelected = selCategory.categoryId == category.categoryId}

eval (SelectSlot ticketId slot) state = do
  let modifiedServicesData = map (modifySlotInfo) state.data.servicesInfo 
      finalAmount = calculateTotalAmount state modifiedServicesData
  continue state {data {totalAmount = finalAmount, servicesInfo = modifiedServicesData}}
  where
    modifySlotInfo service =  if service.id == ticketId then service { selectedSlot = Just slot.slot, selectedBHid = Just slot.bhourId, businessHours = map (modifyBH service.selectedBHid) service.businessHours} else service
    modifyBH selectedBHid bh =  bh {categories = map (initCategories bh) bh.categories} 
    initCategories bh cat = cat {isSelected = length bh.categories <= 1, peopleCategories = map initPeopleCategories cat.peopleCategories}
    initPeopleCategories pc = pc {currentValue = 0}

eval (OpenGoogleMap lat long) state = do
  let dstLat = fromMaybe 0.0 lat
      dstLong = fromMaybe 0.0 long
  updateAndExit state $ GoToOpenGoogleMaps state dstLat dstLong

eval (Copy text) state = continueWithCmd state [ do 
    void $ pure $ JB.copyToClipboard text
    void $ pure $ JB.toast (getString COPIED)
    pure NoAction
  ]

eval _ state = continue state

updateExpandService ticketID service =
  if service.id == ticketID then do toggleAndInitServiceIfNotExpanded service
  else service

toggleAndInitServiceIfNotExpanded :: TicketServiceData -> TicketServiceData
toggleAndInitServiceIfNotExpanded service = if service.isExpanded then 
    service { isExpanded = false,
              businessHours = map initBusinessHours service.businessHours
            }
  else service {isExpanded = true}
  where
    initBusinessHours bh = bh { categories = map (initCategories bh) bh.categories }
    initCategories bh cat = cat {isSelected = length bh.categories <= 1, peopleCategories = map initPeopleCategories cat.peopleCategories}
    initPeopleCategories pc = pc {currentValue = 0}

updateTicketAmount :: TicketBookingScreenState -> String -> Array TicketServiceData -> Int -> Int
updateTicketAmount state ticketID servicesInfo actualAmount = sum (map calculatePrice servicesInfo)
  where
  calculateAmountPrice :: PeopleCategoriesRespData -> Int
  calculateAmountPrice price = price.currentValue * price.pricePerUnit

  calculatePrice :: TicketServiceData -> Int
  calculatePrice service = sum (map (\bh -> do
                                            let selOpDay = convertUTCtoISC state.data.dateOfVisit "dddFull" 
                                            if (Just bh.bhourId == service.selectedBHid && bh.slot == service.selectedSlot && (selOpDay `elem` bh.operationalDays))
                                            then (sum (map (\cat -> if cat.isSelected then (sum (map calculateAmountPrice cat.peopleCategories)) else 0) bh.categories))
                                            else 0
                                ) service.businessHours)

calculateTicketAmount :: TicketPeopleCategoriesOptionData -> Maybe Boolean -> TicketBookingScreenState -> Array TicketServiceData -> {finalAmount :: Int, updateServicesInfo :: Array TicketServiceData }
calculateTicketAmount ticketOption isIncrement state servicesInfo = do
  let updatedServices = map (updateService ticketOption) servicesInfo
      finalAmount = sum (map calculateAmountService updatedServices)
  
  {finalAmount: finalAmount, updateServicesInfo: updatedServices}
  where
    updateService :: TicketPeopleCategoriesOptionData -> TicketServiceData -> TicketServiceData
    updateService ticketOption service =
      if service.id == ticketOption.ticketID then do
        service {businessHours = map (modifyBH ticketOption service) service.businessHours}
      else service

    modifyBH ticketOption service bh = if (isValidBH service bh) then bh { categories = map (modifyCategory ticketOption) bh.categories} else bh{ categories = map (initCategories bh) bh.categories} 
    modifyCategory ticketOption cat = cat { peopleCategories = map (updatePricePerUnit ticketOption) cat.peopleCategories}
    initCategories bh cat = cat {isSelected = length bh.categories <= 1, peopleCategories = map initPeopleCategories cat.peopleCategories}
    initPeopleCategories pc = pc {currentValue = 0}

    isValidBH service bh = do
      let selOpDay = convertUTCtoISC state.data.dateOfVisit "dddFull" 
      (Just bh.bhourId == service.selectedBHid && bh.slot == service.selectedSlot && (selOpDay `elem` bh.operationalDays))

    updatePricePerUnit :: TicketPeopleCategoriesOptionData -> PeopleCategoriesRespData -> PeopleCategoriesRespData
    updatePricePerUnit ticketOption price =
      if price.peopleCategoryId == ticketOption.peopleCategoryId then do
        price { currentValue = min 99 (max 0 ( case isIncrement of 
                                                  Nothing -> price.currentValue
                                                  Just isInc -> if isInc then price.currentValue + 1 else price.currentValue - 1))
              , ticketLimitCrossed = case isIncrement of
                                        Nothing -> price.ticketLimitCrossed
                                        Just val -> if val then price.ticketLimitCrossed else false
              }
      else price

    calculateAmountService :: TicketServiceData -> Int
    calculateAmountService service =
      sum (map (calculateAmountPriceBH service.isExpanded service.selectedBHid) service.businessHours)

    calculateAmountPriceBH :: Boolean ->  Maybe String -> BusinessHoursData -> Int
    calculateAmountPriceBH include selectedBHId bh = if (include && (Just bh.bhourId == selectedBHId)) then sum ( map calculateAmountPriceCatLevel bh.categories) else 0

    calculateAmountPriceCatLevel :: TicketCategoriesData -> Int
    calculateAmountPriceCatLevel category =  if category.isSelected then sum (map (\pc -> pc.pricePerUnit * pc.currentValue) category.peopleCategories) else 0

markLimitCrossed :: TicketPeopleCategoriesOptionData -> Boolean -> Array TicketServiceData -> Array TicketServiceData
markLimitCrossed ticketOption value servicesInfo = do
  map (updateService ticketOption value) servicesInfo
  where
    updateService ticketOption value service = if (ticketOption.ticketID == service.id) then service {businessHours = map (modifyBH ticketOption service.selectedBHid service.selectedSlot value) service.businessHours} else service
    modifyBH ticketOption selectedBHid selectedSlot value bh = if (Just bh.bhourId == selectedBHid  && bh.slot == selectedSlot) then bh { categories = map (modifyCategory ticketOption value) bh.categories} else bh 
    modifyCategory ticketOption value cat =  cat { peopleCategories = map (modifyPC ticketOption value) cat.peopleCategories}
    modifyPC ticketOption value pc = if ticketOption.peopleCategoryId == pc.peopleCategoryId then pc { ticketLimitCrossed = value} else pc

sum :: Array Int -> Int
sum = foldl (\acc x -> acc + x) 0

calculateTotalAmount :: TicketBookingScreenState -> Array TicketServiceData -> Int
calculateTotalAmount state services = sum $ map (calculateServiceAmount state) services

calculateServiceAmount :: TicketBookingScreenState -> TicketServiceData -> Int
calculateServiceAmount state service = if service.isExpanded then sum (map (calculateBHAmount state service) service.businessHours) else 0

calculateBHAmount :: TicketBookingScreenState -> TicketServiceData -> BusinessHoursData -> Int
calculateBHAmount state service bh = do
  let selOpDay = convertUTCtoISC state.data.dateOfVisit "dddFull" 
  if (Just bh.bhourId == service.selectedBHid && bh.slot == service.selectedSlot && (selOpDay `elem` bh.operationalDays)) then (sum (map (calculateCategoryAmount) bh.categories)) else 0

calculateCategoryAmount :: TicketCategoriesData -> Int
calculateCategoryAmount category = if category.isSelected then  sum ( map (\peopleCat -> peopleCat.pricePerUnit * peopleCat.currentValue) category.peopleCategories ) else 0

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
