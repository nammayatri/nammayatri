module Screens.CustomerUtils.Flow where

import Control.Monad.Except.Trans (lift)
import Data.Array ((!!), length, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, toLower, trim)
import Debug (spy)
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow, getNewIDWithTag, getExpiryTime)
import Helpers.Utils (decodeErrorCode, differenceOfLocationLists, filterRecentSearches, setText', seperateByWhiteSpaces, checkPrediction, toString)
import JBridge (firebaseLogEventWithParams, hideKeyboardOnNavigation, loaderText, reallocateMapFragment, toast, toggleBtnLoader, toggleLoader)
import Language.Strings (getString)
import Language.Types (STR(..))
import ModifyScreenState (modifyScreenState, updateRideDetails)
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||))
import Services.API (AddressGeometry(..), BookingLocationAPIEntity(..), ConfirmRes(..), DeleteSavedLocationReq(..), Geometry(..), GetDriverLocationResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), TriggerOTPResp(..), VerifyTokenResp(..), UserSosRes(..),  GetEmergContactsReq(..), GetEmergContactsResp(..), ContactDetails(..), FlowStatusRes(..), FlowStatus(..), CancelEstimateRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..))
import Effect (Effect)
import Foreign.Class (class Encode)
import Screens.Types (EmailErrorType(..), DeleteStatus(..), LocationItemType(..), AddNewAddressScreenState(..), LocationListItemState)

import Screens.HomeScreen.Transformer (getLocationList, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList)
import Screens.HomeScreen.ScreenData as HomeScreenData

import Screens.AboutUsScreen.Handler (aboutUsScreen)

import Screens.SelectLanguageScreen.Handler (selectLanguageScreen)
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData

import Screens.AddNewAddressScreen.Handler (addNewAddressScreen)
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getLocationList, calculateDistance, isValidLocation) as AddNewAddress

import Screens.HelpAndSupportScreen.Handler (helpAndSupportScreen)

import Screens.InvoiceScreen.Handler (invoiceScreen)
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput

import Screens.MyProfileScreen.Handler (myProfileScreen)
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData

import Components.SettingSideBar.Controller as SettingSideBarController

import Screens.EmergencyContactsScreen.Handler (emergencyContactsScreen)

import Screens.ContactUsScreen.Handler (contactUsScreen)

import Screens.TripDetailsScreen.Handler (tripDetailsScreen)

import Screens.MyRidesScreen.Handler (myRidesScreen)

import Components.LocationListItem.Controller (dummyLocationListState)

import Flow (homeScreenFlow, getGenderValue, rideSearchFlow, savedLocationFlow)

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
  flow <- aboutUsScreen
  case flow of
    GO_TO_HOME_FROM_ABOUT -> homeScreenFlow

selectLanguageScreenFlow :: FlowBT String Unit
selectLanguageScreenFlow = do
  flow <- selectLanguageScreen
  case flow of
    UPDATE_LANGUAGE state -> do
                                setValueToLocalStore LANGUAGE_KEY (state.props.selectedLanguage)
                                _ <- lift $ lift $ liftFlow $(firebaseLogEventWithParams "ny_user_lang_selec" "language" (state.props.selectedLanguage))
                                resp <- lift $ lift $ Remote.updateProfile (Remote.makeUpdateLanguageRequest "")
                                modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> SelectLanguageScreenData.initData)
                                homeScreenFlow
    GO_TO_HOME_SCREEN     -> homeScreenFlow

helpAndSupportScreenFlow :: FlowBT String Unit
helpAndSupportScreenFlow = do
  flow <- helpAndSupportScreen
  case flow of
    GO_TO_HOME_FROM_HELP -> homeScreenFlow
    GO_TO_SUPPORT_SCREEN bookingId'-> do
      modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen {data{bookingId = bookingId'}})
      contactUsScreenFlow
    GO_TO_TRIP_DETAILS state -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {tripId = state.data.tripId, selectedItem {date = state.data.date, bookingId = state.data.bookingId,rideStartTime = state.data.rideStartTime, rideEndTime = state.data.rideEndTime, rideId = state.data.rideId, vehicleNumber = state.data.vehicleNumber,time = state.data.time,source = state.data.source,destination = state.data.destination,driverName = state.data.driverName,totalAmount = state.data.totalAmount, rating = state.data.rating},date = state.data.date, time = state.data.time, source = state.data.source, destination = state.data.destination, driverName = state.data.driverName, totalAmount = state.data.totalAmount,rating = state.data.rating}})
      tripDetailsScreenFlow false
    VIEW_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen { data{offsetValue = 0}})
      myRidesScreenFlow false
    UPDATE_STATE updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      helpAndSupportScreenFlow

invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
  flow <- invoiceScreen
  (GlobalState newState) <- getState
  case flow of
    InvoiceScreenOutput.GoBack -> tripDetailsScreenFlow newState.tripDetailsScreen.props.fromMyRides
    InvoiceScreenOutput.GoToHome -> homeScreenFlow

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
  flow <- myProfileScreen
  case flow of
    UPDATE_USER_PROFILE state -> do
      _ <- pure $ toggleBtnLoader "" false
      _ <- pure $ spy "profile_updated_state" state
      let stringName = seperateByWhiteSpaces(state.data.editedName)
          name = split (Pattern " ") stringName
          nameLength = length name
          gender = getGenderValue state.data.editedGender
          email = if state.data.editedEmailId == state.data.emailId || (state.data.editedEmailId == Just "") then Nothing else state.data.editedEmailId
      resp <- if nameLength > 2 then
                lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (name !! 1) (name !! (nameLength - 1)) (email) gender)
                else if nameLength == 2 then
                  lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (name !! 1) (email) gender)
                  else if nameLength == 1 then
                    lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (Just "") (email) gender)
                    else
                      lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (Just "") (Just "") (Just "") (email) gender)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME stringName
          case gender of
            Just gender -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just gender}}})
            _ -> pure unit
          case email of
            Just email -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just email}}})
            _ -> pure unit
          modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData)
          myProfileScreenFlow
        Left err -> do
          let errResponse = err.response
          let codeMessage = decodeErrorCode errResponse.errorMessage
          case codeMessage of
            "PERSON_EMAIL_ALREADY_EXISTS" -> do
              _ <- lift $ lift $ liftFlow (setText' (getNewIDWithTag "EmailEditText") "" )
              modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{props{isEmailValid = false, updateProfile = true}, data{errorMessage = Just EMAIL_EXISTS, name = state.data.name, editedName = state.data.editedName, emailId = state.data.emailId, gender = state.data.gender, editedGender = state.data.editedGender}})
            _ -> pure $ toast (getString ERROR_OCCURED)
          myProfileScreenFlow
      myProfileScreenFlow
    DELETE_ACCOUNT updatedState -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  (Just "nammayatri.support@juspay.in") Nothing "Request To Delete Account" ("Delete account for " <> (getValueToLocalStore MOBILE_NUMBER) <> " , name " <> updatedState.data.name) )
      modifyScreenState $ MyProfileScreenStateType (\myProfileScreen -> myProfileScreen{props{accountStatus = DEL_REQUESTED}})
      myProfileScreenFlow
    GO_TO_HOME_ -> do
      modifyScreenState $ MyProfileScreenStateType (\myProfileScreen -> myProfileScreen{props{accountStatus = ACTIVE}})
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow

emergencyScreenFlow :: FlowBT String Unit
emergencyScreenFlow = do
  flow <- emergencyContactsScreen
  case flow of
    GO_TO_HOME_FROM_EMERGENCY_CONTACTS -> homeScreenFlow
    POST_CONTACTS state -> do
      _ <- Remote.emergencyContactsBT (Remote.postContactsReq state.data.contactsList)
      (GlobalState globalState) <- getState
      if globalState.homeScreen.props.emergencyHelpModelState.isSelectEmergencyContact
      then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{emergencyHelpModelState{isSelectEmergencyContact = false, emergencyContactData = transformContactList state.data.contactsList}}})
        homeScreenFlow
      else emergencyScreenFlow
    GET_CONTACTS state -> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true
        }) res.defaultEmergencyNumbers
      contactsInString <- pure $ toString contacts
      _ <- pure $ setValueToLocalStore CONTACTS (contactsInString)
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state{data{contactsList = contacts}})
      emergencyScreenFlow

addNewAddressScreenFlow ::String -> FlowBT String Unit
addNewAddressScreenFlow input = do
  (GlobalState newState) <- getState
  flow <- addNewAddressScreen
  case flow of
    SEARCH_ADDRESS input state -> do
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input ( newState.homeScreen.props.sourceLat) ( newState.homeScreen.props.sourceLong) 50000 (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                                                                                                                                "HI_IN" -> "HINDI"
                                                                                                                                                                                                                                "KN_IN" -> "KANNADA"
                                                                                                                                                                                                                                _      -> "ENGLISH") "")
      let predictionList = AddNewAddress.getLocationList searchLocationResp.predictions
          recentLists = state.data.recentSearchs.predictionArray
          filteredRecentsList = filterRecentSearches recentLists predictionList
          filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList

      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> state{  data  { locationList = map
                                                                                                                (\item -> item{ postfixImageVisibility = (not (checkPrediction item state.data.savedLocations))
                                                                                                                              , postfixImageUrl = if (checkPrediction item state.data.savedLocations) then "" else "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
                                                                                                                              , isClickable = (checkPrediction item state.data.savedLocations)
                                                                                                                              , alpha = if (checkPrediction item state.data.savedLocations) then 1.0 else 0.5 }) (filteredPredictionList <> filteredRecentsList) }})
      addNewAddressScreenFlow ""

    ADD_LOCATION state -> do
      if (state.props.editSavedLocation) then do
        _ <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim state.data.placeName))
        pure unit
      else pure unit
      (GetPlaceNameResp sourcePlace) <- getPlaceNameResp (state.data.selectedItem.placeId) (fromMaybe 0.0 state.data.selectedItem.lat) (fromMaybe 0.0 state.data.selectedItem.lon)  state.data.selectedItem
      let source = state.data.selectedItem.description
          (PlaceName sourceAddressGeometry) = (fromMaybe HomeScreenData.dummyLocationName (sourcePlace!!0))
          (LatLong sourceLocation) = (sourceAddressGeometry.location)
          lat = sourceLocation.lat
          lng = sourceLocation.lon
          newstate = state { data { lat =lat, lon=lng, selectedItem
                                                        { description = source
                                                        , lat = Just lat
                                                        , lon = Just lng
                                                        }
                                    , addressComponents = sourceAddressGeometry.addressComponents
                                    }
                              }

      resp <- Remote.addSavedLocationBT (AddNewAddress.encodeAddressDescription newstate)
      if state.props.editSavedLocation then pure $ toast (getString FAVOURITE_UPDATED_SUCCESSFULLY)
        else pure $ toast (getString FAVOURITE_ADDED_SUCCESSFULLY)

      _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      if state.props.fromHome then do
        (GlobalState globalState) <- getState
        (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
        case savedLocationResp of
          Right (SavedLocationsListRes listResp) -> do
            let updatedLocationList = getUpdatedLocationList globalState.homeScreen.data.locationList state.data.selectedItem.placeId
            modifyScreenState $ HomeScreenStateType (\homeScreen ->
                                                        homeScreen
                                                          { data
                                                              { settingSideBar {opened = SettingSideBarController.CLOSED}
                                                              , locationList = updatedLocationList
                                                              , savedLocations = (AddNewAddress.getSavedLocations listResp.list)
                                                              }
                                                            } )
            homeScreenFlow
          Left (err) -> homeScreenFlow
        else savedLocationFlow

    UPDATE_LOCATION_NAME_ADDRESS state lat lon -> do
      (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat lon (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                          "HI_IN" -> "HINDI"
                                                                                                                          "KN_IN" -> "KANNADA"
                                                                                                                          _      -> "ENGLISH"))
      let (PlaceName address) = (fromMaybe HomeScreenData.dummyLocationName (locationName !! 0))
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data  { locSelectedFromMap = address.formattedAddress
                                                                                                            , latSelectedFromMap = lat
                                                                                                            , lonSelectedFromMap = lon
                                                                                                            }
                                                                                                    } )
      addNewAddressScreenFlow ""

    GO_TO_FAVOURITES -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      savedLocationFlow

    CHECK_LOCATION_SERVICEABILITY state locItemType-> do
      let item  = state.data.selectedItem
      if item.locationItemType /= Just RECENTS then do
        (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (item.placeId) (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) item
        let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
        let (LatLong placeLatLong) = (placeName.location)
        (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq placeLatLong.lat placeLatLong.lon)
        case (serviceabilityRes.serviceable) , (state.props.editLocation) of
          true , isEditLocation ->  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if isEditLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case isEditLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = isEditLocation
                  }
                } )
          _    ,  _     -> do
            _ <- lift $ lift $ liftFlow (setText' (getNewIDWithTag "SavedLocationEditText") item.description )
            _ <- pure $ hideKeyboardOnNavigation true
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { isSearchedLocationServiceable = false
                  , isLocateOnMap = false
                  , showSavePlaceView = false
                  }
                , data
                  { recentSearchs { predictionArray = state.data.recentSearchs.predictionArray }
                  , address = item.description
                  }
                } )
            addNewAddressScreenFlow ""
        updateDistanceInfo state (Just placeLatLong.lat) (Just placeLatLong.lon)
      else do
        let recentItem = (fromMaybe dummyLocationListItemState ( (filter (\ ( recent) -> (recent.placeId) == (item.placeId))(state.data.recentSearchs.predictionArray)) !! 0))
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if state.props.editLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case state.props.editLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = state.props.editLocation
                  }
                } )
        updateDistanceInfo state recentItem.lat recentItem.lon
    GO_TO_HOME_SCREEN_FLOW -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow

updateDistanceInfo :: AddNewAddressScreenState ->Maybe Number ->Maybe Number -> FlowBT String Unit
updateDistanceInfo state lat lon = do
    distanceInfo <- getDistanceInfo state.data.savedLocations  (if state.props.editLocation then state.data.placeName else "") (fromMaybe 0.0 lat) (fromMaybe 0.0 lon) (fromMaybe "" state.data.selectedItem.placeId)
    modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
      addNewAddressScreen
        { props
          { tagExists = distanceInfo.tagExists
          , isLocateOnMap = false
          , showSavePlaceView = true
          , isBtnActive = case state.data.activeIndex of
                            Just 2 -> if state.data.addressSavedAs /= "" then true else false
                            Just index -> true
                            Nothing -> false
          }
        , data
          { selectedTag = state.data.selectedTag
          , activeIndex = state.data.activeIndex
          , existsAs = distanceInfo.locExistsAs
          }
        } )
    addNewAddressScreenFlow ""

dummyLocationListItemState :: LocationListItemState
dummyLocationListItemState = dummyLocationListState{locationItemType = Just PREDICTION}

getDistanceInfo :: Array LocationListItemState -> String -> Number -> Number -> String -> FlowBT String {tagExists :: Boolean, locExistsAs :: String }
getDistanceInfo savedLocations excludeLocation lat lon placeId = do
  distArr <- pure $ ((AddNewAddress.calculateDistance savedLocations excludeLocation lat lon))
  rslt <- pure $ ((AddNewAddress.isValidLocation savedLocations excludeLocation placeId))
  let placeIdExists =(fromMaybe {locationName : "" , distanceDiff : 1.0} ((rslt)!!0))
      minDist = ((fromMaybe {locationName : "" , distanceDiff : 1.0} ((distArr)!!0)))
      locExistsAs = case placeIdExists.locationName /= "" , minDist.distanceDiff <= 0.020 of
                      true , _ -> placeIdExists.locationName
                      false    , true -> minDist.locationName
                      _ , _ -> ""
      tagExists = ((length rslt) > 0 || minDist.distanceDiff <= 0.020)
  pure $ { tagExists, locExistsAs }

contactUsScreenFlow :: FlowBT String Unit
contactUsScreenFlow = do
  flow <- contactUsScreen
  case flow of
    GO_TO_HOME_FROM_CONTACT state -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq (Just state.data.email) Nothing state.data.description state.data.subject )
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow

tripDetailsScreenFlow :: Boolean ->  FlowBT String Unit
tripDetailsScreenFlow fromMyRides = do
  (GlobalState state) <- getState
  expiryTime <- pure $ (getExpiryTime state.tripDetailsScreen.data.selectedItem.rideEndTimeUTC isForLostAndFound)
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = fromMyRides, canConnectWithDriver = (expiryTime <= 86400)}}) -- expiryTime < 24hrs or 86400 seconds
  flow <- tripDetailsScreen
  case flow of
    GO_TO_HELPSCREEN -> helpAndSupportScreenFlow
    GO_TO_RIDES -> do
      (GlobalState newState) <- getState
      myRidesScreenFlow newState.myRidesScreen.props.fromNavBar
    ON_SUBMIT state -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just state.data.selectedItem.bookingId) state.data.message state.data.message )
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{issueReported = true}})
      tripDetailsScreenFlow state.props.fromMyRides
    GO_TO_INVOICE updatedState -> do
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen = false},data{totalAmount = updatedState.data.totalAmount, date = updatedState.data.date, tripCharges = updatedState.data.totalAmount, selectedItem = updatedState.data.selectedItem}})
      invoiceScreenFlow
    GO_TO_HOME -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    CONNECT_WITH_DRIVER updatedState -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      resp <- Remote.callDriverBT updatedState.data.selectedItem.rideId
      void $ lift $ lift $ toggleLoader false
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  (Just "nammayatri.support@juspay.in") (Just updatedState.data.selectedItem.rideId) "LOSTANDFOUND" "LOST AND FOUND" )
      tripDetailsScreenFlow updatedState.props.fromMyRides

isForLostAndFound :: Boolean
isForLostAndFound = true

myRidesScreenFlow :: Boolean ->  FlowBT String Unit
myRidesScreenFlow fromNavBar = do
  modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen {props{fromNavBar = fromNavBar}})
  flow <- myRidesScreen
  case flow of
    REFRESH state -> myRidesScreenFlow state.props.fromNavBar
    TRIP_DETAILS state -> tripDetailsScreenFlow true
    LOADER_OUTPUT state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state{data{offsetValue = state.data.offsetValue + 8}})
      myRidesScreenFlow state.props.fromNavBar
    BOOK_RIDE -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    GO_TO_NAV_BAR -> homeScreenFlow
    GO_TO_HELP_SCREEN -> helpAndSupportScreenFlow
    REPEAT_RIDE_FLOW state -> do
      updateRideDetails state
      rideSearchFlow "REPEAT_RIDE_FLOW"