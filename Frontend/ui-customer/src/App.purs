{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.App where

import Services.API

import Common.Types.App (CategoryListType, CustomerIssueTypes(..))
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Control.Transformers.Back.Trans (BackT)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Object (Object(..), empty)
import Language.Types (STR(..))
import LoaderOverlay.ScreenData as LoaderScreenScreenData
import Toast.ScreenData as ToastScreenData
import Screens.SelectContactsFlow.SelectContactsScreen.ScreenData (SelectContactsScreenState)
import MerchantConfig.Types (AppConfig)
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AddNewAddressScreen.ScreenData as AddNewAddressScreenData
import Screens.AppUpdatePopUp.ScreenData as AppUpdatePopUpScreenData
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.ContactUsScreen.ScreenData as ContactUsScreenData
import Screens.CustomerUtils.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.FollowRideScreen.ScreenData as FollowRideScreenData
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.InvoiceScreen.ScreenData as InvoiceScreenData
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.MyRidesScreen.ScreenData as MyRideScreenData
import Screens.NammaSafetyFlow.ScreenData as NammaSafetyScreenData
import Screens.DataExplainWithFetch.ScreenData as DataExplainWithFetchScreenData
import Screens.OnBoardingFlow.PermissionScreen.ScreenData as PermissionScreenData
import Screens.OnBoardingFlow.WelcomeScreen.ScreenData as WelcomeScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.SavedLocationScreen.ScreenData as SavedLocationScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.TicketBookingFlow.TicketBooking.ScreenData as TicketBookingScreenData
import Screens.TicketInfoScreen.ScreenData as TicketInfoScreenData
import Screens.TicketBookingFlow.PlaceList.ScreenData as TicketingScreenData
import Screens.TicketBookingFlow.MetroTicketBooking.ScreenData as MetroTicketBookingScreenData
import Screens.Types (AboutUsScreenState, AccountSetUpScreenState, AddNewAddressScreenState, AppUpdatePopUpState, ChooseLanguageScreenState, ContactUsScreenState, EnterMobileNumberScreenState, HomeScreenState, InvoiceScreenState, LocItemType, LocationListItemState, MyProfileScreenState, MyRidesScreenState, PermissionScreenState, SavedLocationScreenState, SelectLanguageScreenState, SplashScreenState, TripDetailsScreenState, ReferralScreenState, EmergencyContactsScreenState, CallType, WelcomeScreenState, PermissionScreenStage, TicketBookingScreenState, TicketInfoScreenState, Trip(..), TicketingScreenState, RideScheduledScreenState, SearchLocationScreenState, GlobalProps, NammaSafetyScreenState, FollowRideScreenState, MetroTicketStatusScreenState, MetroTicketDetailsScreenState, MetroTicketBookingScreenState, MetroMyTicketsScreenState, LocationActionId, GlobalFlowCache, ReferralType, RentalScreenState, CancelSearchType, PickupInstructionsScreenState, RiderRideCompletedScreenState, DataFetchScreenState, SelectFaqScreenState, FaqScreenState, FavouriteDriverTripsState,NotificationBody, ParcelDeliveryScreenState, AadhaarVerificationScreenState)
import Screens.FollowRideScreen.ScreenData as FollowRideScreenData
import Screens.AppUpdatePopUp.ScreenData as AppUpdatePopUpScreenData
import Screens.SelectContactsFlow.SelectContactsScreen.ScreenData as SelectContactsScreenData
import Foreign.Object ( Object(..), empty)
import Services.API (BookingStatus(..))
import Foreign (Foreign)
import MerchantConfig.Types (AppConfig)
import Data.Maybe (Maybe(..))
import Screens.RentalBookingFlow.RideScheduledScreen.ScreenData as RideScheduledScreenData
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Common.Types.App as CTA
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as MetroTicketDetailsScreenData
import Screens.TicketBookingFlow.MetroMyTickets.ScreenData as MetroMyTicketsScreenData
import Screens.TicketBookingFlow.TicketStatus.ScreenData as TicketStatusScreenData
import Screens.TicketBookingFlow.MetroTicketStatus.ScreenData as MetroTicketStatusScreenData
import Services.API
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData as ParcelDeliveryScreenData
import Screens.RentalBookingFlow.RentalScreen.ScreenData as RentalScreenData
import Screens.RideBookingFlow.PickupInstructionsScreen.ScreenData as PickupInstructionsScreenData
import Screens.DriverProfileScreenCommon.ScreenData as DriverProfileScreenCommonState
import Screens.RideBookingFlow.RiderRideCompletedCard.ScreenData as RiderRideCompletedScreenData
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.SelectFaqScreen.ScreenData as SelectFaqScreenData
import Screens.FaqScreen.ScreenData as FaqScreenData
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Screens.SavedLocationScreen.ScreenData as SavedLocationScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.TicketBookingFlow.MetroMyTickets.ScreenData as MetroMyTicketsScreenData
import Screens.TicketBookingFlow.MetroTicketBooking.ScreenData as MetroTicketBookingScreenData
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as MetroTicketDetailsScreenData
import Screens.TicketBookingFlow.PlaceList.ScreenData as TicketingScreenData
import Screens.TicketBookingFlow.TicketBooking.ScreenData as TicketBookingScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.Types (AboutUsScreenState, AccountSetUpScreenState, AddNewAddressScreenState, AppUpdatePopUpState, ChooseLanguageScreenState, ContactUsScreenState, EnterMobileNumberScreenState, HomeScreenState, InvoiceScreenState, LocItemType, LocationListItemState, MyProfileScreenState, MyRidesScreenState, PermissionScreenState, SavedLocationScreenState, SelectLanguageScreenState, SplashScreenState, TripDetailsScreenState, ReferralScreenState, EmergencyContactsScreenState, CallType, WelcomeScreenState, PermissionScreenStage, TicketBookingScreenState, TicketInfoScreenState, Trip(..), TicketingScreenState, RideScheduledScreenState, SearchLocationScreenState, GlobalProps, NammaSafetyScreenState, FollowRideScreenState, MetroTicketStatusScreenState, MetroTicketDetailsScreenState, MetroTicketBookingScreenState, MetroMyTicketsScreenState, LocationActionId, GlobalFlowCache, ReferralType, RentalScreenState, CancelSearchType, BusTicketBookingState)
import Services.API (BookingStatus(..))
import Screens.DriverProfileScreenCommon.ScreenData (DriverProfileScreenCommonState(..))
import Screens.CustomerUtils.FavouriteDriverTrips.ScreenData as FavouriteDriverTripsData
import Screens.RideSummaryScreen.ScreenData as RideSummaryScreenData
import Screens.TicketBookingFlow.BusTicketBooking.ScreenData as BusTicketBookingScreenData
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData as BusTrackingScreenData
import Screens.Types as ST
import Screens.AadhaarVerificationScreen.ScreenData as EnterAadhaarNumberScreenData
import Screens.SelectBusRoute.ScreenData as SelectBusRouteScreenData

type FlowBT e a = BackT (ExceptT e (Free (FlowWrapper GlobalState))) a

newtype GlobalState = GlobalState {
    splashScreen :: SplashScreenState ,
    ticketingScreen :: TicketingScreenState
  , enterMobileNumberScreen :: EnterMobileNumberScreenState
  , chooseLanguageScreen :: ChooseLanguageScreenState
  , accountSetUpScreen :: AccountSetUpScreenState
  , tripDetailsScreen :: TripDetailsScreenState
  , invoiceScreen :: InvoiceScreenState
  , contactUsScreen :: ContactUsScreenState
  , helpAndSupportScreen :: HelpAndSupportScreenData.HelpAndSupportScreenState
  , myRidesScreen :: MyRidesScreenState
  , permissionScreen :: PermissionScreenState
  , homeScreen :: HomeScreenState
  , selectLanguageScreen :: SelectLanguageScreenState
  , aboutUsScreen :: AboutUsScreenState
  , myProfileScreen :: MyProfileScreenState
  , savedLocationScreen :: SavedLocationScreenState
  , addNewAddressScreen :: AddNewAddressScreenState
  , appUpdatePopUpScreen :: AppUpdatePopUpState
  , referralScreen :: ReferralScreenState
  , emergencyContactsScreen :: EmergencyContactsScreenState
  , welcomeScreen :: WelcomeScreenState
  , loaderOverlay :: LoaderScreenScreenData.LoaderOverlayState
  , toast :: ToastScreenData.ToastState
  , ticketBookingScreen :: TicketBookingScreenState
  , ticketInfoScreen :: TicketInfoScreenState
  , searchLocationScreen :: SearchLocationScreenState
  , globalProps :: GlobalProps
  , followRideScreen :: FollowRideScreenState
  , appConfig :: Maybe AppConfig
  , rideScheduledScreen :: RideScheduledScreenState
  , rideSelectionScreen :: RideSelectionScreenData.RideSelectionScreenState
  , reportIssueChatScreen :: ReportIssueChatScreenData.ReportIssueChatScreenState
  , selectFaqScreen :: SelectFaqScreenState
  , faqScreen :: FaqScreenState
  , nammaSafetyScreen :: NammaSafetyScreenState
  , metroTicketDetailsScreen :: MetroTicketDetailsScreenState
  , metroMyTicketsScreen :: MetroMyTicketsScreenState
  , metroTicketBookingScreen :: MetroTicketBookingScreenState
  , metroTicketStatusScreen :: MetroTicketStatusScreenState
  , globalFlowCache :: GlobalFlowCache
  , rentalScreen :: RentalScreenState
  , pickupInstructionsScreen :: PickupInstructionsScreenState
  , riderDriverProfileScreen :: DriverProfileScreenCommonState
  , riderRideCompletedScreen :: RiderRideCompletedScreenState
  , dataExplainWithFetch :: DataFetchScreenState
  , favouriteDriverListScreen :: FavouriteDriverTripsState
  , parcelDeliveryScreen :: ParcelDeliveryScreenState
  , rideSummaryScreen :: RideSummaryScreenData.RideSummaryScreenState
  , selectContactsScreen :: SelectContactsScreenState
  , busTicketBookingScreen :: BusTicketBookingState
  , busTrackingScreen :: ST.BusTrackingScreenState
  , aadhaarVerificationScreen :: AadhaarVerificationScreenState
  , selectBusRouteScreen :: SelectBusRouteScreenData.SelectBusRouteScreenState
  }

defaultGlobalState :: GlobalState
defaultGlobalState = GlobalState {
  ticketingScreen : TicketingScreenData.initData ,
    splashScreen : {data : {message : "pass"}}
  , enterMobileNumberScreen : EnterMobileNumberScreenData.initData
  , chooseLanguageScreen : ChooseLanguageScreenData.initData
  , accountSetUpScreen : AccountSetUpScreenData.initData
  , tripDetailsScreen : TripDetailsScreenData.initData
  , invoiceScreen : InvoiceScreenData.initData
  , contactUsScreen : ContactUsScreenData.initData
  , helpAndSupportScreen : HelpAndSupportScreenData.initData
  , myRidesScreen : MyRideScreenData.initData
  , homeScreen : HomeScreenData.initData
  , selectLanguageScreen : SelectLanguageScreenData.initData
  , permissionScreen : PermissionScreenData.initData
  , aboutUsScreen : AboutUsScreenData.initData
  , myProfileScreen : MyProfileScreenData.initData
  , savedLocationScreen : SavedLocationScreenData.initData
  , addNewAddressScreen : AddNewAddressScreenData.initData
  , appUpdatePopUpScreen : AppUpdatePopUpScreenData.initData
  , referralScreen : ReferralScreenData.initData
  , emergencyContactsScreen : EmergencyContactsScreenData.initData
  , welcomeScreen : WelcomeScreenData.initData
  , loaderOverlay : LoaderScreenScreenData.initData
  , toast : ToastScreenData.initData
  , ticketBookingScreen : TicketBookingScreenData.initData
  , ticketInfoScreen : TicketInfoScreenData.initData
  , followRideScreen : FollowRideScreenData.initData
  , appConfig : Nothing
  , rideScheduledScreen : RideScheduledScreenData.initData
  , rideSelectionScreen : RideSelectionScreenData.initData
  , reportIssueChatScreen : ReportIssueChatScreenData.initData
  , selectFaqScreen : SelectFaqScreenData.initData
  , faqScreen : FaqScreenData.initData
  , nammaSafetyScreen : NammaSafetyScreenData.initData
  , dataExplainWithFetch : DataExplainWithFetchScreenData.initData
  , metroTicketDetailsScreen : MetroTicketDetailsScreenData.initData
  , metroMyTicketsScreen : MetroMyTicketsScreenData.initData
  , searchLocationScreen : SearchLocationScreenData.initData
  , globalProps : defaultGlobalProps
  , metroTicketBookingScreen : MetroTicketBookingScreenData.initData
  , metroTicketStatusScreen : MetroTicketStatusScreenData.initData
  , globalFlowCache : defaultGlobalFlowCache
  , rentalScreen : RentalScreenData.initData
  , pickupInstructionsScreen : PickupInstructionsScreenData.initData
  , riderDriverProfileScreen : DriverProfileScreenCommonState.initData
  , riderRideCompletedScreen : RiderRideCompletedScreenData.initData
  , favouriteDriverListScreen : FavouriteDriverTripsData.initData
  , parcelDeliveryScreen : ParcelDeliveryScreenData.initData
  , rideSummaryScreen : RideSummaryScreenData.initData
  , selectContactsScreen : SelectContactsScreenData.initData
  , busTicketBookingScreen : BusTicketBookingScreenData.initData
  , busTrackingScreen : BusTrackingScreenData.initData
  , aadhaarVerificationScreen : EnterAadhaarNumberScreenData.initData
  , selectBusRouteScreen : SelectBusRouteScreenData.initData
  }

defaultGlobalProps :: GlobalProps
defaultGlobalProps = {
  savedLocations : [] ,
  recentSearches : [] ,
  cachedSearches : []
}

defaultGlobalFlowCache :: GlobalFlowCache
defaultGlobalFlowCache = {
    savedLocations : Nothing
  , savedScheduledRides : Nothing
}

data ACCOUNT_SET_UP_SCREEN_OUTPUT = GO_HOME AccountSetUpScreenState | GO_BACK | APPLY_REFERRAL String

data TRIP_DETAILS_SCREEN_OUTPUT = GO_TO_INVOICE TripDetailsScreenState | GO_TO_HOME TripDetailsScreenState | GO_TO_RIDES | GO_TO_HELPSCREEN | GO_TO_REPORT_ISSUE_CHAT_SCREEN | CONNECT_WITH_DRIVER TripDetailsScreenState | GET_CATEGORIES_LIST TripDetailsScreenState | GO_TO_ISSUE_CHAT_SCREEN TripDetailsScreenState CTA.CategoryListType | GO_TO_RIDE_COMPLETED_SCREEN

data CONTACT_US_SCREEN_OUTPUT = GO_TO_HOME_FROM_CONTACT ContactUsScreenState

data MY_RIDES_SCREEN_OUTPUT = REFRESH MyRidesScreenState | TRIP_DETAILS MyRidesScreenState | LOADER_OUTPUT MyRidesScreenState | BOOK_RIDE | GO_TO_HELP_SCREEN | GO_TO_NAV_BAR | REPEAT_RIDE_FLOW MyRidesScreenState | GO_TO_RIDE_SCHEDULED_SCREEN MyRidesScreenState | MY_RIDES_GO_TO_HOME_SCREEN MyRidesScreenState | NOTIFICATION_HANDLER String NotificationBody



data ABOUT_US_SCREEN_OUTPUT = GO_TO_HOME_FROM_ABOUT

data EMERGECY_CONTACTS_SCREEN_OUTPUT = POST_CONTACTS EmergencyContactsScreenState Boolean
                                      | GET_CONTACTS EmergencyContactsScreenState
                                      | POST_CONTACTS_SAFETY EmergencyContactsScreenState Boolean
                                      | UPDATE_DEFAULT_CONTACTS EmergencyContactsScreenState
                                      | REFRESH_EMERGECY_CONTACTS_SCREEN EmergencyContactsScreenState
                                      | GO_TO_SELECT_CONTACT EmergencyContactsScreenState

data TICKET_INFO_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_TICKET_INFO

data ParcelAction = GO_TO_PARCEL_INSTRUCTIONS HomeScreenState
                    | GO_TO_DELIVERY_DETAILS HomeScreenState
                    | GET_DELIVERY_IMAGE HomeScreenState

data HOME_SCREEN_OUTPUT = HybridAppExit
                        | LOGOUT
                        | RELOAD Boolean
                        | UPDATE_PICKUP_NAME HomeScreenState Number Number
                        | REFRESH_HOME_SCREEN
                        | RETRY
                        | NO_OUTPUT
                        | GO_TO_HELP
                        | GO_TO_NAMMASAFETY HomeScreenState Boolean Boolean
                        | GO_TO_ABOUT
                        | GO_TO_MY_RIDES Boolean
                        | CHANGE_LANGUAGE
                        | GO_TO_EMERGENCY_CONTACTS
                        | GO_TO_MY_TICKETS
                        | GO_TO_MY_PROFILE Boolean
                        | LOCATION_SELECTED LocationListItemState Boolean
                        | EDIT_LOCATION_SELECTED LocationListItemState Boolean
                        | EDIT_DESTINATION_SOFT HomeScreenState
                        | HOME_SCREEN
                        | GET_QUOTES HomeScreenState
                        | CONFIRM_FARE HomeScreenState
                        | SELECT_ESTIMATE HomeScreenState
                        | GET_SELECT_LIST HomeScreenState
                        | CONFIRM_RIDE HomeScreenState
                        | ONGOING_RIDE HomeScreenState
                        | CANCEL_RIDE_REQUEST HomeScreenState CancelSearchType
                        | FCM_NOTIFICATION String NotificationBody HomeScreenState
                        | SEARCH_LOCATION String HomeScreenState Boolean
                        | UPDATE_LOCATION_NAME HomeScreenState Number Number
                        | GET_LOCATION_NAME HomeScreenState
                        | GO_TO_FAVOURITES_
                        | GO_TO_FIND_ESTIMATES HomeScreenState
                        | OPEN_GOOGLE_MAPS HomeScreenState
                        | RELOAD_FLOW_STATUS
                        | IN_APP_TRACK_STATUS HomeScreenState
                        | UPDATE_SAVED_LOCATION
                        | CHECK_SERVICEABILITY HomeScreenState Number Number
                        | GO_TO_INVOICE_ HomeScreenState
                        | CHECK_FOR_DUPLICATE_SAVED_LOCATION HomeScreenState
                        | SAVE_FAVOURITE HomeScreenState
                        | GO_TO_REFERRAL ReferralType
                        | GO_TO_CALL_EMERGENCY_CONTACT HomeScreenState
                        | GO_TO_CALL_POLICE HomeScreenState
                        | GO_TO_CALL_SUPPORT HomeScreenState
                        | GO_TO_SOS_STATUS HomeScreenState
                        | GO_TO_FETCH_CONTACTS HomeScreenState
                        | ON_RESUME_APP
                        | CHECK_CURRENT_STATUS
                        | CHECK_FLOW_STATUS
                        | RETRY_FINDING_QUOTES Boolean String
                        | ON_CALL HomeScreenState CallType String
                        | TRIGGER_PERMISSION_FLOW PermissionScreenStage
                        | RIDE_DETAILS_SCREEN HomeScreenState
                        | GO_TO_TICKET_BOOKING_FLOW HomeScreenState
                        | REPEAT_RIDE_FLOW_HOME Trip
                        | EXIT_TO_TICKETING HomeScreenState
                        | EDIT_LOCATION_FLOW HomeScreenState
                        | CONFIRM_EDITED_PICKUP HomeScreenState
                        | REALLOCATE_RIDE HomeScreenState
                        | GO_TO_SCHEDULED_RIDES (Maybe String)
                        | ADD_STOP HomeScreenState
                        | SAFETY_SUPPORT HomeScreenState Boolean
                        | GO_TO_SHARE_RIDE HomeScreenState
                        | GO_TO_NOTIFY_RIDE_SHARE HomeScreenState
                        | EXIT_TO_FOLLOW_RIDE
                        | GO_TO_MY_METRO_TICKETS HomeScreenState
                        | GO_TO_METRO_BOOKING HomeScreenState
                        | GO_TO_SEARCH_LOCATION_SCREEN_FOR_ROUTE_SEARCH HomeScreenState LocationActionId
                        | GO_TO_SAFETY_EDUCATION
                        | REPEAT_SEARCH HomeScreenState
                        | CHANGE_VEHICLE_VARIANT HomeScreenState
                        | GOTO_CONFIRMING_LOCATION_STAGE HomeScreenState
                        | UPDATE_REFERRAL_CODE String
                        | GO_TO_SAFETY_SETTING_SCREEN
                        | GO_TO_RIDE_RELATED_ISSUES HomeScreenState
                        | GO_TO_RENTALS_FLOW HomeScreenState
                        | GO_TO_SEARCH_LOCATION_SCREEN HomeScreenState Boolean
                        | GO_TO_RIDE_SEARCH_FLOW
                        | CONFIRM_RENTAL_RIDE
                        | GO_TO_DRIVER_PROFILES HomeScreenState
                        | STAY_IN_HOME_SCREEN
                        | GOTO_PICKUP_INSTRUCTIONS HomeScreenState Number Number String String
                        | EDIT_LOCATION_DEST_SELECTED
                        | EDIT_DEST_BACKPRESSED
                        | EXIT_AND_ENTER_HOME_SCREEN
                        | SELECT_ESTIMATE_AND_QUOTES HomeScreenState
                        | UPDATE_CHAT
                        | GO_TO_TRIP_TYPE_SELECTION HomeScreenState
                        | GO_TO_RIDE_SUMMARY_SCREEN HomeScreenState
                        | PARCEL ParcelAction
                        | GO_TO_BUS_TICKET_BOOKING_SCREEN HomeScreenState

data SELECT_LANGUAGE_SCREEN_OUTPUT = GO_BACK_SCREEN | UPDATE_LANGUAGE SelectLanguageScreenState

data PERMISSION_SCREEN_OUTPUT = REFRESH_INTERNET | TURN_ON_GPS | TURN_ON_INTERNET

data SAVED_LOCATION_SCREEN_OUTPUT = ADD_NEW_LOCATION SavedLocationScreenState | DELETE_LOCATION String | EDIT_LOCATION LocationListItemState | GO_BACK_FROM_SAVED_LOCATION | GOTO_FAVOURITEDRIVERS_LIST SavedLocationScreenState | GO_TO_FAV_DRIVER_PROFILE String

data ADD_NEW_ADDRESS_SCREEN_OUTPUT =  SEARCH_ADDRESS String AddNewAddressScreenState
                                    | ADD_LOCATION AddNewAddressScreenState
                                    | UPDATE_LOCATION_NAME_ADDRESS AddNewAddressScreenState Number Number
                                    | GO_TO_FAVOURITES
                                    | CHECK_LOCATION_SERVICEABILITY AddNewAddressScreenState LocItemType
                                    | GO_TO_HOME_SCREEN_FLOW
                                    | GO_TO_SEARCH_LOC_SCREEN

data MY_PROFILE_SCREEN_OUTPUT = UPDATE_USER_PROFILE MyProfileScreenState | GO_TO_HOME_

data REFERRAL_SCREEN_OUPUT = UPDATE_REFERRAL String | BACK_TO_HOME

data WELCOME_SCREEN_OUTPUT = GoToMobileNumberScreen

data FAVOURITE_DRIVERLIST_SCREEN_OUTPUT = GO_BACK_TO_SAVED_LOCATION FavouriteDriverTripsState | GO_TO_FAVOURITE_DRIVER_PROFILE FavouriteDriverTripsState

data APP_UPDATE_POPUP = Later | UpdateNow

data TICKET_BOOKING_SCREEN_OUTPUT =  GET_BOOKING_INFO_SCREEN TicketBookingScreenState BookingStatus
                                    | GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING TicketBookingScreenState
                                    | GO_TO_TICKET_PAYMENT TicketBookingScreenState
                                    | RESET_SCREEN_STATE
                                    | GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW Number Number
                                    | REFRESH_PAYMENT_STATUS TicketBookingScreenState
                                    | GO_TO_TICKET_LIST TicketBookingScreenState
                                    | GO_TO_TICKET_BOOK TicketBookingScreenState String


data METRO_TICKET_STATUS_SCREEN_OUTPUT = GO_TO_METRO_TICKET_DETAILS MetroTicketStatusScreenState FRFSTicketBookingStatusAPIRes
                                       | REFRESH_STATUS_AC MetroTicketStatusScreenState
                                       | GO_TO_TRY_AGAIN_PAYMENT MetroTicketStatusScreenState
                                       | GO_TO_HOME_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN
                                       | GO_TO_METRO_TICKETS_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN
                                       | GO_TO_BUS_TICKET_BOOKING_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN



data TICKETING_SCREEN_SCREEN_OUTPUT = EXIT_TO_HOME TicketingScreenState
                                    | EXIT_TO_MY_TICKETS TicketingScreenState
                                    | BOOK_TICKETS TicketingScreenState
data FOLLOW_RIDE_SCREEN_OUTPUT
  = RESTART_TRACKING
  | GO_TO_HS_FROM_FOLLOW_RIDE FollowRideScreenState Boolean
  | OPEN_GOOGLE_MAPS_FOLLOW_RIDE FollowRideScreenState
  | GO_TO_DRIVER_PROFILE_FROM_FOLLOWRIDE FollowRideScreenState

data METRO_TICKET_DETAILS_SCREEN_OUTPUT = METRO_TICKET_DETAILS_SCREEN_OUTPUT_NO_OUTPUT
                                        | BACK_TO_SEARCH_METRO_LOCATION
                                        | GO_BACK_TO_HOME_SCREEN
                                        | GO_TO_MY_METRO_TICKETS_FLOW
                                        | SOFT_CANCEL_BOOKING MetroTicketDetailsScreenState
                                        | HARD_CANCEL_BOOKING MetroTicketDetailsScreenState
                                        | GO_TO_BUS_TICKET_BOOKING_SCREEN_FROM_METRO_TICKET_DETAILS_SCREEN
                                        | GO_TO_BUS_TRACKING

data METRO_MY_TICKETS_SCREEN_OUTPUT = METRO_MY_TICKETS_SCREEN_OUTPUT_NO_OUTPUT
                                    | GO_TO_METRO_TICKET_DETAILS_FLOW String
                                    | GO_TO_METRO_TICKET_STAUS_FLOW FRFSTicketBookingStatusAPIRes
                                    | GO_HOME_FROM_MEtRO_MY_TICKETS_SCREEN
                                    | GO_HOME_FROM_METRO_MY_TICKETS
                                    | GO_METRO_BOOKING_FROM_METRO_MY_TICKETS
                                    | GO_BUS_BOOKING_FROM_METRO_MY_TICKETS

data METRO_TICKET_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_METRO_TICKET MetroTicketBookingScreenState
                                 | GO_TO_METRO_STATION_SEARCH LocationActionId MetroTicketBookingScreenState
                                 | METRO_FARE_AND_PAYMENT MetroTicketBookingScreenState
                                 | GO_TO_MY_METRO_TICKET_SCREEN
                                 | GO_TO_METRO_ROUTE_MAP
                                 | GO_TO_HOME_FROM_METRO_TICKET
                                 | REFRESH_METRO_TICKET_SCREEN MetroTicketBookingScreenState
                                 | GO_TO_METRO_PAYMENT_PAGE CreateOrderRes String MetroTicketBookingScreenState
                                 | GO_TO_SEARCH_SCREEN MetroTicketBookingScreenState
                                 | GO_TO_AADHAAR_VERIFICATION_SCREEN MetroTicketBookingScreenState String
                                 | EDIT_TICKET_BOOKING_STOPS MetroTicketBookingScreenState

data PICKUP_INSTRUCTIONS_SCREEN_OP = GO_TO_HOME_SCREEN_FROM_PICKUP_INSTRUCTIONS

data DRIVER_PROFILE_SCREEN = GO_TO_HOME_SCREEN_FROM_DRIVER_PROFILE

data RIDER_RIDECOMPLETED_SCREEN_OP = RIDER_DETAILS_SCREEN RiderRideCompletedScreenState | GO_TO_HELP_AND_SUPPORTS | HOME_SCREENS String | GOTO_NAMMASAFETY RiderRideCompletedScreenState Boolean Boolean | SUBMIT_RATINGS RiderRideCompletedScreenState String | GO_TO_DRIVER_PROFILE RiderRideCompletedScreenState | GO_TO_ISSUE_REPORT_CHAT_SCREEN_WITH_ISSUE RiderRideCompletedScreenState CustomerIssueTypes


data PARCEL_DELIVERY_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_PARCEL_DELIVERY ParcelDeliveryScreenState
                                   | REFRESH_PARCEL_DELIVERY_SCREEN ParcelDeliveryScreenState

data RIDE_SUMMARY_SCREEN_OUTPUT =     GO_TO_RIDE_REQUEST
                                    | ACCEPT_SCHEDULED_RIDE String String
                                    | RIDE_CONFIRMED String String (Maybe String)
                                    | CANCEL_SCHEDULED_RIDE String String
                                    | NOTIFICATION_LISTENER String NotificationBody
                                    | REFRESH_RIDE_SUMMARY_SCREEN (Maybe String)
                                    | CALL_DRIVER RideSummaryScreenData.RideSummaryScreenState CallType String

data SELECT_CONTACT_SCREEN_OUTPUT = SELECT_CONTACTS_BACK_PRESSED
                                  | EXECUTE_CALLBACK SelectContactsScreenState

data SELECT_BUS_ROUTE_SCREEN_OUTPUT = TRACK_BUS SelectBusRouteScreenData.SelectBusRouteScreenState
                                    | GO_TO_SEARCH_LOCATION_FROM_SELECT_ROUTE

data AADHAAR_VERIFICATION_SCREEN_OUTPUT = ENTER_AADHAAR_OTP AadhaarVerificationScreenState
  | VERIFY_AADHAAR_OTP AadhaarVerificationScreenState
  | RESEND_AADHAAR_OTP AadhaarVerificationScreenState
  | SEND_UNVERIFIED_AADHAAR_DATA AadhaarVerificationScreenState
  | GO_TO_HOME_FROM_AADHAAR
  | LOGOUT_FROM_AADHAAR
  | GO_TO_TICKET_BOOKING_FROM_AADHAAR

data ScreenType =
    EnterMobileNumberScreenType (EnterMobileNumberScreenState -> EnterMobileNumberScreenState)
  | HomeScreenStateType (HomeScreenState -> HomeScreenState)
  | ChooseLanguageScreenStateType (ChooseLanguageScreenState -> ChooseLanguageScreenState)
  | TripDetailsScreenStateType (TripDetailsScreenState -> TripDetailsScreenState)
  | MyRideScreenStateType (MyRidesScreenState -> MyRidesScreenState)
  | HelpAndSupportScreenStateType (HelpAndSupportScreenData.HelpAndSupportScreenState -> HelpAndSupportScreenData.HelpAndSupportScreenState)
  | InvoiceScreenStateType (InvoiceScreenState -> InvoiceScreenState)
  | SelectLanguageScreenStateType (SelectLanguageScreenState -> SelectLanguageScreenState)
  | AccountSetUpScreenStateType (AccountSetUpScreenState -> AccountSetUpScreenState)
  | AddNewAddressScreenStateType (AddNewAddressScreenState -> AddNewAddressScreenState)
  | MyProfileScreenStateType (MyProfileScreenState -> MyProfileScreenState)
  | ContactUsScreenStateType (ContactUsScreenState -> ContactUsScreenState)
  | SavedLocationScreenStateType (SavedLocationScreenState -> SavedLocationScreenState)
  | ReferralScreenStateType (ReferralScreenState -> ReferralScreenState)
  | EmergencyContactsScreenStateType (EmergencyContactsScreenState -> EmergencyContactsScreenState)
  | TicketBookingScreenStateType (TicketBookingScreenState -> TicketBookingScreenState)
  | TicketInfoScreenStateType (TicketInfoScreenState -> TicketInfoScreenState)
  | PermissionScreenStateType (PermissionScreenState -> PermissionScreenState)
  | AboutUsScreenStateType (AboutUsScreenState -> AboutUsScreenState)
  | AppUpdatePopUpScreenType (AppUpdatePopUpState -> AppUpdatePopUpState)
  | AppConfigType (Maybe AppConfig -> Maybe AppConfig)
  | TicketingScreenStateType (TicketingScreenState -> TicketingScreenState)
  | RideScheduledScreenStateType (RideScheduledScreenState -> RideScheduledScreenState)
  | GlobalPropsType (GlobalProps -> GlobalProps)
  | RideSelectionScreenStateType (RideSelectionScreenData.RideSelectionScreenState -> RideSelectionScreenData.RideSelectionScreenState)
  | ReportIssueChatScreenStateType (ReportIssueChatScreenData.ReportIssueChatScreenState -> ReportIssueChatScreenData.ReportIssueChatScreenState)
  | SelectFaqScreenStateType (SelectFaqScreenState -> SelectFaqScreenState)
  | FaqScreenStateType (FaqScreenState -> FaqScreenState)
  | SearchLocationScreenStateType (SearchLocationScreenState -> SearchLocationScreenState)
  | NammaSafetyScreenStateType (NammaSafetyScreenState -> NammaSafetyScreenState)
  | FollowRideScreenStateType (FollowRideScreenState -> FollowRideScreenState)
  | MetroTicketDetailsScreenStateType (MetroTicketDetailsScreenState -> MetroTicketDetailsScreenState)
  | MetroMyTicketsScreenStateType (MetroMyTicketsScreenState -> MetroMyTicketsScreenState)
  | MetroTicketBookingScreenStateType (MetroTicketBookingScreenState -> MetroTicketBookingScreenState)
  | MetroTicketStatusScreenStateType (MetroTicketStatusScreenState -> MetroTicketStatusScreenState)
  | GlobalFlowCacheType (GlobalFlowCache -> GlobalFlowCache)
  | RentalScreenStateType (RentalScreenState -> RentalScreenState)
  | PickupInstructionsScreenStateType (PickupInstructionsScreenState -> PickupInstructionsScreenState)
  | DriverProfileScreenCommonStateType (DriverProfileScreenCommonState -> DriverProfileScreenCommonState)
  | RiderRideCompletedScreenStateType (RiderRideCompletedScreenState -> RiderRideCompletedScreenState)
  | DataFetchScreenStateType (DataFetchScreenState -> DataFetchScreenState)
  | FavouriteDriverTripsStateType (FavouriteDriverTripsState -> FavouriteDriverTripsState)
  | ParcelDeliveryScreenStateType (ParcelDeliveryScreenState -> ParcelDeliveryScreenState)
  | RideSummaryScreenStateType (RideSummaryScreenData.RideSummaryScreenState -> RideSummaryScreenData.RideSummaryScreenState)
  | SelectContactsScreenStateType (SelectContactsScreenState -> SelectContactsScreenState)
  | BusTicketBookingScreenStateType (BusTicketBookingState -> BusTicketBookingState)
  | BusTrackingScreenStateType (ST.BusTrackingScreenState -> ST.BusTrackingScreenState)
  | AadhaarVerificationScreenType (AadhaarVerificationScreenState -> AadhaarVerificationScreenState)
  | SelectBusRouteScreenType (SelectBusRouteScreenData.SelectBusRouteScreenState -> SelectBusRouteScreenData.SelectBusRouteScreenState)
