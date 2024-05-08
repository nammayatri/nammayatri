{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.App where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Control.Transformers.Back.Trans (BackT)
import Foreign (Foreign)
import Foreign.Object (Object(..), empty)
import LoaderOverlay.ScreenData as LoaderScreenScreenData
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AddNewAddressScreen.ScreenData as AddNewAddressScreenData
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.ContactUsScreen.ScreenData as ContactUsScreenData
import Screens.CustomerUtils.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.InvoiceScreen.ScreenData as InvoiceScreenData
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import LoaderOverlay.ScreenData as LoaderScreenScreenData
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.MyRidesScreen.ScreenData as MyRideScreenData
import Screens.NammaSafetyFlow.ScreenData as NammaSafetyScreenData
import Screens.OnBoardingFlow.PermissionScreen.ScreenData as PermissionScreenData
import Screens.OnBoardingFlow.WelcomeScreen.ScreenData as WelcomeScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.SavedLocationScreen.ScreenData as SavedLocationScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.OnBoardingFlow.PermissionScreen.ScreenData as PermissionScreenData
import Screens.CustomerUtils.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.OnBoardingFlow.WelcomeScreen.ScreenData as WelcomeScreenData
import Screens.TicketBookingFlow.TicketBooking.ScreenData as TicketBookingScreenData
import Screens.TicketInfoScreen.ScreenData as TicketInfoScreenData
import Screens.TicketBookingFlow.PlaceList.ScreenData as TicketingScreenData
import Screens.TicketBookingFlow.MetroTicketBooking.ScreenData as MetroTicketBookingScreenData
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Screens.Types (AboutUsScreenState, AccountSetUpScreenState, AddNewAddressScreenState, AppUpdatePopUpState, ChooseLanguageScreenState, ContactUsScreenState, EnterMobileNumberScreenState, HomeScreenState, InvoiceScreenState, LocItemType, LocationListItemState, MyProfileScreenState, MyRidesScreenState, PermissionScreenState, SavedLocationScreenState, SelectLanguageScreenState, SplashScreenState, TripDetailsScreenState, ReferralScreenState, EmergencyContactsScreenState, CallType, WelcomeScreenState, PermissionScreenStage, TicketBookingScreenState, TicketInfoScreenState, Trip(..), TicketingScreenState, RideScheduledScreenState, SearchLocationScreenState, GlobalProps, NammaSafetyScreenState, FollowRideScreenState, MetroTicketStatusScreenState, MetroTicketDetailsScreenState, MetroTicketBookingScreenState, MetroMyTicketsScreenState, LocationActionId, GlobalFlowCache, ReferralType, RentalScreenState, CancelSearchType) 
import Screens.FollowRideScreen.ScreenData as FollowRideScreenData
import Screens.AppUpdatePopUp.ScreenData as AppUpdatePopUpScreenData
import Foreign.Object ( Object(..), empty)
import Services.API (BookingStatus(..))
import Foreign (Foreign)
import MerchantConfig.Types (AppConfig)
import Data.Maybe (Maybe(..))
import Screens.RentalBookingFlow.RideScheduledScreen.ScreenData as RideScheduledScreenData
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Common.Types.App (CategoryListType)
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as MetroTicketDetailsScreenData
import Screens.TicketBookingFlow.MetroMyTickets.ScreenData as MetroMyTicketsScreenData
import Screens.TicketBookingFlow.TicketStatus.ScreenData as TicketStatusScreenData
import Screens.TicketBookingFlow.MetroTicketStatus.ScreenData as MetroTicketStatusScreenData
import Services.API
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData 
import Screens.RentalBookingFlow.RentalScreen.ScreenData as RentalScreenData

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
  , ticketBookingScreen :: TicketBookingScreenState
  , ticketInfoScreen :: TicketInfoScreenState
  , searchLocationScreen :: SearchLocationScreenState
  , globalProps :: GlobalProps
  , followRideScreen :: FollowRideScreenState
  , appConfig :: Maybe AppConfig
  , rideScheduledScreen :: RideScheduledScreenState
  , rideSelectionScreen :: RideSelectionScreenData.RideSelectionScreenState
  , reportIssueChatScreen :: ReportIssueChatScreenData.ReportIssueChatScreenState
  , nammaSafetyScreen :: NammaSafetyScreenState
  , metroTicketDetailsScreen :: MetroTicketDetailsScreenState
  , metroMyTicketsScreen :: MetroMyTicketsScreenState
  , metroTicketBookingScreen :: MetroTicketBookingScreenState
  , metroTicketStatusScreen :: MetroTicketStatusScreenState
  , globalFlowCache :: GlobalFlowCache
  , rentalScreen :: RentalScreenState
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
  , ticketBookingScreen : TicketBookingScreenData.initData
  , ticketInfoScreen : TicketInfoScreenData.initData
  , followRideScreen : FollowRideScreenData.initData
  , appConfig : Nothing
  , rideScheduledScreen : RideScheduledScreenData.initData
  , rideSelectionScreen : RideSelectionScreenData.initData
  , reportIssueChatScreen : ReportIssueChatScreenData.initData
  , nammaSafetyScreen : NammaSafetyScreenData.initData
  , metroTicketDetailsScreen : MetroTicketDetailsScreenData.initData
  , metroMyTicketsScreen : MetroMyTicketsScreenData.initData
  , searchLocationScreen : SearchLocationScreenData.initData
  , globalProps : defaultGlobalProps
  , metroTicketBookingScreen : MetroTicketBookingScreenData.initData
  , metroTicketStatusScreen : MetroTicketStatusScreenData.initData
  , globalFlowCache : defaultGlobalFlowCache 
  , rentalScreen : RentalScreenData.initData
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
}

data ACCOUNT_SET_UP_SCREEN_OUTPUT = GO_HOME AccountSetUpScreenState | GO_BACK

data TRIP_DETAILS_SCREEN_OUTPUT = ON_SUBMIT TripDetailsScreenState | GO_TO_INVOICE TripDetailsScreenState | GO_TO_HOME TripDetailsScreenState | GO_TO_RIDES | GO_TO_HELPSCREEN | CONNECT_WITH_DRIVER TripDetailsScreenState | GET_CATEGORIES_LIST TripDetailsScreenState | GO_TO_ISSUE_CHAT_SCREEN TripDetailsScreenState CategoryListType

data CONTACT_US_SCREEN_OUTPUT = GO_TO_HOME_FROM_CONTACT ContactUsScreenState

data MY_RIDES_SCREEN_OUTPUT = REFRESH MyRidesScreenState | TRIP_DETAILS MyRidesScreenState | LOADER_OUTPUT MyRidesScreenState | BOOK_RIDE | GO_TO_HELP_SCREEN | GO_TO_NAV_BAR | REPEAT_RIDE_FLOW MyRidesScreenState | GO_TO_RIDE_SCHEDULED_SCREEN MyRidesScreenState | MY_RIDES_GO_TO_HOME_SCREEN MyRidesScreenState



data ABOUT_US_SCREEN_OUTPUT = GO_TO_HOME_FROM_ABOUT

data EMERGECY_CONTACTS_SCREEN_OUTPUT = POST_CONTACTS EmergencyContactsScreenState Boolean
                                      | GET_CONTACTS EmergencyContactsScreenState
                                      | REFRESH_EMERGECY_CONTACTS_SCREEN EmergencyContactsScreenState

data TICKET_INFO_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_TICKET_INFO

data HOME_SCREEN_OUTPUT = LOGOUT
                        | RELOAD Boolean
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
                        | HOME_SCREEN
                        | GET_QUOTES HomeScreenState
                        | SELECT_ESTIMATE HomeScreenState
                        | GET_SELECT_LIST HomeScreenState
                        | CONFIRM_RIDE HomeScreenState
                        | ONGOING_RIDE HomeScreenState
                        | CANCEL_RIDE_REQUEST HomeScreenState CancelSearchType
                        | FCM_NOTIFICATION String HomeScreenState
                        | SEARCH_LOCATION String HomeScreenState
                        | UPDATE_LOCATION_NAME HomeScreenState Number Number
                        | UPDATE_PICKUP_NAME HomeScreenState Number Number
                        | GET_LOCATION_NAME HomeScreenState
                        | GO_TO_FAVOURITES_
                        | SUBMIT_RATING HomeScreenState
                        | GO_TO_FIND_ESTIMATES HomeScreenState
                        | OPEN_GOOGLE_MAPS HomeScreenState
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
                        | RETRY_FINDING_QUOTES Boolean
                        | ON_CALL HomeScreenState CallType String
                        | TRIGGER_PERMISSION_FLOW PermissionScreenStage
                        | REPORT_ISSUE HomeScreenState
                        | RIDE_DETAILS_SCREEN HomeScreenState
                        | GO_TO_TICKET_BOOKING_FLOW HomeScreenState
                        | REPEAT_RIDE_FLOW_HOME Trip
                        | EXIT_TO_TICKETING HomeScreenState
                        | GO_TO_HELP_AND_SUPPORT 
                        | REALLOCATE_RIDE HomeScreenState
                        | GO_TO_SCHEDULED_RIDES
                        | ADD_STOP HomeScreenState
                        | SAFETY_SUPPORT HomeScreenState Boolean
                        | GO_TO_SHARE_RIDE HomeScreenState
                        | GO_TO_NOTIFY_RIDE_SHARE HomeScreenState
                        | EXIT_TO_FOLLOW_RIDE
                        | GO_TO_REPORT_SAFETY_ISSUE HomeScreenState
                        | GO_TO_MY_METRO_TICKETS
                        | GO_TO_METRO_BOOKING HomeScreenState
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
                        | STAY_IN_HOME_SCREEN

data SELECT_LANGUAGE_SCREEN_OUTPUT = GO_TO_HOME_SCREEN | UPDATE_LANGUAGE SelectLanguageScreenState

data PERMISSION_SCREEN_OUTPUT = REFRESH_INTERNET | TURN_ON_GPS | TURN_ON_INTERNET

data SAVED_LOCATION_SCREEN_OUTPUT = ADD_NEW_LOCATION SavedLocationScreenState | DELETE_LOCATION String | EDIT_LOCATION LocationListItemState | GO_BACK_FROM_SAVED_LOCATION

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

data APP_UPDATE_POPUP = Later | UpdateNow

data TICKET_BOOKING_SCREEN_OUTPUT =  GET_BOOKING_INFO_SCREEN TicketBookingScreenState BookingStatus
                                    | GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING TicketBookingScreenState
                                    | GO_TO_TICKET_PAYMENT TicketBookingScreenState
                                    | RESET_SCREEN_STATE
                                    | GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW Number Number
                                    | REFRESH_PAYMENT_STATUS TicketBookingScreenState
                                    | GO_TO_TICKET_LIST TicketBookingScreenState

data METRO_TICKET_STATUS_SCREEN_OUTPUT = GO_TO_METRO_TICKET_DETAILS MetroTicketStatusScreenState MetroTicketBookingStatus
                                       | REFRESH_STATUS_AC MetroTicketStatusScreenState
                                       | GO_TO_TRY_AGAIN_PAYMENT MetroTicketStatusScreenState
                                       | GO_TO_HOME_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN 
                                       | GO_TO_METRO_TICKETS_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN



data TICKETING_SCREEN_SCREEN_OUTPUT = EXIT_TO_HOME TicketingScreenState
                                    | EXIT_TO_MY_TICKETS TicketingScreenState
                                    | BOOK_TICKETS TicketingScreenState
data FOLLOW_RIDE_SCREEN_OUTPUT 
  = RESTART_TRACKING
  | GO_TO_HS_FROM_FOLLOW_RIDE
  | OPEN_GOOGLE_MAPS_FOLLOW_RIDE FollowRideScreenState

data METRO_TICKET_DETAILS_SCREEN_OUTPUT = METRO_TICKET_DETAILS_SCREEN_OUTPUT_NO_OUTPUT 
                                        | BACK_TO_SEARCH_METRO_LOCATION
                                        | GO_BACK_TO_HOME_SCREEN
                                        | GO_TO_MY_METRO_TICKETS_FLOW
                                        | SOFT_CANCEL_BOOKING MetroTicketDetailsScreenState
                                        | HARD_CANCEL_BOOKING MetroTicketDetailsScreenState

data METRO_MY_TICKETS_SCREEN_OUTPUT = METRO_MY_TICKETS_SCREEN_OUTPUT_NO_OUTPUT
                                    | GO_TO_METRO_TICKET_DETAILS_FLOW String
                                    | GO_TO_METRO_TICKET_STAUS_FLOW MetroTicketBookingStatus 
                                    | GO_HOME_FROM_MEtRO_MY_TICKETS_SCREEN 
                                    | GO_HOME_FROM_METRO_MY_TICKETS
                                    | GO_METRO_BOOKING_FROM_METRO_MY_TICKETS

data METRO_TICKET_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_METRO_TICKET MetroTicketBookingScreenState
                                 | GO_TO_METRO_STATION_SEARCH LocationActionId
                                 | METRO_FARE_AND_PAYMENT MetroTicketBookingScreenState
                                 | GO_TO_MY_METRO_TICKET_SCREEN
                                 | GO_TO_METRO_ROUTE_MAP
                                 | GO_TO_HOME_FROM_METRO_TICKET
                                 | REFRESH_METRO_TICKET_SCREEN MetroTicketBookingScreenState
                                 | GO_TO_METRO_PAYMENT_PAGE CreateOrderRes String
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
  | SearchLocationScreenStateType (SearchLocationScreenState -> SearchLocationScreenState)
  | NammaSafetyScreenStateType (NammaSafetyScreenState -> NammaSafetyScreenState)
  | FollowRideScreenStateType (FollowRideScreenState -> FollowRideScreenState)
  | MetroTicketDetailsScreenStateType (MetroTicketDetailsScreenState -> MetroTicketDetailsScreenState)
  | MetroMyTicketsScreenStateType (MetroMyTicketsScreenState -> MetroMyTicketsScreenState)
  | MetroTicketBookingScreenStateType (MetroTicketBookingScreenState -> MetroTicketBookingScreenState)
  | MetroTicketStatusScreenStateType (MetroTicketStatusScreenState -> MetroTicketStatusScreenState)
  | GlobalFlowCacheType (GlobalFlowCache -> GlobalFlowCache)
  | RentalScreenStateType (RentalScreenState -> RentalScreenState)
