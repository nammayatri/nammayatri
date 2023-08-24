{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.App where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Control.Transformers.Back.Trans (BackT)
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AddNewAddressScreen.ScreenData as AddNewAddressScreenData
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.ContactUsScreen.ScreenData as ContactUsScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.InvoiceScreen.ScreenData as InvoiceScreenData
import LoaderOverlay.ScreenData as LoaderScreenScreenData
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.MyRidesScreen.ScreenData as MyRideScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.SavedLocationScreen.ScreenData as SavedLocationScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.OnBoardingFlow.PermissionScreen.ScreenData as PermissionScreenData
import Screens.CustomerUtils.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.OnBoardingFlow.WelcomeScreen.ScreenData as WelcomeScreenData
import Screens.Types (AboutUsScreenState, AccountSetUpScreenState, AddNewAddressScreenState, AppUpdatePopUpState, ChooseLanguageScreenState, ContactUsScreenState, EnterMobileNumberScreenState, HelpAndSupportScreenState, HomeScreenState, InvoiceScreenState, LocItemType, LocationListItemState, MyProfileScreenState, MyRidesScreenState, PermissionScreenState, SavedLocationScreenState, SelectLanguageScreenState, SplashScreenState, TripDetailsScreenState, ReferralScreenState, EmergencyContactsScreenState, CallType, WelcomeScreenState)
import Foreign.Object ( Object(..), empty)
import Foreign (Foreign)

type FlowBT e a = BackT (ExceptT e (Free (FlowWrapper GlobalState))) a

newtype GlobalState = GlobalState {
    splashScreen :: SplashScreenState
  , enterMobileNumberScreen :: EnterMobileNumberScreenState
  , chooseLanguageScreen :: ChooseLanguageScreenState
  , accountSetUpScreen :: AccountSetUpScreenState
  , tripDetailsScreen :: TripDetailsScreenState
  , invoiceScreen :: InvoiceScreenState
  , contactUsScreen :: ContactUsScreenState
  , helpAndSupportScreen :: HelpAndSupportScreenState
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
  }

defaultGlobalState :: GlobalState
defaultGlobalState = GlobalState {
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
  , appUpdatePopUpScreen : {version : 1 , logField : empty}
  , referralScreen : ReferralScreenData.initData
  , emergencyContactsScreen : EmergencyContactsScreenData.initData
  , welcomeScreen : WelcomeScreenData.initData
  , loaderOverlay : LoaderScreenScreenData.initData
  }
data ACCOUNT_SET_UP_SCREEN_OUTPUT = GO_HOME AccountSetUpScreenState | GO_BACK

data TRIP_DETAILS_SCREEN_OUTPUT = ON_SUBMIT TripDetailsScreenState | GO_TO_INVOICE TripDetailsScreenState | GO_TO_HOME TripDetailsScreenState | GO_TO_RIDES | GO_TO_HELPSCREEN | CONNECT_WITH_DRIVER TripDetailsScreenState

data CONTACT_US_SCREEN_OUTPUT = GO_TO_HOME_FROM_CONTACT ContactUsScreenState

data MY_RIDES_SCREEN_OUTPUT = REFRESH MyRidesScreenState | TRIP_DETAILS MyRidesScreenState | LOADER_OUTPUT MyRidesScreenState | BOOK_RIDE | GO_TO_HELP_SCREEN | GO_TO_NAV_BAR | REPEAT_RIDE_FLOW MyRidesScreenState

data HELP_AND_SUPPORT_SCREEN_OUTPUT = GO_TO_SUPPORT_SCREEN String | GO_TO_TRIP_DETAILS HelpAndSupportScreenState | VIEW_RIDES | UPDATE_STATE HelpAndSupportScreenState | GO_TO_HOME_FROM_HELP | DELETE_USER_ACCOUNT HelpAndSupportScreenState

data ABOUT_US_SCREEN_OUTPUT = GO_TO_HOME_FROM_ABOUT

data EMERGECY_CONTACTS_SCREEN_OUTPUT = GO_TO_HOME_FROM_EMERGENCY_CONTACTS
                                      | POST_CONTACTS EmergencyContactsScreenState
                                      | GET_CONTACTS EmergencyContactsScreenState
                                      | REFRESH_EMERGECY_CONTACTS_SCREEN EmergencyContactsScreenState

data HOME_SCREEN_OUTPUT = LOGOUT
                        | RELOAD Boolean
                        | CANCEL
                        | RETRY
                        | NO_OUTPUT
                        | GO_TO_HELP
                        | GO_TO_ABOUT
                        | GO_TO_MY_RIDES
                        | CHANGE_LANGUAGE
                        | GO_TO_EMERGENCY_CONTACTS
                        | GO_TO_MY_PROFILE Boolean
                        | LOCATION_SELECTED LocationListItemState Boolean
                        | HOME_SCREEN
                        | GET_QUOTES HomeScreenState
                        | SELECT_ESTIMATE HomeScreenState
                        | GET_SELECT_LIST HomeScreenState
                        | CONFIRM_RIDE HomeScreenState
                        | ONGOING_RIDE HomeScreenState
                        | CANCEL_RIDE_REQUEST HomeScreenState
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
                        | GO_TO_REFERRAL
                        | GO_TO_CALL_EMERGENCY_CONTACT HomeScreenState
                        | GO_TO_CALL_POLICE HomeScreenState
                        | GO_TO_CALL_SUPPORT HomeScreenState
                        | GO_TO_SOS_STATUS HomeScreenState
                        | GO_TO_FETCH_CONTACTS HomeScreenState
                        | ON_RESUME_APP
                        | CHECK_CURRENT_STATUS
                        | CHECK_FLOW_STATUS
                        | RETRY_FINDING_QUOTES Boolean
                        | ON_CALL HomeScreenState CallType
                        | TRIGGER_PERMISSION_FLOW String
                        | REPORT_ISSUE HomeScreenState
                        | RIDE_DETAILS_SCREEN HomeScreenState
                        | TURN_ON_INTERNET_

data SELECT_LANGUAGE_SCREEN_OUTPUT = GO_TO_HOME_SCREEN | UPDATE_LANGUAGE SelectLanguageScreenState

data PERMISSION_SCREEN_OUTPUT = REFRESH_INTERNET | TURN_ON_GPS | TURN_ON_INTERNET

data SAVED_LOCATION_SCREEN_OUTPUT = ADD_NEW_LOCATION SavedLocationScreenState | DELETE_LOCATION String | EDIT_LOCATION LocationListItemState | GO_BACK_FROM_SAVED_LOCATION

data ADD_NEW_ADDRESS_SCREEN_OUTPUT =  SEARCH_ADDRESS String AddNewAddressScreenState
                                    | ADD_LOCATION AddNewAddressScreenState
                                    | UPDATE_LOCATION_NAME_ADDRESS AddNewAddressScreenState Number Number
                                    | GO_TO_FAVOURITES
                                    | CHECK_LOCATION_SERVICEABILITY AddNewAddressScreenState LocItemType
                                    | GO_TO_HOME_SCREEN_FLOW

data MY_PROFILE_SCREEN_OUTPUT = UPDATE_USER_PROFILE MyProfileScreenState | GO_TO_HOME_

data REFERRAL_SCREEN_OUPUT = UPDATE_REFERRAL String | BACK_TO_HOME

data WELCOME_SCREEN_OUTPUT = GoToMobileNumberScreen

data ScreenType =
    EnterMobileNumberScreenType (EnterMobileNumberScreenState -> EnterMobileNumberScreenState)
  | HomeScreenStateType (HomeScreenState -> HomeScreenState)
  | ChooseLanguageScreenStateType (ChooseLanguageScreenState -> ChooseLanguageScreenState)
  | TripDetailsScreenStateType (TripDetailsScreenState -> TripDetailsScreenState)
  | MyRideScreenStateType (MyRidesScreenState -> MyRidesScreenState)
  | HelpAndSupportScreenStateType (HelpAndSupportScreenState -> HelpAndSupportScreenState)
  | InvoiceScreenStateType (InvoiceScreenState -> InvoiceScreenState)
  | SelectLanguageScreenStateType (SelectLanguageScreenState -> SelectLanguageScreenState)
  | AccountSetUpScreenStateType (AccountSetUpScreenState -> AccountSetUpScreenState)
  | AddNewAddressScreenStateType (AddNewAddressScreenState -> AddNewAddressScreenState)
  | MyProfileScreenStateType (MyProfileScreenState -> MyProfileScreenState)
  | ContactUsScreenStateType (ContactUsScreenState -> ContactUsScreenState)
  | SavedLocationScreenStateType (SavedLocationScreenState -> SavedLocationScreenState)
  | ReferralScreenStateType (ReferralScreenState -> ReferralScreenState)
  | EmergencyContactsScreenStateType (EmergencyContactsScreenState -> EmergencyContactsScreenState)
  | PermissionScreenStateType (PermissionScreenState -> PermissionScreenState)
  | AboutUsScreenStateType (AboutUsScreenState -> AboutUsScreenState)
